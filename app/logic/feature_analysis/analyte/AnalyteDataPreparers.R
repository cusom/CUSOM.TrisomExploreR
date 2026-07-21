box::use(
    R6[R6Class],
    dplyr[select, filter, mutate, case_when, add_count, ungroup, if_else, arrange,
        inner_join, join_by, distinct, everything, rename, rename_with, pull],
    glue[glue],
    forcats[fct_inorder],
    rlang[sym]
)

box::use(
    app/logic/shared/string_utils[parse_delimited_string]
)

PreparerBase <- R6Class(
    "PreparerBase",
    private = list(),
    active = list(
        formatted_analyte_data = function(value) {
            return(
                self$prepared_data
            )
        }
    ),
    public = list(
        source_data = NULL,
        prepared_data = NULL,
        initialize = function(analysis_config, ...) {
            args_list <- list(...)
            self$source_data <- args_list$study_data
        }
    )
)

#' @export
CategoricalSinglePreparer <- R6Class(
    "CategoricalSinglePreparer",
    inherit = PreparerBase,
    public = list(
        analysis_variable_name = NULL,
        initialize = function(analysis_config, ...) {
            super$initialize(analysis_config, ...)
            self$analysis_variable_name <- analysis_config$AnalysisVariableName
        },
        prepare = function(.data) {
            self$prepared_data <- .data |>
                mutate(
                    log2MeasuredValue = if_else(MeasuredValue == 0, 0, log2(MeasuredValue)),
                    log2Measurement   = glue("log<sub>2</sub>({Measurement})"),
                    highlightGroup = NA_character_  # if/when needed
                ) |>
                filter(
                    is.finite(log2MeasuredValue)
                ) |>
                add_count(!!sym(self$analysis_variable_name), name = "n") |>
                mutate(
                    !!sym(self$analysis_variable_name) := paste0("<b>", .data[[self$analysis_variable_name]], "</b> (n=", n, ")"),
                    text      = glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
                ) |>
                select(-n)
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
PrecalculatedCategoricalSinglePreparer <- R6Class(
    "PrecalculatedCategoricalSinglePreparer",
    inherit = CategoricalSinglePreparer,
    public = list(
        prepare = function(.data) {
            self$prepared_data <- .data |>
                mutate(
                    log2MeasuredValue = if_else(MeasuredValue == 0, 0, log2(MeasuredValue)),
                    log2Measurement   = glue("log<sub>2</sub>({Measurement})"),
                    highlightGroup = NA_character_  # if/when needed
                ) |>
                filter(
                    is.finite(log2MeasuredValue)
                ) |>
                add_count(!!sym(self$analysis_variable_name), name = "n") |>
                mutate(
                    !!sym(self$analysis_variable_name) := paste0("<b>", .data[[self$analysis_variable_name]], "</b> (n=", n, ")"),
                    text      = glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
                ) |>
                select(-n)
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
TOFAEndpointsCategoricalPreparer <- R6Class(
    "TOFAEndpointsCategoricalPreparer",
    inherit = PrecalculatedCategoricalSinglePreparer,
    public = list(
        prepare = function(.data) {
            self$prepared_data <- .data |>
                mutate(
                    log2MeasuredValue = MeasuredValue,
                    log2Measurement   = Measurement,
                    highlightGroup = NA_character_  # if/when needed
                ) |>
                filter(
                    is.finite(log2MeasuredValue)
                ) |>
                add_count(!!sym(self$analysis_variable_name), name = "n") |>
                mutate(
                    !!sym(self$analysis_variable_name) := paste0("<b>", .data[[self$analysis_variable_name]], "</b> (n=", n, ")"),
                    text      = glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
                ) |>
                select(-n)
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
ContinuousSinglePreparer <- R6Class(
    "ContinuousSinglePreparer",
    inherit = PreparerBase,
    public = list(
        prepare = function(.data) {
            self$prepared_data <- .data |>
                mutate(
                    log2MeasuredValue = if_else(MeasuredValue == 0, 0, log2(MeasuredValue)),
                    log2Measurement   = glue("log<sub>2</sub>({Measurement})"),
                    highlightGroup = NA_character_  # if/when needed
                ) |>
                filter(
                    is.finite(log2MeasuredValue)
                ) |>
                mutate(
                    text = glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
                )
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
HeatmapPreparer <- R6Class(
    "HeatmapPreparer",
    inherit = PreparerBase,
    public = list(
        prepare = function(.data) {
            self$prepared_data <- .data |>
                select(Analyte, ChangeValue = log2FoldChange, text) |>
                arrange(-ChangeValue) |>
                mutate(
                    Analyte = fct_inorder(Analyte),
                    "ChangeVarName" = "log<sub>2</sub>(Fold Change)",
                    "Analysis" = "T21vD21"
                )
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
CorrelatesPreparer <- R6Class(
    "CorrelatesPreparer",
    inherit = PreparerBase,
    active = list(
        QueryAnalyteLabel = function(value) {
            return(
                self$prepared_data |>
                    distinct(QueryAnalyte) |>
                    pull()
            )
        },
        ComparisonMeasurement = function(value) {
            return(self$prepared_data[1, "Measurement.y"])
        },
        ComparisonAnalyteLabel = function(value) {
            return(self$prepared_data[1, "yLabel"])
        },
        QueryMeasurement = function(value) {
            return(self$prepared_data[1, "Measurement.x"])
        },
        x_label = function(value) {
            return(
                glue("{self$QueryAnalyteLabel} log<sub>2</sub>({self$QueryMeasurement})")
            )
        },
        y_label = function(value) {
            return(
                glue("{self$ComparisonAnalyteLabel} log<sub>2</sub>({self$ComparisonMeasurement})")
            )
        },
        formatted_analyte_data = function(value) {
            return(
                self$prepared_data |>
                    select(-c(Measurement.x, Measurement.y, xLabel, yLabel, x, y)) |>
                    rename_with(~gsub("(?<!^|\\s)([A-Z]+)", " \\1", ., perl = TRUE), everything()) |>
                    rename(`:=`(!!self$x_label, log2x), `:=`(!!self$y_label, log2y))
            )
        }
    ),
    public = list(
        prepare = function(.data) {
            self$prepared_data <- .data |>
                mutate(
                    log2x = log2(x),
                    log2y = log2(y),
                    xLabel = parse_delimited_string(QueryAnalyte, 1),
                    yLabel = parse_delimited_string(ComparisonAnalyte, 1)
                )
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
CorrelatesHeatmapPreparer <- R6Class(
    "CorrelatesHeatmapPreparer",
    inherit = PreparerBase,
    active = list(
        CorrelationMeasureName = function(value) {
            return(
                self$source_data$CorrelationMeasure[1]
            )
        },
        formatted_analyte_data = function(value) {
            return(
                self$prepared_data |>
                    inner_join(self$source_data, by = "Analyte") |>
                    select(QueryExperimentID, QueryAnalyte, ComparisonExperimentID, Analyte, CorrelationValue) |>
                    rename(`:=`(!!self$CorrelationMeasureName, CorrelationValue)) |>
                    rename_with(~gsub("(?<!^|\\s)([A-Z]+)", " \\1", ., perl = TRUE), everything())
            )
        }
    ),
    public = list(
        prepare = function(.data) {
            if (length(.data$Analyte) == 1) {
                return(self$prepare_single(.data))
            } else {
                return(self$prepare_multi(.data))
            }
        },
        prepare_single = function(.data) {
            self$prepared_data <- self$source_data |>
                inner_join(.data, join_by(Analyte == ComparisonAnalyte)) |>
                select(Analyte, CorrelationValue, LabID, QueryAnalyteID, Measurement.x, Measurement.y, x, y) |>
                distinct() |>
                mutate(
                    text = glue(
                        "LabID: {LabID}
                        Query Analyte: {QueryAnalyteID}
                        {Measurement.y}: {x}
                        Comparison Analyte: {Analyte}
                        {Measurement.x}: {y}
                        "
                    )
                ) |>
                select(Analyte, ChangeValue = CorrelationValue, text) |>
                arrange(-ChangeValue) |>
                mutate(
                    Analyte = fct_inorder(Analyte),
                    "ChangeVarName" = !!self$CorrelationMeasureName,
                    "Analysis" = "T21vD21"
                )
            return(invisible(self$prepared_data))
        },
        prepare_multi = function(.data) {
            self$prepared_data <- self$source_data |>
                inner_join(.data |> select(Analyte), by = "Analyte") |>
                select(Analyte, QueryAnalyte, CorrelationValue, CorrelationMeasureName) |>
                distinct() |>
                mutate(
                    text = glue(
                        "Query Analyte: {QueryAnalyte}
                        Comparison Analyte: {Analyte}
                        {CorrelationMeasureName}: {CorrelationValue}
                        "
                    )
                ) |>
                select(Analyte, ChangeValue = CorrelationValue, text) |>
                arrange(-ChangeValue) |>
                mutate(
                    Analyte = fct_inorder(Analyte),
                    "ChangeVarName" = !!self$CorrelationMeasureName,
                    "Analysis" = "T21vD21"
                )
            return(invisible(self$prepared_data))
        }
    )
)
