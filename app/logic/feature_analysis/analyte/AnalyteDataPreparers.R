box::use(
    R6[R6Class],
    dplyr[select, filter, mutate, case_when, add_count, ungroup, if_else, arrange],
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
    active = list(),
    public = list(
        prepared_data = NULL,
        initialize = function(analysis_config, ...) {

        }
    )
)

#' @export
CategoricalSinglePreparer <- R6Class(
    "CategoricalSinglePreparer",
    inherit = PreparerBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, ...) {
            super$initialize(analysis_config, ...)
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
                add_count(Karyotype, name = "n") |>
                mutate(
                    Karyotype = glue("<b>{Karyotype}</b> (n={n})"),
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
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, ...) {
            super$initialize(analysis_config, ...)
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
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, ...) {
            super$initialize(analysis_config, ...)
        },
        prepare = function(.data) {
            self$prepared_data <- .data |>
                select(Analyte, log2FoldChange, text) |>
                arrange(-log2FoldChange) |>
                mutate(Analyte = fct_inorder(Analyte), "Analysis" = "T21vD21")
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
CorrelatesPreparer <- R6Class(
    "CorrelatesPreparer",
    inherit = PreparerBase,
    private = list(),
    active = list(),
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
