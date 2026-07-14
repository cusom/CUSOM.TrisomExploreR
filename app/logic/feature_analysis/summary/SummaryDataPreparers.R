box::use(
    R6[R6Class],
    glue[glue],
    dplyr[select, mutate, mutate_at, group_by, summarise, ungroup, rename_with, rename,
            distinct, n, pull, arrange, dense_rank, row_number, vars, filter, if_else, slice_max],
    forcats[fct_relevel],
    purrr[map2_chr],
    stringr[str_replace],
    rlang[sym]
)

box::use(
    app/logic/shared/statistical_analysis[getStatTestByKeyGroup, getLinearModelWithInteraction,
        formatPValue],
)

SummaryDataPreparerBase <- R6Class(
    "SummaryDataPreparerBase",
    private = list(
        app_config = NULL,
        analysis_config = NULL,
        remote_db = NULL
    ),
    active = list(
        adjusted = function(value) {
            if (missing(value)) {
                return(self$adjustment_method != "none")
            }
        },
        p_val_label = function(value) {
            label <- "p-value"
            if (self$adjusted) {
                label <- str_replace(label, "[/p+-]", "q")
            }
            return(label)
        },
        log_10_p_val_label = function(value) {
            label <- "-log<sub>10</sub>(p-value)"
            if (self$adjusted) {
                label <- str_replace(label, "(?<=\\()p", "q")
            }
            return(label)
        },
        analysis_variable = function(value) {
            return(
                private$analysis_config$AnalysisVariableName
            )
        },
        group_baseline_label = function(value) {
            return(
                private$analysis_config$AnalysisVariableBaselineLabel
            )
        },
        measure_name = function(value) {
            return(
                prepared_data() |>
                    distinct(Measurement) |>
                    pull() |>
                    as.character()
            )
        },
        raw_column_names = function(value) {
            return(
                c(
                    "FoldChange", "p.value.original", "p.value.adjustment.method",
                    "log2FoldChange", "p.value", "-log10pvalue", "lmFormula"
                )
            )
        },
        formatted_column_names = function(value) {
            return(
                c(
                    "Fold Change", "p-value (original)", "Multiple hypothesis correction method",
                    "log<sub>2</sub>(Fold Change)", self$p_val_label, self$log_10_p_val_label, "Model"
                )
            )
        },
        formatted_summary_data = function(value) {
            return(
                self$prepared_data |>
                    rename_with(~ self$formatted_column_names, all_of(self$raw_column_names)) |>
                    select(-c(text, ivs, shape, selectedPoint, formattedPValue))
            )
        }
    ),
    public = list(
        study = NULL,
        study_data = NULL,
        stat_test = NULL,
        covariates = NULL,
        adjustment_method = NULL,
        source_data = NULL,
        summary_data = NULL,
        prepared_data = NULL,
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            private$remote_db <- app_config$remote_db
            self$study <- study
            self$study_data <- study_data
            self$stat_test <- stat_test
            self$covariates <- covariates
            self$adjustment_method <- adjustment_method
        },
        set_source_data = function(source_data) {
            self$source_data <- source_data
            return(invisible(self$source_data))
        },
        set_summary_data = function(source_data) {
            stop("Abstract: must implement")
        },
        prepare = function(source_data) {
            self$prepared_data <- self$set_summary_data(source_data) |>
                mutate(
                    log2FoldChange = log2(FoldChange),
                    `-log10pvalue` = -log10(p.value),
                    `p.value.adjustment.method` = self$adjustment_method,
                    formattedPValue = map2_chr(p.value, `p.value.adjustment.method`, formatPValue),
                    text = glue("Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}")
                )
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
CategoricalSummaryPreparer <- R6Class(
    "CategoricalSummaryPreparer",
    inherit = SummaryDataPreparerBase,
    public = list(
        set_summary_data = function(source_data) {
            self$summary_data <- self$set_source_data(source_data) |>
                select(LabID, Analyte, log2MeasuredValue, self$analysis_variable, self$covariates) |>
                mutate_at(vars(self$analysis_variable), ~fct_relevel(.x, self$group_baseline_label)) |>
                getStatTestByKeyGroup(
                    id = LabID,
                    key = Analyte,
                    response = log2MeasuredValue,
                    independentVariable = !!sym(self$analysis_variable),
                    baselineLabel = self$group_baseline_label,
                    testMethod = self$stat_test,
                    adjustmentMethod = self$adjustment_method,
                    covariates = self$covariates
                ) |>
                mutate(
                    shape = "circle",
                    selectedPoint = 0
                )
            return(invisible(self$summary_data))
        }
    )
)

#' @export
ContinuousSummaryPreparer <- R6Class(
    "ContinuousSummaryPreparer",
    inherit = SummaryDataPreparerBase,
    public = list(
        set_summary_data = function(source_data) {
            self$summary_data <- self$set_source_data(source_data) |>
                select(LabID, Analyte, log2MeasuredValue, self$analysis_variable, self$covariates, Karyotype) |>
                mutate(Karyotype = fct_relevel(Karyotype, "Control")) |>
                getLinearModelWithInteraction(
                    id = LabID,
                    key = Analyte,
                    response = log2MeasuredValue,
                    independentVariable = !!sym(self$analysis_variable),
                    covariates = self$covariates,
                    interactionVariable = Karyotype,
                    adjustmentMethod = self$adjustment_method
                ) |>
                mutate(
                    shape = "circle",
                    selectedPoint = 0
                ) |>
                select(-self$analysis_variable)
            return(invisible(self$summary_data))
        }
    )
)

#' @export
CorrelatesSummaryPreparer <- R6Class(
    "CorrelatesSummaryPreparer",
    inherit = SummaryDataPreparerBase,
    active = list(
        summary_data_max_finite = function(value) {
            return(
                self$source_data |>
                    filter(p.value > 0) |>
                    pull(p.value) |>
                    min() |>
                        (\(x) {
                            -log10(x)
                        })()
            )
        },
        measure_name = function(value) {
            return(
                self$source_data |>
                    distinct(CorrelationMeasureName) |>
                    pull() |>
                    as.character()
            )
        },
        raw_column_names = function(value) {
            return(
                c(
                    "QueryExperimentID", "QueryAnalyte", "ComparisonExperimentID",
                    "Analyte", "p.value", "-log10pvalue"
                )
            )
        },
        formatted_column_names = function(value) {
            return(
                c(
                    "Query Experiment ID", "Query Analyte", "Comparison Experiment ID",
                    "Comparison Analyte", "q-value (BH Adjusted)", "-log10(q-value)"
                )
            )
        },
        formatted_summary_data = function(value) {
            return(
                self$prepared_data |>
                    rename_with(~ self$formatted_column_names, all_of(self$raw_column_names)) |>
                    rename(`:=`(!!sym(self$measure_name), CorrelationValue)) |>
                    select(-c(QueryAnalyteKey, QueryAnalyteID, ComparisonAnalyteKey, CorrelationMeasureName, text,
                        AnalyteID, p.value.original, shape, selectedPoint, p.value.adjustment.method, formattedPValue))
            )
        }
    ),
    public = list(
        set_summary_data = function(source_data) {
            self$summary_data <- self$set_source_data(source_data)
        },
        prepare = function(source_data) {
            self$prepared_data <- self$set_summary_data(source_data) |>
                mutate(
                    shape = if_else(p.value == 0, "triangle-up", "circle"),
                    p.value = if_else(
                        p.value == 0,
                        10^-(self$summary_data_max_finite * 1.05),
                        p.value
                    ),
                    `-log10pvalue` = -log10(p.value),
                    selectedPoint = 0L
                ) |>
                # keep the row with the largest |CorrelationValue| per Analyte
                slice_max(
                    order_by = abs(CorrelationValue),
                    n = 1,
                    with_ties = FALSE,
                    by = Analyte
                ) |>
                mutate(
                    `p.value.adjustment.method` = self$adjustment_method,
                    formattedPValue = map2_chr(p.value, `p.value.adjustment.method`, formatPValue),
                    text = glue(
                        "Analyte: {Analyte} <br />{self$measure_name}: \\
                        {round(CorrelationValue, 2)} <br />{formattedPValue}"
                    )
                )
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
PreCalculatedSummaryPreparer <- R6Class(
    "PreCalculatedSummaryPreparer",
    inherit = SummaryDataPreparerBase,
    public = list(
        set_summary_data = function(source_data) {
            self$summary_data <- self$set_source_data(source_data)
        },
        prepare = function(source_data) {
            source <- self$set_summary_data(source_data)

            first_present <- function(candidates) {
                hits <- intersect(candidates, names(source))

                if (length(hits) == 0) {
                    return(NULL)
                }

                hits[[1]]
            }

            if (all(c("FoldChange", "pvalue", "padj") %in% names(source))) {

                self$prepared_data <- source |>
                    select("Analyte" = AnalyteName, FoldChange, pvalue, padj) |>
                    rename(
                        "p.value.original" = pvalue,
                        "p.value" = padj
                    ) |>
                    mutate(
                        shape = "circle",
                        selectedPoint = 0L,
                        log2FoldChange = log2(FoldChange),
                        `-log10pvalue` = -log10(p.value),
                        `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
                        formattedPValue = map2_chr(p.value, `p.value.adjustment.method`, formatPValue),
                        text = glue(
                            "Gene: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}"
                        ),
                        lmFormula = "
                        <a
                            href='https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html'
                            target='_blank'>DESeq2 model
                        </a>",
                        ivs = ""
                    )
            } else {
                analyte_col <- first_present(c("Analyte", "AnalyteName", "Feature", "Gene_name"))
                fold_col <- first_present(c("FoldChange", "log<sub>2</sub>(Fold Change)", "log2FoldChange"))
                p_orig_col <- first_present(c("p.value.original", "p-value (original)", "pvalue"))
                p_adj_col <- first_present(c("p.value", "q-value", "padj"))

                if (is.null(analyte_col) || is.null(fold_col) || is.null(p_adj_col)) {
                    stop("Precalculated summary artifact is missing required columns.", call. = FALSE)
                }

                self$prepared_data <- source |>
                    mutate(
                        Analyte = .data[[analyte_col]],
                        FoldChange = ifelse(grepl("log", fold_col, ignore.case = TRUE), 2 ^ as.numeric(.data[[fold_col]]), as.numeric(.data[[fold_col]])),
                        `p.value.original` = if (!is.null(p_orig_col)) as.numeric(.data[[p_orig_col]]) else as.numeric(.data[[p_adj_col]]),
                        `p.value` = as.numeric(.data[[p_adj_col]]),
                        shape = "circle",
                        selectedPoint = 0L,
                        log2FoldChange = log2(FoldChange),
                        `-log10pvalue` = -log10(p.value),
                        `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
                        formattedPValue = map2_chr(p.value, `p.value.adjustment.method`, formatPValue),
                        text = glue("Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}"),
                        lmFormula = "",
                        ivs = "",
                        AnalyteID = dense_rank(Analyte)
                    ) |>
                    select(AnalyteID, Analyte, FoldChange, `p.value.original`, `p.value`, shape, selectedPoint,
                        log2FoldChange, `-log10pvalue`, `p.value.adjustment.method`, formattedPValue, text,
                        lmFormula, ivs)
            }
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
PreCalculatedTOFASummaryPreparer <- R6Class(
    "PreCalculatedTOFASummaryPreparer",
    inherit = SummaryDataPreparerBase,
    public = list(
        set_summary_data = function(source_data) {
            self$summary_data <- self$set_source_data(source_data)
        },
        prepare = function(source_data, comparison = NULL, ...) {
            source <- self$set_summary_data(source_data)

            first_present <- function(candidates) {
                hits <- intersect(candidates, names(source))

                if (length(hits) == 0) {
                    return(NULL)
                }

                hits[[1]]
            }

            if (!is.null(comparison) && nzchar(trimws(comparison)) && "Timepoint" %in% names(source)) {
                comparison_parts <- strsplit(as.character(comparison), "\\|")[[1]]
                comparison_parts <- trimws(comparison_parts)
                comparison_parts <- comparison_parts[nzchar(comparison_parts)]

                selected_timepoint <- NULL
                if (length(comparison_parts) >= 2) {
                    selected_timepoint <- comparison_parts[[2]]
                } else if (length(comparison_parts) == 1) {
                    selected_timepoint <- comparison_parts[[1]]
                }

                if (!is.null(selected_timepoint) && nzchar(selected_timepoint)) {
                    source <- source |>
                        filter(Timepoint == selected_timepoint)
                }
            }

            analyte_col <- first_present(c("Analyte", "AnalyteName", "Score_name", "Feature"))
            fold_col <- first_present(c("Mean_difference", "FoldChange", "log2FoldChange"))
            p_orig_col <- first_present(c("pvalue", "p.value.original", "p.value"))
            p_adj_col <- first_present(c("padj", "qvalue", "p.value", "p_adj"))

            if (is.null(analyte_col) || is.null(fold_col) || is.null(p_adj_col)) {
                stop("TOFA precalculated artifact is missing required columns.", call. = FALSE)
            }

            self$prepared_data <- source |>
                mutate(
                    Analyte = .data[[analyte_col]],
                    FoldChange = as.numeric(.data[[fold_col]]),
                    `p.value.original` = if (!is.null(p_orig_col)) as.numeric(.data[[p_orig_col]]) else as.numeric(.data[[p_adj_col]]),
                    `p.value` = as.numeric(.data[[p_adj_col]]),
                    shape = "circle",
                    selectedPoint = 0L,
                    `-log10pvalue` = -log10(p.value),
                    `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
                    formattedPValue = map2_chr(p.value, `p.value.adjustment.method`, formatPValue),
                    text = glue(
                        "Score: {Analyte}<br />Difference: {round(FoldChange,2)}<br />{formattedPValue}"
                    ),
                    lmFormula = "",
                    ivs = ""
                )
            return(invisible(self$prepared_data))
        }
    )
)
