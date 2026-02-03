box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, mutate, mutate_at, group_by, summarise, ungroup, rename_with, distinct, n,
            pull, arrange, dense_rank, row_number, vars],
    forcats[fct_relevel],
    purrr[pmap],
    stringr[str_split_1],
    rlang[sym],
)

box::use(
    app/logic/shared/statistical_analysis[getStatTestByKeyGroup, getLinearModelWithInteraction, 
        formatPValue, addGroupCount],
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
        analysis_variable = function(value) {
            return(
                private$analysis_config$AnalysisVariableName
            )
        },
        group_baseline_label = function(value) {
            return(
                private$analysis_config$AnalysisVariableBaselineLabel
            )
        }
    ),
    public = list(
        study = NULL,
        study_data = NULL,
        stat_test = NULL,
        covariates = NULL,
        adjustment_method = NULL,
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
        set_summary_data = function(source_data) {
            stop("Abstract: must implement")
        },
        prepare = function(source_data) {
            self$prepared_data <- self$set_summary_data(source_data) |>
                mutate(
                    log2FoldChange = log2(FoldChange),
                    `-log10pvalue` = -log10(p.value),
                    `p.value.adjustment.method` = self$adjustment_method,
                    formattedPValue = unlist(
                        pmap(
                            .l = list(p.value, p.value.adjustment.method),
                            formatPValue
                        )
                    ),
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
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            super$initialize(analysis_config, app_config, study, study_data,
                stat_test, covariates, adjustment_method)
        },
        set_summary_data = function(source_data) {
            self$summary_data <- source_data |>
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
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            super$initialize(analysis_config, app_config, study, study_data,
                stat_test, covariates, adjustment_method)
        },
        set_summary_data = function(source_data) {
            self$summary_data <- source_data |>
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
                )
            return(invisible(self$summary_data))
        }
    )
)

#' @export
CorrelatesSummaryPreparer <- R6Class(
    "CorrelatesSummaryPreparer",
    inherit = SummaryDataPreparerBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            super$initialize(analysis_config, app_config, study, study_data,
                stat_test, covariates, adjustment_method)
        },
        set_summary_data = function(source_data) {
            source_data
        }
    )
)
