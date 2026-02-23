box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble, deframe],
    dplyr[select, filter, pull, distinct, arrange, rename],
)

box::use(
    app/logic/feature_analysis/inputs/InputsDataManagers[
        InputsManagerBase
    ],
)

#' @export
InputsManagerCorrelates <- R6Class(
    "InputsManagerCorrelates",
    inherit = InputsManagerBase,
    private = list(),
    active = list(),
    public = list(
        QueryExperiment = NULL,
        CompareExperiment = NULL,
        QueryAnalytes = NULL,
        QueryAnalyte = NULL,
        FoldChangeVar = "rho",
        CorrelationSourceData = NULL,
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        },
        getQueryExperiments = function() {
            self$remoteDB$getQuery(
                "[shiny].[GetQueryExperiments] ?",
                tibble("ApplicationID" = self$application_id)
            )
        },
        getComparisonExperiments = function(query_experiment_id) {
            self$remoteDB$getQuery(
                    "[shiny].[GetComparisonExperiments] ?",
                    tibble("QueryExperimentID" = query_experiment_id)
                )
        },
        getQueryAnalytes = function(query_experiment_id, comparison_experiment_id) {
            self$QueryAnalytes <- self$remoteDB$getQuery(
                "EXEC [shiny].[GetQueryAnalytesByQueryExperimentComparisonExperiment] ?, ?",
                    tibble(
                        "QueryExperiment" = query_experiment_id,
                        "ComparisonExperiment" = comparison_experiment_id
                    )
                )
            return(
                self$QueryAnalytes |>
                    arrange(QueryAnalyte) |>
                    select(QueryAnalyte, QueryAnalyteKey) |>
                    deframe()
            )
        },
        set_correlation_source_data = function(
            query_experiment_id,
            comparison_experiment_id,
            query_analyte_id
        ) {
            self$CorrelationSourceData <- self$remoteDB$getQuery(
                "[shiny].[GetCorrelationDatasetByExperiments] ?, ?, ?",
                tibble(
                    "QueryExperiment" = query_experiment_id,
                    "QueryAnalyte" = query_analyte_id,
                    "ComparisonExperiment" = comparison_experiment_id
                )
            ) |>
            rename(
                "Analyte" = ComparisonAnalyte,
                "AnalyteID" = ComparisonAnalyteID
            )
            return(invisible(self$CorrelationSourceData))
        },
        get_correlation_data = function() {
            if (is.null(self$CorrelationSourceData)) {
                self$set_correlation_source_data()
            }
            return(self$CorrelationSourceData)
        }
    )
)