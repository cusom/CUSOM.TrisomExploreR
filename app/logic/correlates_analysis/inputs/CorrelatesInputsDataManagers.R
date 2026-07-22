box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, filter, pull, distinct, arrange, rename, mutate, inner_join, join_by],
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
    private = list(
        query_experiments_cache = NULL,
        comparison_experiments_cache = list(),
        query_analytes_cache = list(),
        correlation_source_cache = list(),
        make_cache_key = function(...) {
            paste(..., sep = "::")
        }
    ),
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
            private$query_experiments_cache <- NULL
            private$comparison_experiments_cache <- list()
            private$query_analytes_cache <- list()
            private$correlation_source_cache <- list()
        },
        getQueryExperiments = function() {
            if (!is.null(private$query_experiments_cache)) {
                return(private$query_experiments_cache)
            }

            private$query_experiments_cache <- self$remoteDB$getQuery(
                "[shiny].[GetQueryExperiments] ?",
                tibble("ApplicationID" = self$application_id)
            )

            private$query_experiments_cache
        },
        getComparisonExperiments = function(query_experiment_id) {
            cache_key <- as.character(query_experiment_id)
            if (!is.null(private$comparison_experiments_cache[[cache_key]])) {
                return(private$comparison_experiments_cache[[cache_key]])
            }

            private$comparison_experiments_cache[[cache_key]] <- self$remoteDB$getQuery(
                    "[shiny].[GetComparisonExperiments] ?",
                    tibble("QueryExperimentID" = query_experiment_id)
                )

            private$comparison_experiments_cache[[cache_key]]
        },
        getQueryAnalytes = function(query_experiment_id, comparison_experiment_id) {
            cache_key <- private$make_cache_key(query_experiment_id, comparison_experiment_id)
            if (!is.null(private$query_analytes_cache[[cache_key]])) {
                self$QueryAnalytes <- private$query_analytes_cache[[cache_key]]
                return(
                    self$QueryAnalytes |>
                        select(QueryAnalyte, QueryAnalyteKey)
                )
            }

            self$QueryAnalytes <- self$remoteDB$getQuery(
                "EXEC [shiny].[GetQueryAnalytesByQueryExperimentComparisonExperiment] ?, ?",
                    tibble(
                        "QueryExperiment" = query_experiment_id,
                        "ComparisonExperiment" = comparison_experiment_id
                    )
                )
            private$query_analytes_cache[[cache_key]] <- self$QueryAnalytes

            return(
                self$QueryAnalytes |>
                    select(QueryAnalyte, QueryAnalyteKey)
            )
        },
        set_correlation_source_data = function(
            query_experiment_id,
            comparison_experiment_id,
            query_analyte_id
        ) {
            cache_key <- private$make_cache_key(
                query_experiment_id,
                comparison_experiment_id,
                query_analyte_id
            )
            if (!is.null(private$correlation_source_cache[[cache_key]])) {
                self$CorrelationSourceData <- private$correlation_source_cache[[cache_key]]
                return(invisible(self$CorrelationSourceData))
            }

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
            private$correlation_source_cache[[cache_key]] <- self$CorrelationSourceData

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