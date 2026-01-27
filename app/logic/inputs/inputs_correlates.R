box::use(
  app/logic/inputs/inputs_base[InputsManagerBase],
  app/logic/app_resources/data_services[ODBCQueryManager]
)

#' @export
CorrelatesAnalysisInputsManager <- R6::R6Class(
  "CorrelatesAnalysisInputsManager",
  inherit = InputsManagerBase,
  private = list(),
  active = list(),
  public = list(
    QueryExperiment = NULL,
    CompareExperiment = NULL,
    QueryAnalytes = NULL,
    QueryAnalyte = NULL,
    CorrelationSourceData = NULL,
    initialize = function(app_config, analysis_config, input_config) {
      super$initialize(app_config, analysis_config, input_config)
    },
    getQueryExperiments = function() {
      self$remoteDB$getQuery(
        "[shiny].[GetQueryExperiments] ?",
        tibble::tibble("ApplicationID" = self$application_id)
      )
    },
    getComparisonExperiments = function() {
      self$remoteDB$getQuery(
        "[shiny].[GetComparisonExperiments] ?",
        tibble::tibble("QueryExperimentID" = self$QueryExperiment)
      )
    },
    getQueryAnalytes = function() {
      self$QueryAnalytes <- self$remoteDB$getQuery(
          "EXEC [shiny].[GetQueryAnalytesByQueryExperimentComparisonExperiment] ?, ?",
          tibble::tibble(
            "QueryExperiment" = self$QueryExperiment,
            "ComparisonExperiment" = self$CompareExperiment
          )
        )
      return(
        self$QueryAnalytes |>
          dplyr::arrange(QueryAnalyte) |>
          dplyr::select(QueryAnalyte, QueryAnalyteKey) |>
          tibble::deframe()
      )
    },
    set_correlation_source_data = function() {
      self$CorrelationSourceData <- self$remoteDB$getQuery(
        "[shiny].[GetCorrelationDatasetByExperiments] ?, ?, ?",
        tibble::tibble(
          "QueryExperiment" = self$QueryExperiment,
          "QueryAnalyte" = self$QueryAnalyte,
          "ComparisonExperiment" = self$CompareExperiment
        )
      ) |>
      dplyr::rename(
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
