box::use(
  app/logic/feature_analysis/SummaryDataManager[FeatureAnalysis_SummaryDataManager],
  app/logic/statistics/statistical_analysis[getStatTestByKeyGroup, formatPValue, addGroupCount],
)


#' @export
FeatureAnalysis_CategoricalSummaryDataManager <- R6::R6Class(
  "FeatureAnalysis_CategoricalSummaryDataManager",
  inherit = FeatureAnalysis_SummaryDataManager,
  private = list(),
  public = list(

    initialize = function(analysis_config, StatTest, Covariates, AdjustmentMethod) {

      super$initialize(analysis_config, StatTest, Covariates, AdjustmentMethod)
  
    },
  
    setVolcanoSummaryData = function(.data) {
    
      if (is.null(.data)) {
        .data <- self$StudyData
      }

      self$VolcanoSummaryData <- .data |>
        dplyr::select(LabID, Analyte, log2MeasuredValue, self$analysisVariable, self$Covariates()) |>
        dplyr::mutate_at(dplyr::vars(self$analysisVariable), ~forcats::fct_relevel(.x, self$groupBaselineLabel)) |>
        getStatTestByKeyGroup(
          id = LabID,
          key = Analyte,
          response = log2MeasuredValue,
          independentVariable = !!rlang::sym(self$analysisVariable),
          baselineLabel = self$groupBaselineLabel,
          testMethod = self$StatTest(),
          adjustmentMethod = self$AdjustmentMethod(),
          covariates = self$Covariates()
        ) 

      return(self$VolcanoSummaryData)

    }

  )
)
