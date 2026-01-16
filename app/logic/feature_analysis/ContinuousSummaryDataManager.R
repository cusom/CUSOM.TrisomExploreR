box::use(
  app/logic/feature_analysis/SummaryDataManager[FeatureAnalysis_SummaryDataManager],
  app/logic/statistics/statistical_analysis[getLinearModelWithInteraction, formatPValue, addGroupCount],
)


#' @export
FeatureAnalysis_ContinuousSummaryDataManager <- R6::R6Class(
  "FeatureAnalysis_ContinuousSummaryDataManager",
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
        dplyr::select(LabID, Analyte, log2MeasuredValue, self$analysisVariable, self$Covariates(), Karyotype) |>
        dplyr::mutate(Karyotype = forcats::fct_relevel(Karyotype, "Control")) |>
        getLinearModelWithInteraction(
          id = LabID,
          key = Analyte,
          response = log2MeasuredValue,
          independentVariable = !!rlang::sym(self$analysisVariable),
          covariates = self$Covariates(),
          interactionVariable = Karyotype,
          adjustmentMethod = self$AdjustmentMethod()
        ) 

      return(self$VolcanoSummaryData)

    }

  )
)
