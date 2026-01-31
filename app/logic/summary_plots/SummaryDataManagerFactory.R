box::use(
    app/logic/summary_plots/SummaryDataManager[FeatureAnalysis_SummaryDataManager],
    app/logic/summary_plots/CategoricalSummaryDataManager[FeatureAnalysis_CategoricalSummaryDataManager],
    app/logic/summary_plots/PreCalculatedCategoricalSummaryDataManager[PreCalculatedFeatureAnalysis_CategoricalSummaryDataManager],
    app/logic/summary_plots/ContinuousSummaryDataManager[FeatureAnalysis_ContinuousSummaryDataManager]
)

#' @export
getFeatureAnalysisSummaryDataManager <- function(analysis_config, StatTest, Covariates, AdjustmentMethod) {
    if (analysis_config$AnalysisType == "Categorical") {
        if (analysis_config$UsesPreCalculatedData) {
            return(
                PreCalculatedFeatureAnalysis_CategoricalSummaryDataManager$new(
                    analysis_config, StatTest, Covariates, AdjustmentMethod
                )
            )
        } else {
            return(
                FeatureAnalysis_CategoricalSummaryDataManager$new(
                    analysis_config, StatTest, Covariates, AdjustmentMethod
                )
            )
        }
    } else if (analysis_config$AnalysisType == "Continuous") {
        return(
            FeatureAnalysis_ContinuousSummaryDataManager$new(
                analysis_config, StatTest, Covariates, AdjustmentMethod
            )
        )
    } else {
        stop("Unknown Analysis Type")
    }

}