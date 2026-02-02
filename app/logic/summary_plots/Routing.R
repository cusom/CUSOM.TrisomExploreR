
box::use(
    app/logic/summary_plots/CategoricalSummaryDataManager[FeatureAnalysis_CategoricalSummaryDataManager],
    app/logic/summary_plots/PreCalculatedSummaryDataManager[PreCalculatedFeatureAnalysis_SummaryDataManager],
    app/logic/summary_plots/ContinuousSummaryDataManager[FeatureAnalysis_ContinuousSummaryDataManager]
)

#' @export 
get_summary_data_manager_route_map <- function() {

    list(
        Categorical = list(
            Raw     = FeatureAnalysis_CategoricalSummaryDataManager,
            Precalc = PreCalculatedFeatureAnalysis_SummaryDataManager
        ),
        Continuous = list(
            Raw     = FeatureAnalysis_ContinuousSummaryDataManager,
            Precalc = PreCalculatedFeatureAnalysis_SummaryDataManager
        )
    )

}