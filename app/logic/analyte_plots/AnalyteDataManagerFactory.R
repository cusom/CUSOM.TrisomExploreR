box::use(
    app/logic/analyte_plots/CategoricalAnalyteDataManager[CategoricalFeatureAnalysisAnalyteDataManager],
    app/logic/analyte_plots/ContinuousAnalyteDataManager[ContinuousFeatureAnalysisAnalyteDataManager]
)

#' @export
getFeatureAnalysisAnalyteDataManager <- function(
    analysis_config,
    study_data, 
    analyte,
    summary_data
    ) {
    if (analysis_config$AnalysisType == "Categorical") {
        return(
            CategoricalFeatureAnalysisAnalyteDataManager$new(
                analysis_config = analysis_config,
                study_data = study_data,
                analyte = analyte,
                summary_data = summary_data
            )
        )

    } else if (analysis_config$AnalysisType == "Continuous") {
        return(
            ContinuousFeatureAnalysisAnalyteDataManager$new(
                analysis_config = analysis_config,
                study_data = study_data,
                analyte = analyte,
                summary_data = summary_data
            )
        )
    }
}