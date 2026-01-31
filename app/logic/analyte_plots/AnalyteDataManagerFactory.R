box::use(
    app/logic/analyte_plots/CategoricalAnalyteDataManager[CategoricalFeatureAnalysisAnalyteDataManager],
    app/logic/analyte_plots/PreCalculatedCategoricalAnalyteDataManager[PreCalculatedCategoricalFeatureAnalysisAnalyteDataManager],
    app/logic/analyte_plots/ContinuousAnalyteDataManager[ContinuousFeatureAnalysisAnalyteDataManager]
)

#' @export
getFeatureAnalysisAnalyteDataManager <- function(
    app_config, 
    analysis_config,
    study,
    study_data,
    analyte,
    summary_data
    ) {
    if (analysis_config$AnalysisType == "Categorical") {
        if (analysis_config$UsesPreCalculatedData) {
            PreCalculatedCategoricalFeatureAnalysisAnalyteDataManager$new(
                app_config = app_config,
                analysis_config = analysis_config,
                study = study,
                study_data = study_data,
                analyte = analyte,
                summary_data = summary_data
            )
        } else {
            return(
                CategoricalFeatureAnalysisAnalyteDataManager$new(
                    app_config = app_config,
                    analysis_config = analysis_config,
                    study = study,
                    study_data = study_data,
                    analyte = analyte,
                    summary_data = summary_data
                )
            )
        }
    } else if (analysis_config$AnalysisType == "Continuous") {
        return(
            ContinuousFeatureAnalysisAnalyteDataManager$new(
                app_config = app_config,
                analysis_config = analysis_config,
                study = study,
                study_data = study_data,
                analyte = analyte,
                summary_data = summary_data
            )
        )
    }
}