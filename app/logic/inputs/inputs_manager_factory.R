box::use(
    app/logic/inputs/inputs_feature_analysis[FeatureAnalysisInputsManager],
    app/logic/inputs/inputs_precalculated_feature_analysis[PreCalculatedFeatureAnalysisInputsManager],
    app/logic/inputs/inputs_correlates[CorrelatesAnalysisInputsManager],
)


#' @export
get_inputs_manager <- function(app_config, analysis_config, input_config) {
    if (analysis_config$UsesPreCalculatedData) {
        return(
            PreCalculatedFeatureAnalysisInputsManager$new(
                app_config = app_config,
                analysis_config = analysis_config,
                input_config = input_config
            )
        )
    } else {
        return(
            FeatureAnalysisInputsManager$new(
                app_config = app_config,
                analysis_config = analysis_config,
                input_config = input_config
            )
        )
    }
}