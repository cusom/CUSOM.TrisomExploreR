box::use(
    app/logic/feature_analysis/inputs/inputs_feature_analysis[FeatureAnalysisInputsManager],
    app/logic/feature_analysis/inputs/inputs_precalculated_feature_analysis[PreCalculatedFeatureAnalysisInputsManager],
    app/logic/correlates_analysis/inputs_correlates[CorrelatesAnalysisInputsManager],
)


#' @export
get_inputs_manager <- function(app_config, analysis_config, input_config) {

    precalc <- isTRUE(analysis_config$UsesPreCalculatedData)

    key <- if (precalc) "Precalc" else "Raw"

    mgr_class <- switch(
        key,
        "Precalc" = PreCalculatedFeatureAnalysisInputsManager,
        "Raw"     = FeatureAnalysisInputsManager,
        stop(sprintf("No class mapping for routing key: %s", key), call. = FALSE)
    )

    mgr_class$new(app_config, analysis_config, input_config)

}