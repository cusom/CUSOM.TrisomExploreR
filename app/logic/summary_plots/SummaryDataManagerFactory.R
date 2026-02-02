box::use(
    app/logic/shared/factory_routing[resolve_route_map],
    app/logic/summary_plots/Routing[get_summary_data_manager_route_map],
)

#' @export
getFeatureAnalysisSummaryDataManager <- function(analysis_config, StatTest, Covariates, AdjustmentMethod) {

    analysis_type <- analysis_config$AnalysisType
    if (!is.character(analysis_type) || length(analysis_type) != 1L) {
        stop("analysis_config$AnalysisType must be a single string.", call. = FALSE)
    }

    analysis_type <- trimws(analysis_type)

    allowed_types <- c("Categorical", "Continuous")
    if (!analysis_type %in% allowed_types) {
        stop(sprintf(
        "Unknown AnalysisType: '%s'. Expected one of: %s",
        analysis_type, paste(allowed_types, collapse = ", ")
        ), call. = FALSE)
    }

    precalc_key <- if (isTRUE(analysis_config$UsesPreCalculatedData)) "Precalc" else "Raw"

    mgr_class <- resolve_route_map(
        route_map = get_summary_data_manager_route_map(), 
        keys = c(AnalysisType = analysis_type, Data = precalc_key)
    )

    mgr_class$new(
        analysis_config, 
        StatTest, 
        Covariates, 
        AdjustmentMethod
    )

}