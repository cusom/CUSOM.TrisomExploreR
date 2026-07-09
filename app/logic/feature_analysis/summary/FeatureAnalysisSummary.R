box::use(
    R6[R6Class],
    dplyr[case_when]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/feature_analysis/summary/SummaryDataManagers[RuntimeSummaryDataSource,
        PreCalculatedSummaryDataSource, CorrelatesSummaryDataSource],
    app/logic/feature_analysis/summary/SummaryDataPreparers[CategoricalSummaryPreparer,
        ContinuousSummaryPreparer, CorrelatesSummaryPreparer, PreCalculatedSummaryPreparer, PreCalculatedTOFASummaryPreparer],
    app/logic/feature_analysis/summary/SummaryDataPlotStrategies[VolcanoPlotStrategy, CorrelatesVolcanoPlotStrategy],
)

DATA_SOURCE_MAP <- list(
    Runtime = RuntimeSummaryDataSource,
    Precalc = PreCalculatedSummaryDataSource,
    Correlates = CorrelatesSummaryDataSource
)

PREPARER_MAP <- list(
    Categorical = CategoricalSummaryPreparer,
    Continuous = ContinuousSummaryPreparer,
    Correlates = CorrelatesSummaryPreparer,
    Precalc = PreCalculatedSummaryPreparer,
    PrecalcTOFA = PreCalculatedTOFASummaryPreparer
)

PLOT_STRATEGY_MAP <- list(
    Volcano = VolcanoPlotStrategy,
    Correlates = CorrelatesVolcanoPlotStrategy
)

getRouteProfile <- function(precalculated, analysis_config) {
    analysis_type <- trimws(analysis_config$AnalysisType)

    data_source_key <- case_when(
        analysis_type == "Correlates" ~ "Correlates",
        isTRUE(precalculated) ~ "Precalc",
        TRUE ~ "Runtime"
    )

    preparer_key <- case_when(
        analysis_type == "Correlates" ~ "Correlates",
        isTRUE(precalculated) && analysis_config$Namespace == "Timepoint" ~ "PrecalcTOFA",
        isTRUE(precalculated) ~ "Precalc",
        TRUE ~ analysis_type
    )

    plotter_key <- case_when(
        analysis_type == "Correlates" ~ "Correlates",
        TRUE ~ "Volcano"
    )

    list(
        data_source_key = data_source_key,
        preparer_key = preparer_key,
        plotter_key = plotter_key,
        analysis_type = analysis_type
    )
}

resolvePrecalculatedMode <- function(precalculated, study_plan = NULL) {
    if (is.null(study_plan) || is.null(study_plan$execution_mode)) {
        return(precalculated)
    }

    if (identical(study_plan$execution_mode, "generated")) {
        return(FALSE)
    }

    if (identical(study_plan$execution_mode, "precalculated")) {
        return(TRUE)
    }

    precalculated
}

instantiateMappedClass <- function(map, key, kind, analysis_config, ...) {
    cls <- resolve_class(map, key, kind)
    cls$new(analysis_config = analysis_config, ...)
}

getDataSource <- function(route_profile, analysis_config, study_plan = NULL, ...) {
    instantiateMappedClass(
        DATA_SOURCE_MAP,
        route_profile$data_source_key,
        "DataSource",
        analysis_config,
        study_plan = study_plan,
        ...
    )
}

getPreparer <- function(route_profile, analysis_config, ...) {
    instantiateMappedClass(PREPARER_MAP, route_profile$preparer_key, "Preparer", analysis_config, ...)
}

getPlotStrategy <- function(route_profile, analysis_config, ...) {
    instantiateMappedClass(PLOT_STRATEGY_MAP, route_profile$plotter_key, "PlotStrategy", analysis_config, ...)
}

FeatureAnalysisSummaryRunner <- R6Class(
    "FeatureAnalysisSummaryRunner",
    public = list(
        precalculated = NULL,
        analysis_type = NULL,
        data_source = NULL,
        preparer = NULL,
        plotter = NULL,
        initialize = function(
            precalculated,
            analysis_type,
            data_source,
            preparer,
            plotter
        ) {
            self$precalculated <- precalculated
            self$analysis_type <- analysis_type
            self$data_source <- data_source
            self$preparer <- preparer
            self$plotter <- plotter
        },
        get_summary_data = function(source_data) {
            self$data_source$get_data(source_data) |>
                self$preparer$prepare()
        },
        get_summary_plot = function(.data) {
            self$plotter$render(.data)
        },
        set_analyte = function(analyte) {
            self$plotter$analyte <- analyte
        },
        get_table_data = function() {
            self$preparer$formatted_summary_data
        }
    )
)

#' @export
getFeatureAnalysisSummary <- function(
        analysis_config,
    study_plan = NULL,
        ...
    ) {

    precalculated <- resolvePrecalculatedMode(
        analysis_config$UsesPreCalculatedData,
        study_plan = study_plan
    )
    route_profile <- getRouteProfile(precalculated, analysis_config)
    data_src <- getDataSource(route_profile, analysis_config, study_plan = study_plan, ...)
    preparer <- getPreparer(route_profile, analysis_config, ...)
    plotter <- getPlotStrategy(route_profile, analysis_config, ...)

    FeatureAnalysisSummaryRunner$new(
        precalculated,
        route_profile$analysis_type,
        data_src,
        preparer,
        plotter
    )
}
