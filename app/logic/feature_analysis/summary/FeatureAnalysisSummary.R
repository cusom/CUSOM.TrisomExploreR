box::use(
    R6[R6Class],
    dplyr[if_else]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/shared/global_utils[`%||%`],
    app/logic/feature_analysis/summary/SummaryDataManagers[RuntimeSummaryDataSource,
        PreCalculatedSummaryDataSource, CorrelatesSummaryDataSource],
    app/logic/feature_analysis/summary/SummaryDataPreparers[CategoricalSummaryPreparer,
        ContinuousSummaryPreparer, CorrelatesSummaryPreparer, PreCalculatedSummaryPreparer],
    app/logic/feature_analysis/summary/SummaryDataPlotStrategies[VolcanoPlotStrategy, CorrelatesVolcanoPlotStrategy],
)

getPlotKind <- function(analysis_type) {
    return("Volcano")
}

getDataSource <- function(precalculated, analysis_type, analysis_config, ...) {
    type <- if_else(
        analysis_type == "Correlates", "Correlates",
        if (isTRUE(precalculated)) "Precalc" else "Runtime"
    )
    map <- list(
        Runtime     = RuntimeSummaryDataSource,
        Precalc     = PreCalculatedSummaryDataSource,
        Correlates  = CorrelatesSummaryDataSource
    )
    cls <- resolve_class(map, type, "DataSource")
    cls$new(analysis_config = analysis_config, ...)
}

getPreparer <- function(precalculated, analysis_type, analysis_config, ...) {
    type <- if (
        analysis_type == "Correlates"
    ) {
        "Correlates"
    } else if (isTRUE(precalculated)) {
        "Precalc"
    } else {
        analysis_type
    }
    map <- list(
        Categorical  = CategoricalSummaryPreparer,
        Continuous   = ContinuousSummaryPreparer,
        Correlates   = CorrelatesSummaryPreparer,
        Precalc      = PreCalculatedSummaryPreparer
    )
    cls <- resolve_class(map, type, "Preparer")
    cls$new(analysis_config = analysis_config, ...)
}

getPlotStrategy <- function(plot_kind, analysis_type, analysis_config, ...) {
    type <- if_else(
        analysis_type == "Correlates", "Correlates",
        plot_kind
    )
    map <- list(
        Volcano     = VolcanoPlotStrategy,
        Correlates  = CorrelatesVolcanoPlotStrategy
    )
    cls <- resolve_class(map, type, "PlotStrategy")
    cls$new(analysis_config = analysis_config, ...)
}

FeatureAnalysisSummaryRunner <- R6Class(
    "FeatureAnalysisSummaryRunner",
    active = list(),
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
        ...
    ) {

    precalculated <- analysis_config$UsesPreCalculatedData
    analysis_type <- analysis_config$AnalysisType
    plot_kind  <- getPlotKind(analysis_type)
    data_src   <- getDataSource(precalculated, analysis_type, analysis_config, ...)
    preparer   <- getPreparer(precalculated, analysis_type, analysis_config, ...)
    plotter    <- getPlotStrategy(plot_kind, analysis_type, analysis_config, ...)

    FeatureAnalysisSummaryRunner$new(
        precalculated,
        analysis_type,
        data_src,
        preparer,
        plotter
    )
}
