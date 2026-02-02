box::use(
    R6[R6Class],
    dplyr[if_else]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/shared/global_utils[`%||%`],
    app/logic/feature_analysis/analyte/AnalyteDataManagers[RuntimeAnalyteDataSource, PreCalcualtedAnalyteDataSource, CorrelatesAnalyteDataSource],
    app/logic/feature_analysis/analyte/AnalyteDataPreparers[CategoricalSinglePreparer, ContinuousSinglePreparer, HeatmapPreparer, CorrelatesPreparer],
    app/logic/feature_analysis/analyte/AnalytePlotStrategies[BoxPlotStrategy, ScatterPlotStrategy, HeatmapPlotStrategy, ScatterPlotWithSmoothingStrategy]
)

getPlotKind <- function(analysis_type, analyte) {
    vars_n <- length(analyte %||% character())
    if (vars_n > 1) return("Heatmap")
    type <- trimws(analysis_type)
    if (type == "Continuous") return("Scatter")
    if (type == "Categorical") return("Box")
    if (type == "Correlates") return("Correlates")
    stop(sprintf("Unknown AnalysisType: '%s'", type), call. = FALSE)
}

getDataSource <- function(precalculated, analysis_type, analyte, analysis_config, ...) {
    type <- if_else(
        analysis_type == "Correlates", "Correlates",
        if(isTRUE(precalculated)) "Precalc" else "Runtime"
    )
    map <- list(
        Precalc     = PreCalcualtedAnalyteDataSource,
        Runtime     = RuntimeAnalyteDataSource,
        Correlates  = CorrelatesAnalyteDataSource
    )
    cls <- resolve_class(map, type, "DataSource")
    cls$new(analysis_config = analysis_config, analyte = analyte, ...)
}

getPreparer <- function(plot_kind, analysis_config, analyte, ...) {
    map <- list(
        Scatter     = ContinuousSinglePreparer,
        Box         = CategoricalSinglePreparer,
        Heatmap     = HeatmapPreparer,
        Correlates  = CorrelatesPreparer
    )
    cls <- resolve_class(map, plot_kind, "Preparer")
    cls$new(analysis_config = analysis_config, analyte = analyte, ...)
}

getPlotStrategy <- function(plot_kind, analysis_config, analyte, ...) {
    map <- list(
        Scatter     = ScatterPlotStrategy,
        Box         = BoxPlotStrategy,
        Heatmap     = HeatmapPlotStrategy,
        Correlates  = ScatterPlotWithSmoothingStrategy
    )
    cls <- resolve_class(map, plot_kind, "PlotStrategy")
    cls$new(analysis_config = analysis_config, analyte = analyte, ...)
}

FeatureAnalysisAnalyteRunner <- R6Class(
    "FeatureAnalysisAnalyteRunner",
    active = list(),
    public = list(
        precalculated = NULL,
        analysis_type = NULL,
        data_source = NULL,
        preparer = NULL,
        plotter = NULL,
        analyte = NULL,
        initialize = function(
            precalculated,
            analysis_type,
            data_source,
            preparer,
            plotter,
            analyte
        ) {
            self$precalculated <- precalculated
            self$analysis_type <- analysis_type
            self$data_source <- data_source
            self$preparer <- preparer
            self$plotter <- plotter
            self$analyte <- analyte
        },
        get_analyte_data = function(analyte) {
            self$data_source$get_data(analyte) |>
                self$preparer$prepare()
        },
        get_analyte_plot = function(.data) {
            self$plotter$render(.data)
        },
        get_table_data = function() {
            self$preparer$prepared_data
        }
    )
)

#' @export
getFeatureAnalysisForAnalyte <- function(
        analysis_config,
        analyte,
        ...
    ) {
    precalculated <- analysis_config$UsesPreCalculatedData
    analysis_type <- analysis_config$AnalysisType
    plot_kind  <- getPlotKind(analysis_type, analyte)
    data_src   <- getDataSource(precalculated, analysis_type, analyte, analysis_config, ...)
    preparer   <- getPreparer(plot_kind, analysis_config, analyte, ...)
    plotter    <- getPlotStrategy(plot_kind, analysis_config, analyte, ...)

    FeatureAnalysisAnalyteRunner$new(
        precalculated,
        analysis_type,
        data_src,
        preparer,
        plotter,
        analyte
    )
}
