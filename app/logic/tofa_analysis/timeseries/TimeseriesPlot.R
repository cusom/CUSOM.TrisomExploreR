box::use(
    R6[R6Class],
    dplyr[if_else]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/tofa_analysis/timeseries/TimeseriesPlotDataManagers[BaseTimeseriesDataSource],
    app/logic/tofa_analysis/timeseries/TimeseriesPlotDataPreparers[BaseTimeseriesPreparer, DifferenceTimeseriesPreparer],
    app/logic/tofa_analysis/timeseries/TimeseriesPlotStrategies[BasePlotStrategy, DifferencePlotStrategy]
)

getPlotKind <- function(analysis_type) {
    return("BoxPlot")
}

getDataSource <- function(type, dataset, cohort, feature, ...) {

    map <- list(
        Base            = BaseTimeseriesDataSource,
        Difference      = BaseTimeseriesDataSource
    )
    cls <- resolve_class(map, type, "DataSource")
    cls$new(type, dataset, cohort, feature, ...)
}

getPreparer <- function(type, dataset, cohort, feature, ...) {   
    map <- list(
        Base            = BaseTimeseriesPreparer,
        Difference      = DifferenceTimeseriesPreparer
    )
    cls <- resolve_class(map, type, "Preparer")
    cls$new(type, dataset, cohort, feature, ...)
}

getPlotStrategy <- function(type, dataset, cohort, feature, ...) {

    map <- list(
        Base            = BasePlotStrategy,
        Difference      = DifferencePlotStrategy
    )
    cls <- resolve_class(map, type, "PlotStrategy")
    cls$new(type, dataset, cohort, feature, ...)
}


TimeseriesPlotRunner <- R6Class(
    "TimeseriesPlotRunner",
    active = list(),
    public = list(
        data_source = NULL,
        preparer = NULL,
        plotter = NULL,
        initialize = function(
            data_source,
            preparer,
            plotter
        ) {
            self$data_source <- data_source
            self$preparer <- preparer
            self$plotter <- plotter
        },
        get_plot = function(dataset, cohort, feature, plot_type) {
    
            data <- self$data_source$get_data(dataset, cohort, feature)
            prepared_data <- self$preparer$prepare(data)
            plot <- self$plotter$render(prepared_data)
            return(plot)
            
        }
    )
)

#' @export
getTimeseriesPlot <- function(
        type, 
        dataset,
        cohort,
        feature,
        ...
    ) {

    data_src   <- getDataSource(type, dataset, cohort, feature, ...)
    preparer   <- getPreparer(type, dataset, cohort, feature, ...)
    plotter    <- getPlotStrategy(type, dataset, cohort, feature, ...)

    TimeseriesPlotRunner$new(
        data_src,
        preparer,
        plotter
    )
}