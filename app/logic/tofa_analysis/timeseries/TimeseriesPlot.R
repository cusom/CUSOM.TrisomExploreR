box::use(
    R6[R6Class],
    dplyr[if_else, case_when]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/tofa_analysis/timeseries/TimeseriesPlotDataManagers[EndpointsTimeSeriesDataSource, NULISATimeSeriesDataSource, OLINKTimeSeriesDataSource],
    app/logic/tofa_analysis/timeseries/TimeseriesPlotDataPreparers[BaseTimeseriesPreparer, DifferenceTimeseriesPreparer],
    app/logic/tofa_analysis/timeseries/TimeseriesPlotStrategies[BasePlotStrategy, DifferencePlotStrategy]
)

getPlotKind <- function(analysis_type) {
    return("BoxPlot")
}

getDataSource <- function(analysis_config, dataset_type, plot_type, dataset, cohort, feature, ...) {

    map <- list(
        endpoints           = EndpointsTimeSeriesDataSource,
        nulisa              = NULISATimeSeriesDataSource,
        olink               = OLINKTimeSeriesDataSource
    )
    cls <- resolve_class(map, dataset_type, "DataSource")
    cls$new(analysis_config, plot_type, dataset, cohort, feature, ...)
}

getPreparer <- function(analysis_config, dataset_type, plot_type, dataset, cohort, feature, ...) {

    map <- list(
        Base            = BaseTimeseriesPreparer,
        Difference      = DifferenceTimeseriesPreparer
    )
    cls <- resolve_class(map, plot_type, "Preparer")
    cls$new(plot_type, dataset, cohort, feature, ...)
}

getPlotStrategy <- function(analysis_config, dataset_type, plot_type, dataset, cohort, feature, ...) {

    map <- list(
        Base            = BasePlotStrategy,
        Difference      = DifferencePlotStrategy
    )
    cls <- resolve_class(map, plot_type, "PlotStrategy")
    cls$new(plot_type, dataset, cohort, feature, ...)
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
        analysis_config,
        plot_type,
        dataset,
        cohort,
        feature,
        ...
    ) {

    dataset_type <- case_when(
        grepl("Endpoints", dataset, ignore.case = TRUE) ~ "endpoints",
        grepl("Nulisa", dataset, ignore.case = TRUE) ~ "nulisa",
        grepl("Olink", dataset, ignore.case = TRUE) ~ "olink",
        TRUE ~ "other"
    )

    data_src   <- getDataSource(analysis_config, dataset_type, plot_type, dataset, cohort, feature, ...)
    preparer   <- getPreparer(analysis_config, dataset_type, plot_type, dataset, cohort, feature, ...)
    plotter    <- getPlotStrategy(analysis_config, dataset_type, plot_type, dataset, cohort, feature, ...)

    TimeseriesPlotRunner$new(
        data_src,
        preparer,
        plotter
    )
}