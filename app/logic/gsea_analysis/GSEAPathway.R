box::use(
    R6[R6Class],
    dplyr[case_when],
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/shared/global_utils[`%||%`],
    app/logic/gsea_analysis/GSEAPathwayDataManagers[GSEADataSourceSOMA, GSEADataSourceRNA],
    app/logic/gsea_analysis/GSEAPathwayDataPreparers[GSEADataPreparerBase],
    app/logic/gsea_analysis/GSEAPathwayPlotStrategies[GSEAPlotStrategy]
)

getDataSource <- function(type, study, summary_data, ...) {

    map <- list(
        SOMA = GSEADataSourceSOMA,
        RNA  = GSEADataSourceRNA
    )
    cls <- resolve_class(map, type, "DataSource")
    cls$new(study = study, summary_data = summary_data, ...)
}

getPreparer <- function(...) {
    type <- "All"
    map <- list(
        All = GSEADataPreparerBase
    )
    cls <- resolve_class(map, type, "Preparer")
    cls$new(...)
}

getPlotStrategy <- function(study, summary_data, ...) {
    type <- "All"
    map <- list(
        All     = GSEAPlotStrategy
    )
    cls <- resolve_class(map, type, "PlotStrategy")
    cls$new(study = study, summary_data = summary_data, ...)
}

GSEAAnalysisRunner <- R6Class(
    "GSEAAnalysisRunner",
    active = list(
        gsea_trace_name = function(value) {
            return(self$plotter$gsea_trace_name)
        },
        gsea_analytes = function(value) {
            return(self$plotter$gsea_analytes)
        }
    ),
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
        get_gsea_data = function() {
            data <- self$data_source$get_data()
            prepared_data <- self$preparer$prepare(data)
            return(prepared_data)
        },
        render_gsea_plot = function(.data) {
            self$plotter$render_gsea_plot(.data)
        },
        set_event_data = function(event_data) {
            self$plotter$event_data <- event_data
        },
        set_GSEA_pathway_data = function(trace_name) {
            self$plotter$set_GSEA_pathway_data(trace_name)
        },
        render_enrichment_plot = function() {
            self$plotter$render_enrichment_plot()
        }
    )
)

#' @export
getGSEAPathwayAnalysis <- function(
        app_config,
        study,
        summary_data,
        ...
    ) {
        type <- case_when(
            grepl("SOMA", study) ~ "SOMA",
            grepl("RNA", study) ~ "RNA",
            TRUE ~ NA
        )


        data_src   <- getDataSource(type, study, summary_data, ...)
        preparer   <- getPreparer(...)
        plotter    <- getPlotStrategy(study, summary_data, ...)

        GSEAAnalysisRunner$new(
            data_source = data_src,
            preparer = preparer,
            plotter = plotter
        )
}