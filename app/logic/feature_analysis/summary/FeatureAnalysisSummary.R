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
    if (!is.null(precalculated)) {
        return(isTRUE(precalculated))
    }

    if (is.null(study_plan) || is.null(study_plan$execution_mode)) {
        return(FALSE)
    }

    if (identical(study_plan$execution_mode, "generated")) {
        return(FALSE)
    }

    if (identical(study_plan$execution_mode, "precalculated")) {
        return(TRUE)
    }

    FALSE
}

normalizeStatisticId <- function(stat_test) {
    if (is.null(stat_test) || length(stat_test) == 0) {
        return("linear_model")
    }

    stat_test <- as.character(stat_test[[1]])
    if (!nzchar(stat_test)) {
        return("linear_model")
    }

    mapping <- c(
        "Linear Model" = "linear_model",
        "linear model" = "linear_model",
        "Wilcoxon test" = "wilcoxon"
    )

    mapped <- unname(mapping[stat_test])
    if (length(mapped) == 1 && !is.na(mapped)) {
        return(mapped[[1]])
    }

    tolower(gsub("[^a-zA-Z0-9]+", "_", stat_test))
}

resolvePrecalculatedFromManifest <- function(app_config, study, stat_test) {
    if (is.null(app_config) || is.null(study) || !nzchar(study)) {
        return(FALSE)
    }

    dataset_def <- tryCatch(
        app_config$get_catalog_dataset_definition(study),
        error = function(e) NULL
    )

    if (is.null(dataset_def)) {
        return(FALSE)
    }

    package_id <- dataset_def$package
    if (is.null(package_id) || !nzchar(package_id)) {
        package_id <- dataset_def$id
    }

    if (is.null(package_id) || !nzchar(package_id)) {
        return(FALSE)
    }

    statistic_id <- normalizeStatisticId(stat_test)

    support <- tryCatch(
        app_config$package_resolver$resolve_statistic_support(package_id, statistic_id),
        error = function(e) NULL
    )

    if (is.null(support) || is.null(support$precalculated)) {
        return(FALSE)
    }

    isTRUE(support$precalculated)
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
        get_summary_data = function(source_data, ...) {
            prepared_source <- self$data_source$get_data(source_data)
            self$preparer$prepare(prepared_source, ...)
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

    args <- list(...)
    app_config <- args$app_config
    study <- args$study
    stat_test <- args$stat_test

    precalculated <- resolvePrecalculatedMode(
        NULL,
        study_plan = study_plan
    )

    if (!isTRUE(precalculated)) {
        precalculated <- resolvePrecalculatedFromManifest(app_config, study, stat_test)
    }

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
