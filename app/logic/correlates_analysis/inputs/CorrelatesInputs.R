box::use(
    dplyr[if_else, case_when],
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/shared/global_utils[`%||%`],
    app/logic/correlates_analysis/inputs/CorrelatesInputsDataManagers[
        InputsManagerCorrelates
    ],
    app/logic/correlates_analysis/inputs/CorrelatesInputsDataPreparers[
        CorrelatesAnalysisInputsPreparer
    ]
)

box::use(
    R6[R6Class],
    dplyr[between, filter, mutate, select, distinct, arrange, summarise, pull],
    stringr[str_split, str_c],
    tibble[tibble]
)

getDataSource <- function(app_config, analysis_type, analysis_config, input_config, ...) {

    map <- list(
        Correlates =            InputsManagerCorrelates
    )
    cls <- resolve_class(map, analysis_type, "DataSource")
    cls$new(
        app_config = app_config,
        analysis_config = analysis_config,
        input_config = input_config,
        ...
    )
}

getPreparer <- function(precalculated, analysis_type, analysis_config, app_config, ...) {
    map <- list(
        Correlates      = CorrelatesAnalysisInputsPreparer
    )
    cls <- resolve_class(map, analysis_type, "Preparer")
    cls$new(analysis_config = analysis_config, app_config = app_config, ...)
}

CorrelatesAnalysisInputsRunner <- R6Class(
    "CorrelatesAnalysisInputsRunner",
    active = list(
        fold_change_variable = function() {
            return(
                self$data_source$FoldChangeVar %||% self$data_source$FoldChangeVar %||% "rho"
            )
        }
    ),
    public = list(
        precalculated = NULL,
        analysis_type = NULL,
        data_source = NULL,
        preparer = NULL,
        initialize = function(
            precalculated,
            analysis_type,
            data_source,
            preparer
        ) {
            self$precalculated <- precalculated
            self$analysis_type <- analysis_type
            self$data_source <- data_source
            self$preparer <- preparer
        },
        getQueryExperiments = function() {
            self$data_source$getQueryExperiments()
        },
        getComparisonExperiments = function(query_experiment_id) {
            self$data_source$getComparisonExperiments(query_experiment_id)
        },
        getQueryAnalytes = function(query_experiment_id, comparison_experiment_id) {
            self$data_source$getQueryAnalytes(query_experiment_id, comparison_experiment_id)   
        },
        get_correlation_data = function(
                query_experiment_id, 
                comparison_experiment_id,
                query_analyte_id
            ) {
                self$data_source$set_correlation_source_data(
                    query_experiment_id, 
                    comparison_experiment_id,
                    query_analyte_id
                )
        }
    )
)

#' @export
getCorrelatesAnalysisInputs <- function(
        app_config,
        analysis_config,
        input_config,
        ...
    ) {
    precalculated <- FALSE
    analysis_type <- analysis_config$AnalysisType
    data_src   <- getDataSource(app_config, analysis_type, analysis_config, input_config, ...)
    preparer   <- getPreparer(precalculated, analysis_type, analysis_config, app_config, ...)

    CorrelatesAnalysisInputsRunner$new(
        precalculated,
        analysis_type,
        data_src,
        preparer
    )
}