box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/shared/global_utils[`%||%`],
    app/logic/feature_analysis/inputs/InputsDataManagers[FeatureAnalysisInputsManager,
        PreCalculatedFeatureAnalysisInputsManager],
    app/logic/feature_analysis/inputs/InputsDataPreparers[FeatureAnalysisInputsDataPreparer,
        PreCalculatedFeatureAnalysisInputsPreparer]
)

box::use(
    R6[R6Class],
    dplyr[between, filter, mutate, select, distinct, arrange, summarise, pull],
    stringr[str_split, str_c],
    tibble[tibble]
)

getDataSource <- function(precalculated, app_config, analysis_type, analysis_config, input_config, ...) {
    type <- if (isTRUE(precalculated)) "Precalc" else "Runtime"

    map <- list(
        Runtime     = FeatureAnalysisInputsManager,
        Precalc     = PreCalculatedFeatureAnalysisInputsManager
    )
    cls <- resolve_class(map, type, "DataSource")
    cls$new(
        app_config = app_config,
        analysis_config = analysis_config,
        input_config = input_config,
        ...
    )
}

getPreparer <- function(precalculated, analysis_type, analysis_config, ...) {
    type <- if (isTRUE(precalculated)) "Precalc" else "Runtime"
    map <- list(
        Runtime     = FeatureAnalysisInputsDataPreparer,
        Precalc     = PreCalculatedFeatureAnalysisInputsPreparer
    )
    cls <- resolve_class(map, type, "Preparer")
    cls$new(analysis_config = analysis_config, ...)
}


FeatureAnalysisInputsRunner <- R6Class(
    "FeatureAnalysisInputsRunner",
    active = list(
        Study = function(value) {
            if (missing(value)) {
                return(
                    self$data_source$Study
                )
            } else {
                self$data_source$Study <- value
            }
        },
        Studies = function(value) {
            return(
                self$data_source$Studies
            )
        },
        StudyLabel = function(value) {
            return(
                self$data_source$StudyLabel
            )
        },
        Karyotypes = function(value) {
            return(
                self$data_source$Karyotypes
            )
        },
        Sexes = function(value) {
            return(
                self$data_source$Sexes
            )
        },
        Ages = function(value) {
            return(
                self$data_source$Ages
            )
        },
        StatTestNames = function(value) {
            return(
                self$data_source$StatTestNames
            )
        },
        StatTestValues = function(value) {
            return(
                self$data_source$StatTestValues
            )
        },
        CovariateChoices = function(value) {
            return(
                self$data_source$CovariateChoices
            )
        },
        AdjustmentMethodNames = function(value) {
            return(
                self$data_source$AdjustmentMethodNames
            )
        }, AdjustmentMethodValues = function(value) {
            return(
                self$data_source$AdjustmentMethodValues
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
        get_study_data = function(...) {
            self$data_source$StudyData |>
                self$preparer$prepare(...)
        },
        addInputSpecialClass = function(input_name, class_name) {
            self$data_source$addInputSpecialClass(input_name, class_name)
        }
    )
)

#' @export
getFeatureAnalysisInputs <- function(
        app_config,
        analysis_config,
        input_config,
        ...
    ) {

    precalculated <- analysis_config$UsesPreCalculatedData
    analysis_type <- analysis_config$AnalysisType
    data_src   <- getDataSource(precalculated,  app_config, analysis_type, analysis_config, input_config, ...)
    preparer   <- getPreparer(precalculated, analysis_type, analysis_config, ...)

    FeatureAnalysisInputsRunner$new(
        precalculated,
        analysis_type,
        data_src,
        preparer
    )
}
