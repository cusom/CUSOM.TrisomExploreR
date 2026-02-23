box::use(
    dplyr[if_else],
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/shared/global_utils[`%||%`],
    app/logic/feature_analysis/inputs/InputsDataManagers[
        InputsManagerKaryotype,
        InputsManagerSex,
        InputsManagerAge,
        InputsManagerComorbidity,
        InputsManagerBMI,
        InputsManagerCellTypes
    ],
    app/logic/feature_analysis/inputs/InputsDataPreparers[
        FeatureAnalysisInputsDataPreparer,
        FeatureAnalysisInputsComorbidityDataPreparer,
        PreCalculatedFeatureAnalysisInputsPreparer
    ]
)

box::use(
    R6[R6Class],
    dplyr[between, filter, mutate, select, distinct, arrange, summarise, pull],
    stringr[str_split, str_c],
    tibble[tibble]
)

getDataSource <- function(app_config, analysis_type, analysis_config, input_config, ...) {
    type <- analysis_config$AnalysisVariableName
    map <- list(
        Karyotype =             InputsManagerKaryotype,
        Sex =                   InputsManagerSex,
        Age =                   InputsManagerAge,
        HasAnyConditionFlag =   InputsManagerComorbidity,
        BMI =                   InputsManagerBMI,
        CellTypes =             InputsManagerCellTypes
    )
    cls <- resolve_class(map, type, "DataSource")
    cls$new(
        app_config = app_config,
        analysis_config = analysis_config,
        input_config = input_config,
        ...
    )
}

getPreparer <- function(precalculated, analysis_config, app_config, ...) {
    type <- if_else(
        analysis_config$AnalysisVariableName == "HasAnyConditionFlag", "Comorbidity",
        if (isTRUE(precalculated)) "Precalc" else "Runtime"
    )
    map <- list(
        Runtime     = FeatureAnalysisInputsDataPreparer,
        Comorbidity = FeatureAnalysisInputsComorbidityDataPreparer,
        Precalc     = PreCalculatedFeatureAnalysisInputsPreparer
    )
    cls <- resolve_class(map, type, "Preparer")
    cls$new(analysis_config = analysis_config, app_config = app_config, ...)
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
        },
        AdjustmentMethodValues = function(value) {
            return(
                self$data_source$AdjustmentMethodValues
            )
        },
        ConditionChoices = function(value) {
            return(
                self$data_source$ConditionChoices
            )
        },
        fold_change_variable = function() {
            return(
                self$data_source$FoldChangeVar
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
        getConditionTree = function(conditions = NULL) {
            self$data_source$getConditionTree(conditions)
        },
        get_selected_conditions = function(selected_conditions) {
            self$data_source$get_selected_conditions(selected_conditions)
        },
        get_selected_condition_list = function(selected_conditions) {
            self$data_source$get_selected_condition_list(selected_conditions)
        }
        # addInputSpecialClass = function(input_name, class_name) {
        #     self$data_source$addInputSpecialClass(input_name, class_name)
        # }
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
    data_src   <- getDataSource(app_config, analysis_type, analysis_config, input_config, ...)
    preparer   <- getPreparer(precalculated, analysis_config, app_config, ...)

    FeatureAnalysisInputsRunner$new(
        precalculated,
        analysis_type,
        data_src,
        preparer
    )
}
