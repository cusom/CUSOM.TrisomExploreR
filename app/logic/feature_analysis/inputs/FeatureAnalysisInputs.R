box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/feature_analysis/inputs/InputsDataManagers[
        InputsManagerKaryotype,
        InputsManagerPrecalculatedKaryotype,
        InputsManagerSex,
        InputsManagerAge,
        InputsManagerComorbidity,
        InputsManagerBMI,
        InputsManagerCellTypes,
        InputsManagerTOFA
    ],
    app/logic/feature_analysis/inputs/InputsDataPreparers[
        FeatureAnalysisInputsDataPreparer,
        FeatureAnalysisInputsComorbidityDataPreparer,
        PreCalculatedFeatureAnalysisInputsPreparer,
        TOFAAnalysisInputsDataPreparer
    ]
)

box::use(
    R6[R6Class],
    dplyr[filter, case_when]
)

DATA_SOURCE_MAP <- list(
    Karyotype = InputsManagerKaryotype,
    PrecalcKaryotype = InputsManagerPrecalculatedKaryotype,
    Sex = InputsManagerSex,
    Age = InputsManagerAge,
    HasAnyConditionFlag = InputsManagerComorbidity,
    BMI = InputsManagerBMI,
    CellTypes = InputsManagerCellTypes,
    Event_Name = InputsManagerTOFA,
    PrecalcEvent_Name = InputsManagerTOFA
)

PREPARER_MAP <- list(
    Runtime = FeatureAnalysisInputsDataPreparer,
    Comorbidity = FeatureAnalysisInputsComorbidityDataPreparer,
    Precalc = PreCalculatedFeatureAnalysisInputsPreparer,
    TOFA = TOFAAnalysisInputsDataPreparer
)

getRouteProfile <- function(precalculated, analysis_config) {
    analysis_variable <- analysis_config$AnalysisVariableName

    data_source_key <- paste0(
        if (isTRUE(precalculated)) "Precalc" else "",
        analysis_variable
    )

    preparer_key <- case_when(
        analysis_variable == "HasAnyConditionFlag" ~ "Comorbidity",
        analysis_variable == "Event_Name" ~ "TOFA",
        isTRUE(precalculated) ~ "Precalc",
        TRUE ~ "Runtime"
    )

    list(
        data_source_key = data_source_key,
        preparer_key = preparer_key
    )
}

instantiateMappedClass <- function(map, key, kind, ...) {
    cls <- resolve_class(map, key, kind)
    cls$new(...)
}

getDataSource <- function(route_profile, app_config, analysis_config, input_config, ...) {
    instantiateMappedClass(
        map = DATA_SOURCE_MAP,
        key = route_profile$data_source_key,
        kind = "DataSource",
        app_config = app_config,
        analysis_config = analysis_config,
        input_config = input_config,
        ...
    )
}

getPreparer <- function(route_profile, analysis_config, app_config, ...) {
    instantiateMappedClass(
        map = PREPARER_MAP,
        key = route_profile$preparer_key,
        kind = "Preparer",
        analysis_config = analysis_config,
        app_config = app_config,
        ...
    )
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
        Age_Groups = function(value) {
            return(
                self$data_source$Age_Groups
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
        },
        StudyPlan = function(value) {
            return(
                self$data_source$CurrentPlan
            )
        },
        event_comparisons = function(value) {
            return(self$data_source$event_comparisons)
        },
        baseline_comparisons = function(value) {
            return(
                self$data_source$event_comparisons |>
                    filter(grepl("Baseline vs", analysis))
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
            args <- list(...)
            data_source_fields <- names(self$data_source)

            # Keep datasource state in sync with the triggering inputs so the
            # first Analyze click uses the current selections.
            if ("Study" %in% data_source_fields && !is.null(args$study) && nzchar(args$study)) {
                self$data_source$Study <- args$study
            }

            if ("StatTest" %in% data_source_fields && !is.null(args$stat_test) && nzchar(args$stat_test)) {
                self$data_source$StatTest <- args$stat_test
            }

            if ("Covariates" %in% data_source_fields && !is.null(args$covariates)) {
                self$data_source$Covariates <- args$covariates
            }

            if ("AdjustmentMethod" %in% data_source_fields && !is.null(args$adjustment_method) && nzchar(args$adjustment_method)) {
                self$data_source$AdjustmentMethod <- args$adjustment_method
            }

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

    precalculated <- FALSE
    route_profile <- getRouteProfile(precalculated, analysis_config)
    data_src <- getDataSource(route_profile, app_config, analysis_config, input_config, ...)
    preparer <- getPreparer(route_profile, analysis_config, app_config, ...)

    FeatureAnalysisInputsRunner$new(
        precalculated,
        analysis_config$AnalysisType,
        data_src,
        preparer
    )
}
