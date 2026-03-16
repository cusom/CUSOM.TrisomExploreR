box::use(
    R6[R6Class],
    dplyr[between, filter, mutate, select, distinct, arrange, summarise, pull],
    stringr[str_split, str_c],
    tibble[tibble]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/tofa_analysis/inputs/AnalysisInputsDataManagers[AnalysisInputsManager],
    app/logic/tofa_analysis/inputs/AnalysisInputsDataPreparers[AnalysisInputsDataPreparer],
)

getDataSource <- function(participant_data, visit_data, dataset_data, ...) {
    type <- "base"
    map <- list(
        "base" = AnalysisInputsManager
    )
    cls <-  resolve_class(map, type, "DataSource") 

    cls$new(participant_data, visit_data, dataset_data, ...)
}

getPreparer <- function(dataset_data) {
    type <- "base"
    map <- list(
        "base" = AnalysisInputsDataPreparer
    )
    cls <-  resolve_class(map, type, "Preparer") 
    cls$new(dataset_data)
}

AnalysisInputsRunner <- R6Class(
    "FeatureAnalysisInputsRunner",
    active = list(
        sexes = function(value) {
            return(self$data_source$sexes)
        },
        races = function(value) {
            return(self$data_source$races)
        },
        ethnicities = function(value) {
            return(self$data_source$ethnicities)
        },
        down_syndrome_status = function(value) {
            return(self$data_source$down_syndrome_status)
        },
        events = function(value) {
            return(self$data_source$events)
        },
        event_comparisons = function(value) {
            return(self$data_source$event_comparisons)
        },
        age_at_visit = function(value) {
            return(self$data_source$age_at_visit)
        },
        age_groups = function(value) {
            return(self$data_source$age_groups)
        },
        conditions = function(value) {
            return(self$data_source$conditions)
        },
        features = function(value) {
            return(self$data_source$features)
        }
    ),
    public = list(
        data_source = NULL,
        preparer = NULL,
        initialize = function(
            data_source,
            preparer
        ) {
            self$data_source <- data_source
            self$preparer <- preparer
        },
        get_data = function(
            sexes, races, ethnicities, down_syndrome_status, age_at_visit, age_group, conditions
        ) {
            data <- self$data_source$get_data(
                sexes, races, ethnicities, down_syndrome_status, age_at_visit, age_group, conditions
            )
            return(self$preparer$prepare(data))
        }
    )
)

#' @export
getAnalysisInputs <- function(
        participant_data,
        visit_data,
        dataset_data,
        ...
    ) {

    data_src   <- getDataSource(participant_data, visit_data, dataset_data, ...)
    preparer   <- getPreparer(dataset_data)

    AnalysisInputsRunner$new(
        data_src,
        preparer
    )
}