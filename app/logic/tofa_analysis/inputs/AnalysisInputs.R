box::use(
    R6[R6Class],
    dplyr[between, filter, mutate, select, distinct, arrange, summarise, pull, case_when],
    stringr[str_split, str_c],
    tibble[tibble]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/tofa_analysis/inputs/AnalysisInputsDataManagers[EndpointsInputsManager, NULISAInputsManager, OLINKInputsManager],
    app/logic/tofa_analysis/inputs/AnalysisInputsDataPreparers[AnalysisInputsDataPreparer],
)

getDataSource <- function(analysis_config, dataset, ...) {
    dataset_type <- case_when(
        grepl("Endpoints", dataset, ignore.case = TRUE) ~ "endpoints",
        grepl("Nulisa", dataset, ignore.case = TRUE) ~ "nulisa",
        grepl("Olink", dataset, ignore.case = TRUE) ~ "olink",
        TRUE ~ "other"
    )
    map <- list(
        endpoints = EndpointsInputsManager,
        nulisa = NULISAInputsManager,
        olink = OLINKInputsManager
    )
    cls <-  resolve_class(map, dataset_type, "DataSource") 

    cls$new(analysis_config, dataset, ...)
}

getPreparer <- function(analysis_config, dataset, ...) {
    dataset_type <- case_when(
        grepl("Endpoints", dataset, ignore.case = TRUE) ~ "endpoints",
        grepl("Nulisa", dataset, ignore.case = TRUE) ~ "nulisa",
        grepl("Olink", dataset, ignore.case = TRUE) ~ "olink",
        TRUE ~ "other"
    )
    map <- list(
        endpoints = AnalysisInputsDataPreparer,
        nulisa = AnalysisInputsDataPreparer,
        olink = AnalysisInputsDataPreparer
    )
    map <- list(
        endpoints = AnalysisInputsDataPreparer,
        nulisa = AnalysisInputsDataPreparer,
        olink = AnalysisInputsDataPreparer
    )
    cls <-  resolve_class(map, dataset_type, "Preparer") 
    cls$new(analysis_config, dataset, ...)
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
        karyotype = function(value) {
            return(self$data_source$karyotype)
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
        },
        stat_test_names = function(value) {
            return(
                self$data_source$stat_test_names
            )
        },
        stat_test_values = function(value) {
            return(
                self$data_source$stat_test_values
            )
        },
        covariate_choices = function(value) {
            return(
                self$data_source$covariate_choices
            )
        },
        adjustment_method_names = function(value) {
            return(
                self$data_source$adjustment_method_names
            )
        },
        adjustment_method_values = function(value) {
            return(
                self$data_source$adjustment_method_values
            )
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
        analysis_config,
        dataset, 
        ...
    ) {

    data_src   <- getDataSource(analysis_config, dataset, ...)
    preparer   <- getPreparer(analysis_config, dataset, ...)

    AnalysisInputsRunner$new(
        data_src,
        preparer
    )
}