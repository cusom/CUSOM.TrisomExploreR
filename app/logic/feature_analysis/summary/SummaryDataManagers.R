box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, mutate, group_by, summarise, ungroup, rename_with, distinct, n,
            pull, arrange, dense_rank, row_number],
    purrr[pmap],
    stringr[str_split_1],
    rlang[sym],
)

SummaryDataSourceBase <- R6Class(
    "SummaryDataSourceBase",
    private = list(
        app_config = NULL,
        analysis_config = NULL,
        remote_db = NULL
    ),
    active = list(
        adjusted = function(value) {
            if (missing(value)) {
                return(self$adjustment_method != "none")
            }
        }
    ),
    public = list(
        study = NULL,
        study_data = NULL,
        stat_test = NULL,
        covariates = NULL,
        adjustment_method = NULL,
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            private$remote_db <- app_config$remote_db
            self$study <- study
            self$study_data <- study_data
            self$stat_test <- stat_test
            self$covariates <- covariates
            self$adjustment_method <- adjustment_method
        },
        get_data = function(source_data) {
            stop("Abstract: must implement")
        }
    )
)

#' @export
RuntimeSummaryDataSource <- R6Class(
    "RuntimeSummaryDataSource",
    inherit = SummaryDataSourceBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            super$initialize(analysis_config, app_config, study, study_data,
                stat_test, covariates, adjustment_method)
        },
        get_data = function(source_data) {
            return(invisible(self$study_data))
        }
    )
)

#' @export
PreCalculatedSummaryDataSource <- R6Class(
    "PreCalculatedSummaryDataSource",
    inherit = SummaryDataSourceBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            super$initialize(analysis_config, app_config, study, study_data,
                stat_test, covariates, adjustment_method)
        },
        get_data = function(study_data) {
            return(invisible(self$study_data))
        }
    )
)

#' @export
CorrelatesSummaryDataSource <- R6Class(
    "CorrelatesSummaryDataSource",
    inherit = SummaryDataSourceBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            super$initialize(analysis_config, app_config, study, study_data,
                stat_test, covariates, adjustment_method)
        },
        get_data = function(study_data) {
            return(invisible(self$study_data))
        }
    )
)
