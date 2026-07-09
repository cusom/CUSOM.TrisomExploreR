box::use(
    R6[R6Class],
    arrow[read_parquet],
    readr[read_csv]
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
        },
        execution_mode = function(value) {
            if (is.null(self$study_plan) || is.null(self$study_plan$execution_mode)) {
                return(NULL)
            }

            self$study_plan$execution_mode
        }
    ),
    public = list(
        study = NULL,
        study_data = NULL,
        study_plan = NULL,
        stat_test = NULL,
        covariates = NULL,
        adjustment_method = NULL,
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method, study_plan = NULL) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            private$remote_db <- app_config$remote_db
            self$study <- study
            self$study_data <- study_data
            self$study_plan <- study_plan
            self$stat_test <- stat_test
            self$covariates <- covariates
            self$adjustment_method <- adjustment_method
        },
        load_precalculated_artifact = function() {
            if (is.null(self$study_plan) || !identical(self$execution_mode, "precalculated")) {
                return(NULL)
            }

            artifact <- self$study_plan$precalculated_artifact
            package_id <- self$study_plan$package_id

            if (is.null(artifact) || !nzchar(artifact) || is.null(package_id) || !nzchar(package_id)) {
                return(NULL)
            }

            package_root <- private$app_config$package_resolver$packages_root
            artifact_path <- file.path(package_root, package_id, artifact)

            if (!file.exists(artifact_path)) {
                return(NULL)
            }

            parquet_attempt <- tryCatch(read_parquet(artifact_path), error = function(e) NULL)
            if (!is.null(parquet_attempt)) {
                return(parquet_attempt)
            }

            csv_attempt <- tryCatch(
                read_csv(artifact_path, show_col_types = FALSE, progress = FALSE),
                error = function(e) NULL
            )

            csv_attempt
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
    public = list(
        get_data = function(source_data) {
            return(invisible(self$study_data))
        }
    )
)

#' @export
PreCalculatedSummaryDataSource <- R6Class(
    "PreCalculatedSummaryDataSource",
    inherit = SummaryDataSourceBase,
    public = list(
        get_data = function(study_data) {
            if (identical(self$execution_mode, "generated")) {
                return(invisible(self$study_data))
            }

            precalc_data <- self$load_precalculated_artifact()
            if (!is.null(precalc_data)) {
                self$study_data <- precalc_data
                return(invisible(self$study_data))
            }

            return(invisible(self$study_data))
        }
    )
)

#' @export
CorrelatesSummaryDataSource <- R6Class(
    "CorrelatesSummaryDataSource",
    inherit = SummaryDataSourceBase,
    public = list(
        get_data = function(study_data) {
            return(invisible(self$study_data))
        }
    )
)
