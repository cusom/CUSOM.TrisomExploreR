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
        remote_db = NULL,
        normalize_statistic_id = function(stat_test) {
            if (is.null(stat_test) || !nzchar(stat_test)) {
                return("linear_model")
            }

            mapping <- c(
                "Linear Model" = "linear_model",
                "linear model" = "linear_model",
                "Wilcoxon test" = "wilcoxon"
            )

            mapped <- mapping[[stat_test]]
            if (!is.null(mapped)) {
                return(mapped)
            }

            tolower(gsub("[^a-zA-Z0-9]+", "_", stat_test))
        },
        load_artifact_data = function(package_id, artifact_rel_path) {
            if (is.null(artifact_rel_path) || !nzchar(artifact_rel_path) || is.null(package_id) || !nzchar(package_id)) {
                return(NULL)
            }

            package_root <- private$app_config$package_resolver$packages_root
            artifact_path <- file.path(package_root, package_id, artifact_rel_path)

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
        }
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
            if (!is.null(self$study_plan)) {
                plan_data <- private$load_artifact_data(
                    package_id = self$study_plan$package_id,
                    artifact_rel_path = self$study_plan$precalculated_artifact
                )

                if (!is.null(plan_data)) {
                    return(plan_data)
                }
            }

            if (is.null(self$study) || !nzchar(self$study)) {
                return(NULL)
            }

            dataset_def <- tryCatch(
                private$app_config$get_catalog_dataset_definition(self$study),
                error = function(e) NULL
            )

            if (is.null(dataset_def)) {
                return(NULL)
            }

            package_id <- dataset_def$package %||% dataset_def$id

            if (is.null(package_id) || !nzchar(package_id)) {
                return(NULL)
            }

            feature_id <- tolower(private$analysis_config$Namespace %||% "")
            statistic_id <- private$normalize_statistic_id(self$stat_test)

            artifact_rel_path <- tryCatch(
                private$app_config$package_resolver$resolve_precalculated_artifact(
                    package_id = package_id,
                    statistic_id = statistic_id,
                    feature_id = feature_id
                ),
                error = function(e) NULL
            )

            private$load_artifact_data(package_id, artifact_rel_path)
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
PreCalculatedSummaryDataSource <- R6Class(
    "PreCalculatedSummaryDataSource",
    inherit = SummaryDataSourceBase,
    public = list(
        get_data = function(study_data) {
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
