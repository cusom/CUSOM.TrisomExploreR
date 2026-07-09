box::use(
    R6[R6Class],
    arrow[read_parquet],
    readr[read_csv],
    dplyr[select, filter, mutate, case_when, add_count, ungroup,
        inner_join, rename, distinct, arrange, pull, join_by],
    tibble[tibble],
    tidyr[separate_rows],
    forcats[fct_inorder],
    glue[glue],
    rlang[sym],
    stringr[str_split, str_split_1]
)

AnalyteDataSourceBase <- R6Class(
    "AnalyteDataSourceBase",
    private = list(
        app_config = NULL,
        analysis_config = NULL,
        remote_db = NULL
    ),
    active = list(
        analysis_mode = function(value) {
            return(
                if (length(self$analyte) == 1) "single" else "multi"
            )
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
        analyte = NULL,
        analyte_data = NULL,
        summary_data = NULL,
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data, study_plan = NULL) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            private$remote_db <- app_config$remote_db
            self$study <- study
            self$study_data <- study_data
            self$study_plan <- study_plan
            self$analyte <- analyte
            self$summary_data <- summary_data
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
        get_data = function(analyte) {
            if (self$analysis_mode == "single") {
                self$get_single_data(analyte)
            } else {
                self$get_multi_data(analyte)
            }
        },
        get_single_data = function(analyte) {
            stop("Abstract: must implement")
        },
        get_multi_data = function(analyte) {
            self$analyte_data <- self$summary_data |>
                filter(Analyte %in% analyte)
            return(invisible(self$analyte_data))
        }
    )
)

#' @export
RuntimeAnalyteDataSource <- R6Class(
    "RuntimeAnalyteDataSource",
    inherit = AnalyteDataSourceBase,
    public = list(
        get_single_data = function(analyte) {
            self$analyte_data <- self$study_data |>
                filter(Analyte == analyte)
            return(invisible(self$analyte_data))
        }
    )
)

#' @export
PreCalcualtedAnalyteDataSource <- R6Class(
    "PreCalcualtedAnalyteDataSource",
    inherit = AnalyteDataSourceBase,
    active = list(
        age = function(value) {
            return(
                self$study_data |>
                    distinct(ages) |>
                    separate_rows(ages, sep = ";") |>
                    rename("Age" = ages) |>
                    mutate(Age = as.integer(Age))
            )
        },
        sex = function(value) {
            self$study_data |>
                distinct(sexes) |>
                separate_rows(sexes, sep = ";") |>
                rename("Sex" = sexes)
        },
        karyotype = function(value) {
            self$study_data |>
                distinct(karyotypes) |>
                separate_rows(karyotypes, sep = ";") |>
                rename("Karyotype" = karyotypes)
        },
        analysis_var = function(value) {
            return(
                sym(self$analysisVariable)
            )
        },
        age_min = function(value)  {
            return(
                min(self$age, na.rm = TRUE)
            )
        },
        age_max = function(value) {
            return(
                max(self$age, na.rm = TRUE)
            )
        }
    ),
    public = list(
        get_single_data = function(analyte) {
            if (identical(self$execution_mode, "generated")) {
                self$analyte_data <- self$study_data |>
                    filter(Analyte == analyte)
                return(invisible(self$analyte_data))
            }

            precalc_data <- self$load_precalculated_artifact()
            if (!is.null(precalc_data)) {
                analyte_col <- intersect(c("Analyte", "Feature", "analyte", "feature"), names(precalc_data))

                if (length(analyte_col) > 0) {
                    selected <- precalc_data[precalc_data[[analyte_col[[1]]]] %in% analyte, , drop = FALSE]
                    if (analyte_col[[1]] != "Analyte") {
                        names(selected)[names(selected) == analyte_col[[1]]] <- "Analyte"
                    }
                    self$analyte_data <- selected
                } else {
                    self$analyte_data <- precalc_data
                }

                return(invisible(self$analyte_data))
            }

            self$analyte_data <- private$remote_db$getQuery(
                    "EXEC [shiny].[GetDataByExperimentAnalyte] ?, ?",
                    tibble(StudyName = self$study, Analyte = analyte)
                ) |>
                rename(record_id = Record_ID) |>
                inner_join(
                    private$remote_db$getQuery(
                        "EXEC [shiny].[GetParticipantsByExperiment] ?",
                        tibble(StudyName = self$study)
                    ),
                    by = "record_id"
                ) |>
                rename(Sex = Gender) |>
                inner_join(
                    private$remote_db$getQuery(
                        "EXEC [shiny].[GetParticipantEncounterByExperiment] ?",
                        tibble(StudyName = self$study)
                    ),
                    by = c("LabID", "record_id")
                ) |>
                rename(Age = AgeAtTimeOfVisit) |>
                inner_join(self$sex, by = "Sex") |>
                inner_join(self$karyotype, by = "Karyotype")
            return(invisible(self$analyte_data))
        }
    )
)

#' @export
CorrelatesAnalyteDataSource <- R6Class(
    "CorrelatesAnalyteDataSource",
    inherit = AnalyteDataSourceBase,
    active = list(

        CompareExperiment = function(value) {
            if (missing(value)) {
                return(
                    self$study_data |>
                        distinct(ComparisonExperimentID) |>
                        pull()
                    )
            }
        },
        ComparisonAnalyteKey = function(value) {
            return(
                self$analyte
            )
        },
        QueryExperiment = function(value) {
            return(
                self$study_data |>
                    distinct(QueryExperimentID) |>
                    pull()
            )
        },
        QueryAnalyte = function(value) {
            return(
                self$study_data |>
                    distinct(QueryAnalyteKey) |>
                    pull() |>
                    as.integer()
            )
        }
    ),
    public = list(
        get_data = function(analyte) {
            if (length(analyte) > 1) {
                return(self$get_multi_data(analyte))
            } else {
                return(self$get_single_data(analyte))
            }
        },
        get_single_data = function(analyte) {
            self$analyte_data <- private$remote_db$getQuery(
                "[shiny].[GetAnalyteDataByExperiment] ?, ?",
                tibble(
                    "ExperimentID" =  self$CompareExperiment,
                    "Analyte" = self$ComparisonAnalyteKey
                )
            ) |>
            filter(outlier == FALSE) |>
            select(LabID, "ComparisonAnalyte" = Analyte,  MeasuredValue, Measurement) |>
            rename(y = MeasuredValue) |>
            inner_join(
                private$remote_db$getQuery(
                    "[shiny].[GetAnalyteDataByExperiment] ?, ?",
                    tibble(
                        "ExperimentID" = self$QueryExperiment,
                        "Analyte" = self$QueryAnalyte
                    )
                ) |>
                    filter(outlier == FALSE) |>
                    select(LabID, "QueryAnalyte" = Analyte, MeasuredValue, Measurement) |>
                    rename(x = MeasuredValue)
                , by = "LabID"
            )
            return(invisible(self$analyte_data))
        },
        get_multi_data = function(analyte) {
            self$analyte_data <- self$summary_data |>
                filter(Analyte %in% analyte) |>
                distinct()
            return(invisible(self$analyte_data))
        }
    )
)

#' @export
PreCalcualtedTOFAAnalyteDataSource <- R6Class(
    "PreCalcualtedTOFAAnalyteDataSource",
    inherit = PreCalcualtedAnalyteDataSource,
    active = list(
        source_data = function(value) {
            private$app_config$get_local_dataset_data(self$dataset)
        }
    ),
    public = list(
        dataset = NULL,
        comparison = NULL,
        initialize = function(
            analysis_config,
            app_config,
            study,
            study_data,
            analyte,
            summary_data,
            comparison,
            study_plan = NULL
        ) {
            super$initialize(
                analysis_config,
                app_config,
                study,
                study_data,
                analyte,
                summary_data,
                study_plan = study_plan
            )
            self$dataset <- study
            self$comparison <- comparison
        },
        get_single_data = function(analyte) {
            self$analyte_data <- self$source_data |>
                filter(
                    Feature == analyte,
                    Event_Name %in% str_split_1(self$comparison(),"\\|")
                ) |>
                rename(
                    "LabID" = TOFA_LabID,
                    "Analyte" = Feature,
                    "MeasuredValue" = Value,
                    "Measurement" = Units
                ) |>
                mutate(MeasuredValue = as.numeric(MeasuredValue)) 
            return(invisible(self$analyte_data))
        }
    )
)
