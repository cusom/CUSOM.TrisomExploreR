box::use(
    R6[R6Class],
    dplyr[select, filter, mutate, case_when, add_count, ungroup,
        inner_join, left_join, rename, distinct, arrange, pull, join_by],
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
        remote_db = NULL,
        load_package_dimension = function(package_id, dimension_name) {
            rel_path <- private$app_config$package_resolver$resolve_dimension_file(package_id, dimension_name)

            if (is.null(rel_path) || !nzchar(rel_path)) {
                return(NULL)
            }

            tryCatch(
                private$app_config$load_local_package_artifact(package_id, rel_path),
                error = function(e) NULL
            )
        },
        resolve_analyte_ids = function(package_id, analyte) {
            analytes_dim <- private$load_package_dimension(package_id, "analytes")

            if (is.null(analytes_dim) || nrow(analytes_dim) == 0) {
                return(character(0))
            }

            ids <- character(0)
            id_col <- intersect(c("AnalyteID", "AnalyteKey"), names(analytes_dim))
            name_col <- intersect(c("Analyte", "AnalyteName", "Gene", "Gene_name", "Feature"), names(analytes_dim))

            if (length(id_col) == 0) {
                return(character(0))
            }

            if (length(name_col) > 0) {
                mapped <- analytes_dim[as.character(analytes_dim[[name_col[[1]]]]) %in% analyte, , drop = FALSE]
                if (nrow(mapped) > 0) {
                    ids <- c(ids, as.character(mapped[[id_col[[1]]]]))
                }
            }

            ids <- c(ids, analyte)
            ids <- unique(ids[!is.na(ids) & nzchar(ids)])
            ids
        },
        load_local_fact_analyte_rows = function(package_id, analyte, fact_name = NULL) {
            fact_rel_path <- private$app_config$package_resolver$resolve_fact_file(package_id, fact_name)

            if (is.null(fact_rel_path) || !nzchar(fact_rel_path)) {
                return(NULL)
            }

            analyte_ids <- private$resolve_analyte_ids(package_id, analyte)

            facts <- tryCatch(
                private$app_config$load_local_package_artifact(
                    package_id,
                    fact_rel_path,
                    analyte_id = analyte_ids
                ),
                error = function(e) NULL
            )

            if (is.null(facts)) {
                return(NULL)
            }

            participants <- private$load_package_dimension(package_id, "participants")
            visits <- private$load_package_dimension(package_id, "visits")
            analytes <- private$load_package_dimension(package_id, "analytes")

            enriched <- facts

            if (!is.null(participants) && "record_id" %in% names(enriched) && "record_id" %in% names(participants)) {
                participant_cols <- intersect(c("record_id", "Sex", "Karyotype"), names(participants))
                if (length(participant_cols) > 1) {
                    enriched <- enriched |>
                        left_join(
                            participants |>
                                select(all_of(participant_cols)) |>
                                distinct(),
                            by = "record_id"
                        )
                }
            }

            if (!is.null(visits) && "LabID" %in% names(enriched) && "LabID" %in% names(visits)) {
                visit_cols <- intersect(c("LabID", "Age", "BMI", "AgeAtTimeOfVisit", "BMIAtTimeOfVisit"), names(visits))
                if (length(visit_cols) > 1) {
                    enriched <- enriched |>
                        left_join(
                            visits |>
                                select(all_of(visit_cols)) |>
                                distinct(),
                            by = "LabID"
                        )
                }

                if (!"Age" %in% names(enriched) && "AgeAtTimeOfVisit" %in% names(enriched)) {
                    enriched$Age <- enriched$AgeAtTimeOfVisit
                }

                if (!"BMI" %in% names(enriched) && "BMIAtTimeOfVisit" %in% names(enriched)) {
                    enriched$BMI <- enriched$BMIAtTimeOfVisit
                }
            }

            if (!is.null(analytes)) {
                id_col <- NULL
                if ("AnalyteID" %in% names(enriched) && "AnalyteID" %in% names(analytes)) {
                    id_col <- "AnalyteID"
                } else if ("AnalyteKey" %in% names(enriched) && "AnalyteKey" %in% names(analytes)) {
                    id_col <- "AnalyteKey"
                }

                name_col <- intersect(c("Analyte", "AnalyteName", "Gene", "Gene_name", "Feature"), names(analytes))

                if (!is.null(id_col) && length(name_col) > 0) {
                    analyte_map <- analytes |>
                        select(all_of(c(id_col, name_col[[1]]))) |>
                        distinct()

                    names(analyte_map)[names(analyte_map) == name_col[[1]]] <- "Analyte"
                    enriched <- enriched |>
                        left_join(analyte_map, by = id_col)
                }
            }

            if (!"Analyte" %in% names(enriched)) {
                analyte_alias <- intersect(c("AnalyteName", "Gene", "Gene_name", "Feature", "feature", "analyte"), names(enriched))

                if (length(analyte_alias) > 0) {
                    names(enriched)[names(enriched) == analyte_alias[[1]]] <- "Analyte"
                }
            }

            if ("Analyte" %in% names(enriched)) {
                enriched <- enriched |>
                    filter(Analyte %in% analyte)
            }

            selected_groups <- self$selected_karyotypes

            if (!is.null(selected_groups) && length(selected_groups) > 0 && "Karyotype" %in% names(enriched)) {
                selected_groups <- as.character(selected_groups)
                selected_groups <- selected_groups[!is.na(selected_groups) & nzchar(selected_groups)]

                if (length(selected_groups) > 0) {
                    selected_groups <- unlist(str_split(selected_groups, pattern = ";", simplify = FALSE), use.names = FALSE)
                    selected_groups <- trimws(selected_groups)
                    selected_groups <- unique(selected_groups[nzchar(selected_groups)])

                    if (length(selected_groups) > 0) {
                        enriched <- enriched |>
                            filter(as.character(Karyotype) %in% selected_groups)
                    }
                }
            }

            enriched
        }
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
        selected_karyotypes = NULL,
        analyte = NULL,
        analyte_data = NULL,
        summary_data = NULL,
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data, study_plan = NULL, selected_karyotypes = NULL) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            private$remote_db <- app_config$remote_db
            self$study <- study
            self$study_data <- study_data
            self$study_plan <- study_plan
            self$selected_karyotypes <- selected_karyotypes
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

            package_root <- private$app_config$package_resolver$get_package_root(package_id)
            artifact_path <- file.path(package_root, artifact)

            if (!(file.exists(artifact_path) || dir.exists(artifact_path))) {
                return(NULL)
            }

            tryCatch(
                private$app_config$load_local_package_artifact(
                    package_id,
                    artifact,
                    feature_id = self$study_plan$feature_id
                ),
                error = function(e) NULL
            )
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
            if (!is.null(self$study_plan) && !is.null(self$study_plan$package_id) && nzchar(self$study_plan$package_id)) {
                planned_fact_name <- self$study_plan$fact_name

                if (is.null(planned_fact_name) || !nzchar(planned_fact_name)) {
                    planned_fact_name <- NULL
                }

                fact_data <- private$load_local_fact_analyte_rows(
                    self$study_plan$package_id,
                    analyte,
                    fact_name = planned_fact_name
                )

                if (!is.null(fact_data)) {
                    self$analyte_data <- fact_data
                    return(invisible(self$analyte_data))
                }
            }

            precalc_data <- self$load_precalculated_artifact()
            if (!is.null(precalc_data)) {
                analyte_col <- intersect(c("Analyte", "AnalyteName", "Feature", "analyte", "feature"), names(precalc_data))

                if (length(analyte_col) > 0) {
                    selected <- precalc_data[precalc_data[[analyte_col[[1]]]] %in% analyte, , drop = FALSE]
                    if (analyte_col[[1]] != "Analyte") {
                        names(selected)[names(selected) == analyte_col[[1]]] <- "Analyte"
                    }
                    self$analyte_data <- selected
                } else {
                    self$analyte_data <- precalc_data
                }

                # Single-analyte plot preparers require factual columns.
                if (!all(c("MeasuredValue", "Measurement") %in% names(self$analyte_data))) {
                    self$analyte_data <- NULL
                }

                if (!is.null(self$analyte_data)) {
                    return(invisible(self$analyte_data))
                }
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
            study_plan = NULL,
            ...
        ) {
            super$initialize(
                analysis_config,
                app_config,
                study,
                study_data,
                analyte,
                summary_data,
                study_plan = study_plan,
                ...
            )
            self$dataset <- study
            self$comparison <- comparison
        },
        get_single_data = function(analyte) {
            source <- self$source_data

            if (!"Analyte" %in% names(source) && "AnalyteID" %in% names(source)) {
                package_id <- NULL

                if (!is.null(self$study_plan) && !is.null(self$study_plan$package_id) && nzchar(self$study_plan$package_id)) {
                    package_id <- self$study_plan$package_id
                } else {
                    dataset_def <- tryCatch(
                        private$app_config$get_catalog_dataset_definition(self$dataset),
                        error = function(e) NULL
                    )

                    if (!is.null(dataset_def)) {
                        package_id <- dataset_def$package
                        if (is.null(package_id) || !nzchar(package_id)) {
                            package_id <- dataset_def$id
                        }
                    }
                }

                if (!is.null(package_id) && nzchar(package_id)) {
                    analyte_rel_path <- private$app_config$package_resolver$resolve_dimension_file(
                        package_id,
                        "analytes"
                    )

                    if (!is.null(analyte_rel_path) && nzchar(analyte_rel_path)) {
                        analytes_dim <- tryCatch(
                            private$app_config$load_local_package_artifact(package_id, analyte_rel_path),
                            error = function(e) NULL
                        )

                        if (!is.null(analytes_dim) && "AnalyteID" %in% names(analytes_dim)) {
                            analyte_name_col <- intersect(
                                c("Analyte", "AnalyteName", "Gene", "Gene_name", "Feature"),
                                names(analytes_dim)
                            )

                            if (length(analyte_name_col) > 0) {
                                analyte_map <- analytes_dim |>
                                    select(all_of(c("AnalyteID", analyte_name_col[[1]]))) |>
                                    distinct()

                                names(analyte_map)[names(analyte_map) == analyte_name_col[[1]]] <- "Analyte"

                                source <- source |>
                                    left_join(analyte_map, by = "AnalyteID")
                            }
                        }
                    }
                }
            }

            self$analyte_data <- source |>
                filter(
                    Analyte == analyte,
                    Event_Name %in% str_split_1(self$comparison(),"\\|")
                ) |>
                rename( 
                    "LabID" = TOFA_LabID,
                    "Analyte" = Analyte,
                    "MeasuredValue" = Value,
                    "Measurement" = Units
                ) |>
                mutate(MeasuredValue = as.numeric(MeasuredValue)) 
            return(invisible(self$analyte_data))
        }
    )
)
