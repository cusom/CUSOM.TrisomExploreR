box::use(
    app/view/custom_ui/input_widgets[dfToTree],
)

box::use(
    R6[R6Class],
    glue[glue],
    tibble[tibble],
    dplyr[select, mutate, group_by, summarise, distinct,
            pull, arrange, row_number, filter, bind_rows,
            n_distinct, if_else, inner_join, left_join, join_by, cross_join],
    purrr[pluck, set_names],
    stringr[str_split_1, str_c],
    stats[median],
    shinyTree[get_selected]
)

# Helper to create HTML tooltip with info icon
make_tooltip <- function(tooltip_text) {
    glue('<span data-toggle="tooltip" data-placement="auto right" ',
        'class="fas fa-info-circle gtooltip info-tooltip" ',
        'data-original-title="{tooltip_text}"></span>')
}

# Helper to create comparison choice HTML
make_comparison_html <- function(karyo_list, tooltip_text) {
    glue('<div>{str_c(karyo_list, collapse = " vs. ")} {make_tooltip(tooltip_text)}</div>')
}

# Helper to create comparison row for karyotype inputs
make_comparison_row <- function(karyo_list, tooltip_text) {
    tibble(
        Karyotype = str_c(karyo_list, collapse = ";"),
        n = NA,
        sort = 999,
        choiceNames = make_comparison_html(karyo_list, tooltip_text),
        choiceValues = str_c(karyo_list, collapse = ";")
    )
}

make_collapsed_karyotype_choices <- function(karyo_list, tooltip_text) {
    tibble(
        choiceNames = make_comparison_html(karyo_list, tooltip_text),
        choiceValues = str_c(karyo_list, collapse = ";")
    )
}

sanitize_choice_vector <- function(values) {
    values <- as.character(values)
    values <- trimws(values)
    unique(values[!is.na(values) & nzchar(values)])
}

normalize_field_set_label <- function(value, default = "Catalog Datasets") {
    if (is.null(value) || is.na(value) || !nzchar(trimws(value))) {
        value <- default
    }

    value <- trimws(as.character(value))
    value <- gsub("&copy;", "©", value, fixed = TRUE)
    value <- gsub("\\s+", " ", value)

    value
}

# Helper to set shared karyotype sorting/label fields
build_karyotype_choices <- function(.data) {
    .data |>
        mutate(Karyotype = trimws(as.character(Karyotype))) |>
        filter(!is.na(Karyotype), nzchar(Karyotype)) |>
        mutate(
            sort = if_else(Karyotype == "Trisomy 21", 1, 99),
            choiceNames = glue("{Karyotype} (n={n})"),
            choiceValues = Karyotype
        ) |>
        arrange(sort)
}

feature_id_from_analysis_variable <- function(analysis_variable) {
    mapping <- c(
        Karyotype = "karyotype",
        Age = "age",
        Sex = "sex",
        BMI = "bmi",
        HasAnyConditionFlag = "comorbidity"
    )

    mapped <- mapping[[analysis_variable]]
    if (is.null(mapped)) {
        return(tolower(analysis_variable))
    }

    mapped
}

statistic_id_from_ui_label <- function(stat_test) {
    if (is.null(stat_test) || length(stat_test) == 0) {
        return(NULL)
    }

    stat_test <- as.character(stat_test[[1]])
    if (!nzchar(stat_test)) {
        return(NULL)
    }

    mapping <- c(
        "Linear Model" = "linear_model",
        "Wilcoxon test" = "wilcoxon",
        "linear model" = "linear_model"
    )

    mapped <- unname(mapping[stat_test])

    if (length(mapped) == 1 && !is.na(mapped)) {
        return(mapped[[1]])
    }

    tolower(gsub("[^a-zA-Z0-9]+", "_", stat_test))
}

statistic_label_from_id <- function(statistic_id) {
    mapping <- c(
        linear_model = "Linear Model",
        wilcoxon = "Wilcoxon test"
    )

    mapped <- mapping[[statistic_id]]
    if (!is.null(mapped)) {
        return(mapped)
    }

    gsub("_", " ", tools::toTitleCase(statistic_id))
}

#' @export
InputsManagerBase <- R6Class(
    "InputsManagerBase",
    private = list(
        app_config = NULL,
        analysis_config = NULL,
        build_catalog_studies = function(feature_id) {
            datasets <- private$app_config$get_catalog_feature_datasets(feature_id)

            if (nrow(datasets) == 0) {
                return(NULL)
            }

            study_meta <- private$app_config$input_config$studies

            dataset_rows <- lapply(seq_len(nrow(datasets)), function(i) {
                dataset_id <- datasets$dataset_id[[i]]
                package_id <- datasets$package_id[[i]]

                dataset_def <- private$app_config$get_catalog_dataset_definition(dataset_id)
                if (is.null(package_id) || is.na(package_id) || !nzchar(package_id)) {
                    package_id <- dataset_def$package %||% dataset_def$id
                }

                manifest <- tryCatch(
                    private$app_config$package_resolver$get_package_manifest(package_id),
                    error = function(e) list()
                )

                meta_idx <- integer(0)
                if (!is.null(study_meta) && nrow(study_meta) > 0) {
                    package_match <- rep(FALSE, nrow(study_meta))

                    if ("PackageID" %in% names(study_meta)) {
                        package_match <- study_meta$PackageID == package_id
                        package_match[is.na(package_match)] <- FALSE
                    }

                    meta_idx <- which(
                        study_meta$Values == dataset_id |
                        package_match
                    )
                }

                meta_row <- if (length(meta_idx) > 0) study_meta[meta_idx[[1]], , drop = FALSE] else NULL

                choice_text <- manifest$display_name %||% dataset_def$id
                if (is.null(manifest$display_name) && !is.null(meta_row) && !is.na(meta_row$Text[[1]]) && nzchar(meta_row$Text[[1]])) {
                    choice_text <- meta_row$Text[[1]]
                }

                choice_url <- manifest$url %||% NA
                if (is.null(manifest$url) && !is.null(meta_row) && "URL" %in% names(meta_row) && !is.na(meta_row$URL[[1]]) && nzchar(meta_row$URL[[1]])) {
                    choice_url <- meta_row$URL[[1]]
                }

                choice_tooltip <- manifest$helper_text %||% ""
                if (is.null(manifest$helper_text) && !is.null(meta_row) && "TooltipText" %in% names(meta_row) && !is.na(meta_row$TooltipText[[1]]) && nzchar(meta_row$TooltipText[[1]])) {
                    choice_tooltip <- meta_row$TooltipText[[1]]
                }

                choice_group <- normalize_field_set_label(manifest$group)
                if (is.null(manifest$group) && !is.null(meta_row) && "FieldSet" %in% names(meta_row) && !is.na(meta_row$FieldSet[[1]]) && nzchar(meta_row$FieldSet[[1]])) {
                    choice_group <- normalize_field_set_label(meta_row$FieldSet[[1]])
                }

                tibble(
                    Values = dataset_def$id,
                    Text = choice_text,
                    URL = choice_url,
                    TooltipText = choice_tooltip,
                    ShowTooltip = nzchar(choice_tooltip),
                    FieldSet = choice_group,
                    PackageID = package_id
                )
            })

            dplyr::bind_rows(dataset_rows)
        },
        get_catalog_plan = function(dataset_id) {
            dataset_id <- as.character(dataset_id)
            dataset_id <- dataset_id[!is.na(dataset_id) & nzchar(dataset_id)]

            if (length(dataset_id) == 0) {
                return(NULL)
            }

            feature_id <- feature_id_from_analysis_variable(self$analysisVariable)
            statistic_id <- statistic_id_from_ui_label(self$StatTest)

            tryCatch(
                private$app_config$plan_feature_association(
                    feature_id = feature_id,
                    dataset_id = dataset_id[[1]],
                    statistic_id = statistic_id
                ),
                error = function(e) NULL
            )
        },
        enrich_local_fact_data = function(plan, fact_data) {
            if (is.null(fact_data) || nrow(fact_data) == 0) {
                return(fact_data)
            }

            required_cols <- c("Karyotype", "Sex", "Age", "BMI", "Analyte")

            if (all(required_cols %in% names(fact_data))) {
                return(fact_data)
            }

            enriched <- fact_data

            if (!all(c("Karyotype", "Sex") %in% names(enriched)) && "record_id" %in% names(enriched)) {
                participants <- private$load_package_dimension("participants", plan)

                if (!is.null(participants) && "record_id" %in% names(participants)) {
                    cols <- intersect(c("record_id", "Karyotype", "Sex"), names(participants))

                    if (length(cols) > 1) {
                        enriched <- enriched |>
                            left_join(
                                participants |>
                                    select(all_of(cols)) |>
                                    distinct(),
                                by = "record_id"
                            )
                    }
                }
            }

            if (!all(c("Age", "BMI") %in% names(enriched)) && "LabID" %in% names(enriched)) {
                visits <- private$load_package_dimension("visits", plan)

                if (!is.null(visits) && "LabID" %in% names(visits)) {
                    cols <- intersect(
                        c("LabID", "Age", "BMI", "AgeAtTimeOfVisit", "BMIAtTimeOfVisit"),
                        names(visits)
                    )

                    if (length(cols) > 1) {
                        enriched <- enriched |>
                            left_join(
                                visits |>
                                    select(all_of(cols)) |>
                                    distinct(),
                                by = "LabID"
                            )
                    }
                }

                if (!"Age" %in% names(enriched) && "AgeAtTimeOfVisit" %in% names(enriched)) {
                    enriched$Age <- enriched$AgeAtTimeOfVisit
                }

                if (!"BMI" %in% names(enriched) && "BMIAtTimeOfVisit" %in% names(enriched)) {
                    enriched$BMI <- enriched$BMIAtTimeOfVisit
                }
            }

            if (!"Analyte" %in% names(enriched)) {
                analytes <- private$load_package_dimension("analytes", plan)

                if (!is.null(analytes)) {
                    name_col <- intersect(c("Analyte", "AnalyteName", "Gene", "Gene_name", "Feature"), names(analytes))
                    key_col <- NULL

                    if ("AnalyteID" %in% names(enriched) && "AnalyteID" %in% names(analytes)) {
                        key_col <- "AnalyteID"
                    } else if ("AnalyteKey" %in% names(enriched) && "AnalyteKey" %in% names(analytes)) {
                        key_col <- "AnalyteKey"
                    }

                    if (!is.null(key_col) && length(name_col) > 0) {
                        analyte_map <- analytes |>
                            select(all_of(c(key_col, name_col[[1]]))) |>
                            distinct()

                        names(analyte_map)[names(analyte_map) == name_col[[1]]] <- "Analyte"

                        enriched <- enriched |>
                            left_join(analyte_map, by = key_col)
                    }
                }

                if (!"Analyte" %in% names(enriched)) {
                    analyte_alias <- intersect(c("AnalyteName", "Gene", "Gene_name", "Feature", "feature", "analyte"), names(enriched))

                    if (length(analyte_alias) > 0) {
                        names(enriched)[names(enriched) == analyte_alias[[1]]] <- "Analyte"
                    }
                }
            }

            enriched
        },
        get_local_fact_data = function(plan) {
            fact_files <- plan$required_files$facts
            fact_file <- NULL

            if (!is.null(fact_files) && length(fact_files) > 0) {
                fact_file <- fact_files[[1]]
            }
            planned_fact_name <- plan$fact_name

            if (is.null(planned_fact_name) || !nzchar(planned_fact_name)) {
                planned_fact_name <- NULL
            }

            if (is.null(fact_file) || !nzchar(fact_file)) {
                fact_file <- private$app_config$package_resolver$resolve_fact_file(
                    plan$package_id,
                    planned_fact_name
                )
            }

            if (is.null(fact_file) || !nzchar(fact_file)) {
                stop(sprintf("Package '%s' does not expose a fact table.", plan$package_id), call. = FALSE)
            }

            package_root <- private$app_config$package_resolver$get_package_root(plan$package_id)
            parquet_path <- file.path(package_root, fact_file)

            if (!(file.exists(parquet_path) || dir.exists(parquet_path))) {
                stop(sprintf("Planned parquet file not found: %s", parquet_path), call. = FALSE)
            }

            fact_data <- private$app_config$load_local_package_artifact(plan$package_id, fact_file)
            private$enrich_local_fact_data(plan, fact_data)
        },
        load_package_dimension = function(dimension_name, plan = NULL) {
            if (is.null(plan)) {
                plan <- self$CurrentPlan

                if (is.null(plan) && !is.null(self$Study) && nzchar(self$Study)) {
                    plan <- private$get_catalog_plan(self$Study)
                    self$CurrentPlan <- plan
                }
            }

            if (is.null(plan) || is.null(plan$package_id) || !nzchar(plan$package_id)) {
                return(NULL)
            }

            rel_path <- tryCatch(
                private$app_config$package_resolver$resolve_dimension_file(plan$package_id, dimension_name),
                error = function(e) NULL
            )

            if (is.null(rel_path) || !nzchar(rel_path)) {
                return(NULL)
            }

            tryCatch(
                private$app_config$load_local_package_artifact(plan$package_id, rel_path),
                error = function(e) NULL
            )
        },
        get_local_dataset_data = function(dataset_id) {
            private$app_config$get_local_dataset_data(dataset_id)
        }
    ),
    active = list(
        application_id = function(value) {
            return(private$app_config$application_id)
        },
        applicationName = function(value) {
            if (!is.null(private$analysis_config) && "ApplicationName" %in% names(private$analysis_config)) {
                return(private$analysis_config$ApplicationName)
            }

            return("")
        },
        namespace = function(value) {
            return(private$analysis_config$Namespace)
        },
        analysisVariable = function(value) {
            return(private$analysis_config$AnalysisVariableName)
        },
        analysisVariableLabel = function(value) {
            return(private$analysis_config$AnalysisVariableLabel)
        },
        analysisType = function(value) {
            return(private$analysis_config$AnalysisType)
        },
        experimentIDs = function(value) {
            return(str_split_1(private$analysis_config$ExperimentIDs, "\\|"))
        },
        remoteDB = function(value) {
            return(private$app_config$remote_db)
        },
        Studies = function(value) {
            feature_id <- feature_id_from_analysis_variable(self$analysisVariable)

            catalog_studies <- tryCatch(
                private$build_catalog_studies(feature_id),
                error = function(e) NULL
            )

            if (!is.null(catalog_studies) && nrow(catalog_studies) > 0) {
                return(catalog_studies)
            }

            fallback_studies <- self$input_config$studies |>
                filter(Values %in% self$experimentIDs)

            if (is.null(fallback_studies) || nrow(fallback_studies) == 0) {
                return(fallback_studies)
            }

            enriched_rows <- lapply(seq_len(nrow(fallback_studies)), function(i) {
                row <- fallback_studies[i, , drop = FALSE]

                dataset_id <- row$Values[[1]]
                dataset_def <- tryCatch(
                    private$app_config$get_catalog_dataset_definition(dataset_id),
                    error = function(e) NULL
                )

                package_id <- NA_character_
                if ("PackageID" %in% names(row)) {
                    package_id <- row$PackageID[[1]]
                }

                if (is.null(package_id) || is.na(package_id) || !nzchar(package_id)) {
                    if (!is.null(dataset_def)) {
                        package_id <- dataset_def$package %||% dataset_def$id
                    }
                }

                if (is.null(package_id) || is.na(package_id) || !nzchar(package_id)) {
                    package_id <- dataset_id
                }

                manifest <- tryCatch(
                    private$app_config$package_resolver$get_package_manifest(package_id),
                    error = function(e) list()
                )

                if (!is.null(manifest$display_name) && nzchar(manifest$display_name)) {
                    row$Text[[1]] <- manifest$display_name
                }

                if (!is.null(manifest$url) && nzchar(manifest$url)) {
                    row$URL[[1]] <- manifest$url
                }

                if (!is.null(manifest$helper_text) && nzchar(manifest$helper_text)) {
                    row$TooltipText[[1]] <- manifest$helper_text
                    if ("ShowTooltip" %in% names(row)) {
                        row$ShowTooltip[[1]] <- TRUE
                    }
                }

                if (!is.null(manifest$group) && nzchar(manifest$group)) {
                    row$FieldSet[[1]] <- normalize_field_set_label(manifest$group)
                } else {
                    row$FieldSet[[1]] <- normalize_field_set_label(row$FieldSet[[1]])
                }

                if ("PackageID" %in% names(row)) {
                    row$PackageID[[1]] <- package_id
                }

                row
            })

            dplyr::bind_rows(enriched_rows)
        },
        StudyLabel = function(value) {
            study_value <- as.character(self$Study)
            study_value <- study_value[!is.na(study_value) & nzchar(study_value)]

            if (length(study_value) == 0) {
                return("")
            }

            study_value <- study_value[[1]]
            studies <- self$Studies

            if (is.null(studies) || nrow(studies) == 0) {
                return(study_value)
            }

            selected <- studies |>
                filter(Values == study_value) |>
                pull(Text)

            selected <- as.character(selected)
            selected <- selected[!is.na(selected) & nzchar(selected)]

            if (length(selected) == 0) {
                return(study_value)
            }

            selected[[1]]
        },
        StudyData = function(value) {
            if (is.null(self$Study) || !nzchar(self$Study)) {
                return(tibble())
            }

            plan <- private$get_catalog_plan(self$Study)
            self$CurrentPlan <- plan

            if (is.null(plan) || is.null(plan$package_id) || !nzchar(plan$package_id)) {
                return(tibble())
            }

            private$get_local_fact_data(plan)
        },
        KaryotypeCounts = function(value) {
            return(
                self$StudyData |>
                    group_by(Analyte, Karyotype) |>
                    summarise(n = n_distinct(LabID), .groups = "drop") |>
                    group_by(Karyotype) |>
                    summarise(n = round(median(n)), .groups = "drop") |>
                    build_karyotype_choices()
            )
        },
        Karyotypes = function(value) {
            karyotypes <- sanitize_choice_vector(self$input_config$karyotypes)

            comparison_row <- make_comparison_row(
                karyotypes,
                glue("Test for differences in {self$analysisVariable} trajectories between Trisomy 21 & Controls")
            )
            return(
                bind_rows(self$KaryotypeCounts, comparison_row) |>
                    arrange(sort)
            )
        },
        Sexes = function() {
            return(
                self$input_config$sexes
            )
        },
        Ages = function() {
            ages <- suppressWarnings(as.numeric(self$input_config$ages))
            ages <- ages[is.finite(ages)]

            if (length(ages) == 0) {
                return(c(0, 0))
            }

            return(c(min(ages), max(ages)))
        },
        CovariateChoices = function(value) {
            return(c("Age", "Sex"))
        },
        StatTestNames = function(value) {
            if (!is.null(self$Study) && nzchar(self$Study)) {
                stat_ids <- tryCatch(
                    private$app_config$get_dataset_statistic_ids(self$Study),
                    error = function(e) character(0)
                )

                if (length(stat_ids) > 0) {
                    return(unname(vapply(stat_ids, statistic_label_from_id, FUN.VALUE = character(1))))
                }
            }

            unname(self$input_config$statTestschoiceNames)
        },
        StatTestValues = function(value) {
            if (!is.null(self$Study) && nzchar(self$Study)) {
                stat_ids <- tryCatch(
                    private$app_config$get_dataset_statistic_ids(self$Study),
                    error = function(e) character(0)
                )

                if (length(stat_ids) > 0) {
                    return(unname(vapply(stat_ids, statistic_label_from_id, FUN.VALUE = character(1))))
                }
            }

            unname(self$input_config$statTests)
        },
        AdjustmentMethodNames = function(value) {
            return(
                self$input_config$adjustmentMethodsNames
            )
        },
        AdjustmentMethodValues = function(value) {
            return(
                self$input_config$adjustmentMethods
            )
        }
    ),
    public = list(
        localDB = NULL,
        input_config = NULL,
        analytesLabel = "Analytes",
        groupBaselineLabel = "",
        FoldChangeVar = "log2FoldChange",
        SignificanceVariable = "-log10pvalue",
        Study = NULL,
        Platform = NULL,
        CellType = NULL,
        Karyotype = NULL,
        Sex = NULL,
        Age = NULL,
        FilterLowCount = NULL,
        StatTest = NULL,
        Covariates = NULL,
        AdjustmentMethod = NULL,
        Adjusted = FALSE,
        SignificanceLabel = "p-value",
        FeatureData = NULL,
        CurrentPlan = NULL,
        initialize = function(app_config, analysis_config, input_config) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            self$input_config <- input_config
        }
    )
)

# karyotype inuts - only show karyotypes collapsed
#' @export
InputsManagerKaryotype <- R6Class(
    "InputsManagerKaryotype",
    inherit = InputsManagerBase,
    active = list(
        Karyotypes = function(value) {
            karyotypes <- sanitize_choice_vector(self$input_config$karyotypes)

            make_collapsed_karyotype_choices(
                karyotypes,
                "Test for differences between Trisomy 21 & Controls"
            )
        }
    )
)

# karyotype inuts - only show karyotypes collapsed
#' @export
InputsManagerPrecalculatedKaryotype <- R6Class(
    "InputsManagerPrecalculatedKaryotype",
    inherit = InputsManagerBase,
    active = list(
        StudyData = function(value) {
            if (is.null(self$Study) || !nzchar(self$Study)) {
                return(tibble())
            }

            plan <- private$get_catalog_plan(self$Study)
            self$CurrentPlan <- plan

            if (is.null(plan) || is.null(plan$package_id) || !nzchar(plan$package_id)) {
                return(tibble())
            }

            artifact_rel_path <- plan$precalculated_artifact

            if (is.null(artifact_rel_path) || !nzchar(artifact_rel_path)) {
                statistic_id <- statistic_id_from_ui_label(self$StatTest)

                if (is.null(statistic_id) || !nzchar(statistic_id)) {
                    statistic_id <- "linear_model"
                }

                artifact_rel_path <- tryCatch(
                    private$app_config$package_resolver$resolve_precalculated_artifact(
                        package_id = plan$package_id,
                        statistic_id = statistic_id,
                        feature_id = plan$feature_id
                    ),
                    error = function(e) NULL
                )
            }

            if (is.null(artifact_rel_path) || !nzchar(artifact_rel_path)) {
                return(tibble())
            }

            private$app_config$load_local_package_artifact(
                package_id = plan$package_id,
                rel_path = artifact_rel_path
            )
        },
        Karyotypes = function(value) {
            data <- self$StudyData

            if (is.null(data) || nrow(data) == 0 || !"samples" %in% names(data)) {
                karyotypes <- sanitize_choice_vector(self$input_config$karyotypes)

                return(
                    make_collapsed_karyotype_choices(
                        karyotypes,
                        "Test for differences between Trisomy 21 & Controls"
                    )
                )
            }

            samples <- data |>
                distinct(samples) |>
                pull(samples) |>
                as.character() |>
                sanitize_choice_vector()

            samples <- unname(samples)

            if (identical(as.character(self$analysisVariable), "Karyotype")) {
                comparison_samples <- samples[grepl(";", samples, fixed = TRUE)]
                comparison_samples <- sanitize_choice_vector(comparison_samples)

                if (length(comparison_samples) > 0) {
                    samples <- comparison_samples
                }
            }

            if (length(samples) == 0) {
                karyotypes <- sanitize_choice_vector(self$input_config$karyotypes)

                return(
                    make_collapsed_karyotype_choices(
                        karyotypes,
                        "Test for differences between Trisomy 21 & Controls"
                    )
                )
            }

            label_for_sample <- function(sample) {
                parts <- strsplit(sample, ";", fixed = TRUE)[[1]]
                parts <- trimws(parts)
                parts <- parts[nzchar(parts)]

                if (length(parts) == 0) {
                    return("")
                }

                if (length(parts) == 1) {
                    return(tools::toTitleCase(parts[[1]]))
                }

                display_parts <- sort(parts)
                glue("{tools::toTitleCase(display_parts[[1]])} vs. {tools::toTitleCase(display_parts[[2]])}")
            }

            if (length(samples) == 1) {
                return(
                    tibble(
                        choiceNames = unname(vapply(samples, label_for_sample, FUN.VALUE = character(1))),
                        choiceValues = samples
                    )
                )
            }

            sample_estimates <- data |>
                filter(!is.na(samples), nzchar(samples)) |>
                group_by(samples) |>
                summarise(n_estimate = length(samples), .groups = "drop")

            estimate_for_sample <- function(sample) {
                idx <- match(sample, sample_estimates$samples)

                if (is.na(idx)) {
                    return(NA_integer_)
                }

                sample_estimates$n_estimate[[idx]]
            }

            choice_names <- vapply(samples, function(sample) {
                label <- label_for_sample(sample)

                n_estimate <- estimate_for_sample(sample)

                if (is.na(n_estimate)) {
                    return(label)
                }

                glue("{label} (n={format(n_estimate, big.mark = ',', scientific = FALSE, trim = TRUE)})")
            }, FUN.VALUE = character(1))

            tibble(
                choiceNames = unname(choice_names),
                choiceValues = samples
            )
        },
        StatTestNames = function(value) {
            return(
                "DESeq2 model"
            )
        },
        StatTestValues = function(value) {
            return(
                "linear model"
            )
        },
        AdjustmentMethodNames = function(value) {
            return(
                "Benjamini-Hochberg (FDR)"
            )
        },
        AdjustmentMethodValues = function(value) {
            return(
                "BH"
            )
        }
    )
)

# Age inputs - do not show age as covariate
#' @export
InputsManagerAge <- R6Class(
    "InputsManagerAge",
    inherit = InputsManagerBase,
    active = list(
        CovariateChoices = function(value) {
            return(c("Sex"))
        }
    )
)

# Age inputs in precalculated mode - use summary-backed study data and age-specific covariate choices
#' @export
InputsManagerPrecalculatedAge <- R6Class(
    "InputsManagerPrecalculatedAge",
    inherit = InputsManagerPrecalculatedKaryotype,
    active = list(
        CovariateChoices = function(value) {
            return(c("Sex"))
        }
    )
)

# Sex inputs - do not show sex as covariate
#' @export
InputsManagerSex <- R6Class(
    "InputsManagerSex",
    inherit = InputsManagerBase,
    active = list(
        CovariateChoices = function(value) {
            return(c("Age"))
        }
    )
)

# Comorbidity inputs - add comorbidity fields
#' @export
InputsManagerComorbidity <- R6Class(
    "InputsManagerComorbidity",
    inherit = InputsManagerBase,
    active = list(
        Karyotypes = function(value) {
            karyotypes <- sanitize_choice_vector(self$input_config$karyotypes)
            return(
                tibble(
                    choiceNames = karyotypes[1],
                    choiceValues = karyotypes[1]
                )
            )
        },
        ConditionChoices = function(value) {
            return(
                self$input_config$ConditionChoices |>
                    select(ConditionClass, Condition)
            )
        },
        Conditions = function(value) {
            return(
                self$data_source$ConditionChoices
            )
        }
    ),
    public = list(
        getConditionTree = function(conditions = NULL) {
            tree <- conditions |>
                dfToTree()

            if (is.null(self$Conditions)) {
                return(tree)
            }

            selected_nodes <- self$get_selected_conditions(self$Conditions) |>
                pull(selected)

            if (length(selected_nodes) == 0) {
                return(tree)
            }

            for (i in seq_along(tree)) {
                if (!is.list(tree[i])) {
                    next
                }

                available_nodes <- names(tree[[i]][[1]])
                for (node in intersect(available_nodes, selected_nodes)) {
                    attr(tree[[i]][[node]], "stselected") <- TRUE
                    attr(tree[[i]][[node]], "stopened") <- TRUE
                }
            }

            return(tree)
        },
        get_selected_conditions = function(conditions) {
            return(
                get_selected(conditions, "classid") |>
                    unlist() |>
                    tibble() |>
                    set_names("selected") |>
                    distinct()
            )
        },
        get_selected_condition_list = function(conditions) {
            return(
                self$get_selected_conditions(conditions) |>
                    arrange() |>
                    summarise(text = str_c(selected, collapse = "<br />")) |>
                    pull()
            )
        }
    )
)

# BMI inputs - no spefici overrides, but create class for future BMI
# specific input handling if needed
#' @export
InputsManagerBMI <- R6Class(
    "InputsManagerBMI",
    inherit = InputsManagerBase
)

# cell types - source karyotype counts from remote files, include params field
#' @export
InputsManagerCellTypes <- R6Class(
    "InputsManagerCellTypes",
    inherit = InputsManagerBase,
    active = list(
        Karyotypes = function(value) {
            karyotypes <- sanitize_choice_vector(self$input_config$karyotypes)
            return(
                self$KaryotypeCounts |>
                    bind_rows(
                        make_comparison_row(
                            karyotypes,
                            "Test for differences in trajectories between Trisomy 21 & Controls"
                        )
                    ) |>
                arrange(sort)
            )
        },
        params = function(value) {
            return(
                ifelse(
                    is.null(self$Covariates),
                    "none",
                    str_c(self$Covariates, collapse = ";")
                )
            )
        }
    )
)

# TOFA inputs
#' @export
InputsManagerTOFA <- R6Class(
    "InputsManagerTOFA",
    inherit = InputsManagerBase,
    private = list(
        get_catalog_plan = function(dataset_id) {
            feature_id <- "timepoint"
            if (!is.null(private$analysis_config$Namespace) && nzchar(private$analysis_config$Namespace)) {
                feature_id <- tolower(private$analysis_config$Namespace)
            }

            statistic_id <- statistic_id_from_ui_label(self$StatTest)

            context <- private$app_config$feature_association_planner$create_context(
                feature_id = feature_id,
                dataset_id = dataset_id,
                statistic_id = statistic_id,
                filters = list(),
                covariates = character(0),
                visualization = list(),
                analysis_id = "tofa_feature_association"
            )

            private$app_config$feature_association_planner$plan(context)
        }
    ),
    active = list(
        participant_data = function(value) {
            data <- private$load_package_dimension("participants")

            if (!is.null(data)) {
                return(data)
            }

            # Fallback: filter global cache to this package
            global_data <- private$app_config$participant_data
            plan <- self$CurrentPlan

            if (!is.null(plan) && !is.null(plan$package_id) && nzchar(plan$package_id) &&
                "PackageID" %in% names(global_data)) {
                return(global_data |> filter(PackageID == plan$package_id))
            }

            global_data
        },
        visit_data = function(value) {
            data <- private$load_package_dimension("visits")

            if (!is.null(data)) {
                return(
                    data |>
                        mutate(
                            Age_at_visit_in_days = as.numeric(Age_at_visit_in_days),
                            Height_cm = as.numeric(Height_cm),
                            Weight_kg = as.numeric(Weight_kg)
                        )
                )
            }

            # Fallback: filter global encounter cache to this package
            global_data <- private$app_config$encounter_data |>
                mutate(
                    Age_at_visit_in_days = as.numeric(Age_at_visit_in_days),
                    Height_cm = as.numeric(Height_cm),
                    Weight_kg = as.numeric(Weight_kg)
                )
            plan <- self$CurrentPlan

            if (!is.null(plan) && !is.null(plan$package_id) && nzchar(plan$package_id) &&
                "PackageID" %in% names(global_data)) {
                return(global_data |> filter(PackageID == plan$package_id))
            }

            global_data
        },
        Karyotypes = function(value) {
            return(
                self$participant_data |>
                    select(Karyotype) |>
                    distinct() |>
                    pull() |>
                    sanitize_choice_vector()
            )
        },
        Sexes = function(value) {
            return(
                self$participant_data |>
                    select(Sex) |>
                    distinct() |>
                    pull()
            )
        },
        races = function(value) {
            return(
                self$participant_data |>
                    select(Race) |>
                    distinct() |>
                    pull()
            )
        },
        ethnicities = function(value) {
            return(
                self$participant_data |>
                    select(Ethnicity) |>
                    distinct() |>
                    pull()
            )
        },
        visit_extended_data = function(value) {
            return(
                self$visit_data |>
                    mutate(
                        age_at_visit_in_years = Age_at_visit_in_days / 365,
                        age_group = ifelse(age_at_visit_in_years >= 18, "Adult", "Under 18")
                    )
            )
        },
        events = function(value) {
            return(
                self$visit_data |>
                    select(Event_Name) |>
                    distinct() |>
                    pull()
            )
        },
        event_with_sequence = function(value) {
            return(
                self$visit_data |>
                    select(Event_Name) |>
                    distinct() |>
                    mutate(t = row_number())
            )
        },
        event_comparisons = function(value) {
            return(
                self$event_with_sequence |>
                    cross_join(
                        self$event_with_sequence
                    ) |>
                    filter(
                        Event_Name.x != Event_Name.y,
                        t.y > t.x
                    ) |>
                    select(-c(t.x, t.y)) |>
                    mutate(
                        analysis = glue(
                            "{Event_Name.x} vs {Event_Name.y}"
                        ),
                        events = glue("{Event_Name.x}|{Event_Name.y}")
                    ) |>
                    select(analysis, events)
            )
        },
        age_at_visit = function(value) {
            return(
                self$visit_extended_data |>
                    select(Age_at_visit_in_days) |>
                    distinct() |>
                    pull()
            )
        },
        Age_Groups = function(value) {
            return(
                self$visit_extended_data |>
                    select(age_group) |>
                    distinct() |>
                    pull()
            )
        },
        conditions = function(value) {
            return(
                self$participant_data |>
                    select("condition" = Qualifying_feature) |>
                    separate_rows(condition, sep = "; ") |>
                    distinct() |>
                    pull()
            )
        },
        features = function(value) {
            return(
                private$get_local_dataset_data(self$dataset) |>
                    select(self$feature_col) |>
                    distinct() |>
                    arrange(.data[[self$feature_col]]) |>
                    pull()
            )
        },
        StudyData = function(value) {
            plan <- private$get_catalog_plan(self$Study)
            self$CurrentPlan <- plan

            return(
                self$participant_data |>
                inner_join(
                    self$visit_extended_data, join_by(Internal_ParticipantID, External_ParticipantID)
                )
            )
        }
    ),
    public = list(
        feature_col = "Feature",
        dataset = NULL,
        filtered_data = NULL,
        initialize = function(app_config, analysis_config, input_config, dataset, ...) {
            super$initialize(app_config, analysis_config, input_config)
            self$dataset <- dataset
            self$Study <- dataset
            self$StatTest <- "Linear Model"
        }
    )
)

