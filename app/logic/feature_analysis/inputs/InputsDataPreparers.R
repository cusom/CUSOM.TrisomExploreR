box::use(
    R6[R6Class],
    glue[glue],
    dplyr[select, filter, between, mutate, summarise,
            pull, if_else, inner_join,
            case_when],
    tidyr[drop_na],
    stringr[str_split_1],
    stringr[str_split, str_c],
)

#' @export
InputsDataPreparerBase <- R6Class(
    "InputsDataPreparerBase",
    private = list(
        analysis_config = NULL,
        app_config = NULL
    ),
    active = list(
        analysisVariable = function(value) {
            if (missing(value)) {
                return(
                    private$analysis_config$AnalysisVariableName
                )
            } else {
                private$analysis_config$AnalysisVariable <- value
            }
        }
    ),
    public = list(
        data = NULL,
        initialize = function(analysis_config, app_config = NULL) {
            private$analysis_config <- analysis_config
            private$app_config <- app_config
        },
        parse_karyotypes = function(karyotypes) {
            str_split(karyotypes, pattern = ";", simplify = FALSE) |>
                unlist() |>
                trimws() |>
                unique()
        },
        prepare_feature_inputs = function(.data, ages, sexes, karyotypes, include_record_id = FALSE) {
            columns <- c("LabID", "Karyotype", "Sex", "Age", "BMI", "Analyte", "MeasuredValue", "Measurement")
            if (isTRUE(include_record_id)) {
                columns <- c("record_id", columns)
            }

            .data |>
                select(all_of(columns)) |>
                filter(
                    between(Age, ages[1], ages[2]),
                    Sex %in% sexes,
                    Karyotype %in% karyotypes
                ) |>
                mutate(
                    log2MeasuredValue = if_else(MeasuredValue == 0, 0, log2(MeasuredValue)),
                    log2Measurement = glue("log<sub>2</sub>({Measurement})")
                )
        },
        collapse_values = function(values) {
            str_c(values, collapse = ";")
        },
        set_prepared_data = function(data) {
            self$data <- data
            invisible(self$data)
        },
        prepare = function() {
            stop("implement prepare")
        }
    )
)

#' @export
FeatureAnalysisInputsDataPreparer <- R6Class(
    "FeatureAnalysisInputsDataPreparer",
    inherit = InputsDataPreparerBase,
    public = list(
        prepare = function(.data, study, karyotypes, sexes, ages, ...) {
            k_vec <- self$parse_karyotypes(karyotypes)

            prepared <- self$prepare_feature_inputs(
                .data = .data,
                ages = ages,
                sexes = sexes,
                karyotypes = k_vec
            ) |>
                filter(!is.na(.data[[self$analysisVariable]]))

            self$set_prepared_data(prepared)
        }
    )
)

#' @export
FeatureAnalysisInputsComorbidityDataPreparer <- R6Class(
    "FeatureAnalysisInputsComorbidityDataPreparer",
    inherit = InputsDataPreparerBase,
    active = list(
        participant_conditions = function(value) {
            private$app_config$condition_data
        }
    ),
    public = list(
        prepare = function(.data, study, karyotypes, sexes, ages, conditions) {
            k_vec <- self$parse_karyotypes(karyotypes)

            conds <- conditions |> pull() |> unique()

            cond_flags <- self$participant_conditions |>
                filter(Condition %in% conds) |>
                mutate(
                    has_flag = case_when(
                        HasCondition == "True"  ~ TRUE,
                        HasCondition == "False" ~ FALSE,
                        TRUE                    ~ NA
                    )
                ) |>
                summarise(
                    HasAnyConditionFlag = {
                        if (any(is.na(has_flag))) {
                            NA_character_
                        } else if (any(has_flag)) {
                            "Yes"
                        } else {
                            "No"
                        }
                    },
                    .by = record_id
                ) |>
                drop_na(HasAnyConditionFlag)

            prepared <- self$prepare_feature_inputs(
                .data = .data,
                ages = ages,
                sexes = sexes,
                karyotypes = k_vec,
                include_record_id = TRUE
            ) |>
                inner_join(cond_flags, by = "record_id") |>
                mutate(
                    HasAnyConditionFlag = factor(HasAnyConditionFlag, levels = c("No", "Yes"))
                )

            self$set_prepared_data(prepared)
        }
    )
)

#' @export
PreCalculatedFeatureAnalysisInputsPreparer <- R6Class(
    "PreCalculatedFeatureAnalysisInputsPreparer",
    inherit = InputsDataPreparerBase,
    public = list(
        prepare = function(data, study, karyotypes, ages, sexes, params, ...) {
            sample_col <- intersect(c("samples", "karyotypes"), names(data))
            params_col <- intersect(c("selected_parameters", "params"), names(data))

            prepared <- data

            if (length(sample_col) > 0) {
                prepared <- prepared |>
                    filter(.data[[sample_col[[1]]]] == str_c(karyotypes, collapse = ";"))
            }

            if (length(params_col) > 0) {
                prepared <- prepared |>
                    filter(.data[[params_col[[1]]]] == params)
            }

            prepared <- prepared |>
                select(-tidyselect::any_of(c("samples", "selected_parameters", "params"))) |>
                mutate(
                    karyotypes = self$collapse_values(karyotypes),
                    ages = self$collapse_values(ages),
                    sexes = self$collapse_values(sexes)
                )

            self$set_prepared_data(prepared)
        }
    )
)

#' @export
TOFAAnalysisInputsDataPreparer <- R6Class(
    "TOFAAnalysisInputsDataPreparer",
    inherit = InputsDataPreparerBase,
    active = list(
        time_series_data = function(value) {
            private$app_config$get_local_dataset_data(self$dataset)
        }
    ),
    public = list(
        dataset = NULL,
        dataset_data = NULL,
        visit_data = NULL,
        statistic_id = "linear_model",
        initialize = function(analysis_config, app_config, dataset, ...) {

            super$initialize(analysis_config, app_config)
            self$dataset <- dataset
            self$visit_data <- app_config$encounter_data
            self$dataset_data <- app_config$dataset_data

            available_stats <- tryCatch(
                private$app_config$get_dataset_statistic_ids(dataset),
                error = function(e) character(0)
            )

            if (length(available_stats) > 0 && !"linear_model" %in% available_stats) {
                self$statistic_id <- available_stats[[1]]
            }
        },
        load_precalculated_summary = function() {
            dataset_def <- private$app_config$get_catalog_dataset_definition(self$dataset)
            package_id <- dataset_def$package
            if (is.null(package_id) || !nzchar(package_id)) {
                package_id <- dataset_def$id
            }

            artifact_rel_path <- private$app_config$package_resolver$resolve_precalculated_artifact(
                package_id = package_id,
                statistic_id = self$statistic_id,
                feature_id = "timepoint"
            )

            if (is.null(artifact_rel_path) || !nzchar(artifact_rel_path)) {
                stop(
                    sprintf("No precalculated artifact found for dataset '%s' and statistic '%s'.", self$dataset, self$statistic_id),
                    call. = FALSE
                )
            }

            artifact_path <- file.path(private$app_config$package_resolver$packages_root, package_id, artifact_rel_path)

            if (!(file.exists(artifact_path) || dir.exists(artifact_path))) {
                stop(sprintf("Precalculated artifact not found: %s", artifact_path), call. = FALSE)
            }

            private$app_config$load_local_package_artifact(
                package_id,
                artifact_rel_path,
                feature_id = "timepoint"
            )
        },
        prepare = function(data, sexes, races, ethnicities, karyotype, age_at_visit, age_groups, 
            conditions = NULL, comparison = NULL, ...) {
            source <- self$load_precalculated_summary()

            comparison_value <- comparison
            if (is.null(comparison_value)) {
                comparison_value <- ""
            }

            comparison_timepoints <- str_split_1(comparison_value, "\\|") |>
                trimws() |>
                (
                    function(x) x[nzchar(x)]
                )() |>
                unique()

            if (length(comparison_timepoints) > 0) {
                source <- source |>
                    filter(Timepoint %in% comparison_timepoints)
            }

            return(
                source |>
                    select("Analyte" = Score_name, Mean_difference, pvalue, "padj" = qvalue) |>
                    mutate(Analyte = gsub(" ", "_", Analyte))  
            )
        }
    )
)
