box::use(
    R6[R6Class],
    glue[glue],
    dplyr[select, filter, between, mutate, summarise,
            pull, if_else, inner_join,
            case_when],
    tidyr[drop_na],
    stringr[str_split_1],
    stringr[str_split, str_c],
    utils[read.csv]
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
            prepared <- data |>
                filter(
                    samples == str_c(karyotypes, collapse = ";"),
                    selected_parameters == params
                ) |>
                select(-c(samples, selected_parameters)) |>
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
        initialize = function(analysis_config, app_config, dataset, ...) {

            super$initialize(analysis_config, app_config)
            self$dataset <- dataset
            self$visit_data <- app_config$encounter_data
            self$dataset_data <- app_config$dataset_data
        },
        prepare = function(data, sexes, races, ethnicities, karyotype, age_at_visit, age_groups, 
            conditions = NULL, comparison = NULL, ...) {
            return(
                read.csv("app/data/TOFA_trial_Endpioints_RESULTS_LMM_DRAFT.csv") |>
                    filter(Timepoint %in% str_split_1(comparison, "\\|")) |>
                    select("Analyte" = Score_name, Mean_difference, pvalue, "padj" = qvalue) |>
                    mutate(Analyte = gsub(" ", "_", Analyte))  
            )
        }
    )
)
