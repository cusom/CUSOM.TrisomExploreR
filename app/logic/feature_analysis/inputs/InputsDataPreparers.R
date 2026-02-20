box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, filter, between, mutate, group_by, summarise, ungroup, rename_with,
            distinct, n, pull, arrange, dense_rank, row_number, if_else, inner_join,
            case_when],
    tidyr[drop_na],
    forcats[fct_relevel],
    purrr[pmap],
    stringr[str_split, str_c],
    rlang[sym]
)

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
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config) {
            super$initialize(analysis_config, app_config)
        },
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
    private = list(
        app_config = NULL
    ),
    active = list(
        remote_files = function(value) {
            return(private$app_config$remote_files)
        },
        participant_conditions = function(value) {
            return(
                self$remote_files$get_remote_file_data("conditions")
            )
        }
    ),
    public = list(
        initialize = function(analysis_config, app_config) {
            super$initialize(analysis_config, app_config)
            private$app_config <- app_config
        },
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
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config) {
            super$initialize(analysis_config)
        },
        prepare = function(data, study, karyotype, age, sex, params) {
            prepared <- data |>
                filter(
                    samples == str_c(karyotype, collapse = ";"),
                    selected_parameters == params
                ) |>
                select(-c(samples, selected_parameters)) |>
                mutate(
                    karyotypes = self$collapse_values(karyotype),
                    ages = self$collapse_values(age),
                    sexes = self$collapse_values(sex)
                )

            self$set_prepared_data(prepared)
        }
    )
)
