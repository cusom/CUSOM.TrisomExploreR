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
    stringr[str_split],
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
        prepare = function(.data, study, karyotypes, sexes, ages) {
            k_vec <- str_split(karyotypes, pattern = ";", simplify = FALSE) |>
                unlist() |>
                trimws() |>
                unique()

            self$data <- .data |>
                select(LabID, Karyotype, Sex, Age, BMI, Analyte, MeasuredValue, Measurement) |>
                filter(
                    between(Age, ages[1], ages[2]),
                    Sex %in% sexes,
                    Karyotype %in% k_vec,
                    !is.na(.data[[self$analysisVariable]])
                ) |>
                mutate(
                    log2MeasuredValue = if_else(MeasuredValue == 0, 0, log2(MeasuredValue)),
                    log2Measurement   = glue("log<sub>2</sub>({Measurement})")
                )

            return(invisible(self$data))

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

            k_vec <- str_split(karyotypes, pattern = ";", simplify = FALSE) |>
                unlist() |>
                trimws() |>
                unique()

            # precompute conditions vector (if `conditions` is a tibble)
            conds <- conditions |> pull() |> unique()

            # Build once: per-record_id flag with SAME semantics you had:
            # - If ANY NA found among that record_id's rows → NA (and will be dropped below)
            # - Else "Yes" if ANY True, otherwise "No"
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
                tidyr::drop_na(HasAnyConditionFlag)

            # Main pipeline
            self$data <- .data |>
                select(record_id, LabID, Karyotype, Sex, Age, BMI, Analyte, MeasuredValue, Measurement) |>
                filter(
                    between(Age, ages[1], ages[2]),
                    Sex %in% sexes,
                    Karyotype %in% k_vec
                ) |>
                mutate(
                    # preserve original behavior: 0 → 0, positive → log2(value)
                    log2MeasuredValue = if_else(MeasuredValue == 0, 0.0, log2(MeasuredValue)),
                    log2Measurement   = sprintf("log<sub>2</sub>(%s)", Measurement)
                ) |>
                inner_join(cond_flags, by = "record_id") |>
                mutate(
                    HasAnyConditionFlag = factor(HasAnyConditionFlag, levels = c("No", "Yes"))
                )
            return(invisible(self$data))
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
            self$data <- data
                filter(
                    samples == str_c(karyotype, collapse = ";"),
                    selected_parameters == params
                ) |>
                select(-c(samples, selected_parameters)) |>
                mutate(
                    karyotypes = str_c(karyotype, collapse = ";"),
                    ages = str_c(age, collapse = ";"),
                    sexes = str_c(sex, collapse = ";")
                )
            return(invisible(self$data))
        }
    )
)
