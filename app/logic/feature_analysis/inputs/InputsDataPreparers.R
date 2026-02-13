box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, filter, between, mutate, group_by, summarise, ungroup, rename_with,
            distinct, n, pull, arrange, dense_rank, row_number, if_else],
    purrr[pmap],
    stringr[str_split],
    rlang[sym]
)

InputsDataPreparerBase <- R6Class(
    "InputsDataPreparerBase",
    private = list(
        analysis_config = NULL
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
        initialize = function(analysis_config) {
            private$analysis_config <- analysis_config
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
        initialize = function(analysis_config) {
            super$initialize(analysis_config)
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
