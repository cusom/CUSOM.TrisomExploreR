box::use(
    R6[R6Class],
    dplyr[select, filter, mutate, case_when, add_count, ungroup,
        inner_join, rename, distinct, arrange, pull],
    tibble[tibble],
    tidyr[separate_rows],
    forcats[fct_inorder],
    glue[glue],
    rlang[sym]
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
        }
    ),
    public = list(
        study = NULL,
        study_data = NULL,
        analyte = NULL,
        analyte_data = NULL,
        summary_data = NULL,
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            private$remote_db <- app_config$remote_db
            self$study <- study
            self$study_data <- study_data
            self$analyte <- analyte
            self$summary_data <- summary_data
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
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            super$initialize(analysis_config, app_config, study, study_data, analyte, summary_data)
        },
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
    private = list(),
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
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            super$initialize(analysis_config, app_config, study, study_data, analyte, summary_data)
        },
        get_single_data = function(analyte) {
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
    private = list(),
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
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            super$initialize(analysis_config, app_config, study, study_data, analyte, summary_data)
        },
        get_data = function(analyte) {
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
        }
    )
)
