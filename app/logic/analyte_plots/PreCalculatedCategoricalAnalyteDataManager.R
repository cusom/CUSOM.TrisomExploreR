box::use(
  dplyr[select, filter, mutate, case_when, ungroup, first,
      distinct, inner_join, rename, add_count, if_else, between],
  tibble[tibble],
  tidyr[separate_rows],
  glue[glue],
  plotly[layout, config],
  htmlwidgets[onRender],
  rlang[sym]
)

box::use(
  app/logic/analyte_plots/CategoricalAnalyteDataManager[CategoricalFeatureAnalysisAnalyteDataManager]
)

#' @export
PreCalculatedCategoricalFeatureAnalysisAnalyteDataManager <- R6::R6Class(
  "PreCalculatedCategoricalFeatureAnalysisAnalyteDataManager",
  inherit = CategoricalFeatureAnalysisAnalyteDataManager,
  private = list(),
  active = list(
    Age = function(value) {
      return(
        self$StudyData() |>
          distinct(ages) |>
          separate_rows(ages, sep = ";") |>
          rename("Age" = ages) |>
          mutate(Age = as.integer(Age))
      )
    },
    Sex = function(value) {
      self$StudyData() |>
          distinct(sexes) |>
          separate_rows(sexes, sep = ";") |>
          rename("Sex" = sexes)
    },
    Karyotype = function(value) {
      self$StudyData() |>
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
        min(self$Age, na.rm = TRUE)
      )
    },
    age_max = function(value) {
      return(
        max(self$Age, na.rm = TRUE)
      )
    }
  ),
  public = list(
    initialize = function(app_config, analysis_config, study, study_data, analyte, summary_data) {
      super$initialize(app_config, analysis_config, study, study_data, analyte, summary_data)
    },
    set_single_analyte_data = function() {
      self$AnalyteData <- self$remote_db$getQuery(
          "EXEC [shiny].[GetDataByExperimentAnalyte] ?,?",
          tibble(StudyName = self$study(), Analyte = self$Analyte())
        ) |>
          rename(record_id = Record_ID) |>
          inner_join(
            self$remote_db$getQuery(
              "EXEC [shiny].[GetParticipantsByExperiment] ?",
              tibble(StudyName = self$study())
            ),
            by = "record_id"
          ) |>
          rename(Sex = Gender) |>
          inner_join(
            self$remote_db$getQuery(
              "EXEC [shiny].[GetParticipantEncounterByExperiment] ?",
              tibble(StudyName = self$study())
            ),
            by = c("LabID", "record_id")
          ) |>
          rename(Age = AgeAtTimeOfVisit) |>
          inner_join(self$Sex, by = "Sex") |>
          inner_join(self$Karyotype, by = "Karyotype") |>
          mutate(
            log2MeasuredValue = if_else(MeasuredValue == 0, 0, log2(MeasuredValue)),
            log2Measurement   = glue("log<sub>2</sub>({Measurement})"),
            highlightGroup = NA_character_  # if/when needed
          ) |>
          filter(
            is.finite(log2MeasuredValue),
            between(Age, self$age_min, self$age_max)
          ) |>
          add_count(!!self$analysis_var, name = "n") |>
          mutate(
            Karyotype = glue("<b>{Karyotype}</b> (n={n})"),
            text      = glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
          ) |>
          select(-n)
      return(invisible(self$AnalyteData))
    }
  )
)
