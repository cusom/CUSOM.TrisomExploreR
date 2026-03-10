box::use(
  R6[R6Class],
  purrr[pluck, pmap],
  dplyr[left_join, inner_join, join_by, select, first,
    rename, filter, mutate, distinct, pull, add_count],
  tibble[tibble],
  glue[glue],
  forcats[fct_relevel],

)

box::use(
  app/logic/feature_analysis/inputs/InputsDataManagers[InputsManagerBase],
  app/logic/shared/statistical_analysis[getGroupedStatTestByKeyGroup, formatPValue]
)

#' @export
CellTypesInputsManager <- R6Class(
  "CellTypesInputsManager",
  inherit = InputsManagerBase,
  private = list(),
  active = list(
    CellTypes = function() {
      return(
        self$remote_files$get_remote_file_data("inputs") |>
          pluck("cell_types") |>
          unlist() |>
          unname()
      )
    },
    Analytes = function() {
      return(
        self$remote_files$get_remote_file_data("genes") |>
          pull(Gene)
      )
    },
    Participants = function(value) {
      return(
        self$remote_files$get_remote_file_data("participants")
      )
    },
    Encounters = function(value) {
      return(
        self$remote_files$get_remote_file_data("encounter")
      )
    },
    ParticipantsWithEncounters = function(value) {
      return(
        self$Participants |>
          inner_join(self$Encounters, by = "record_id") |>
          select(LabID, record_id, "Age" = AgeAtTimeOfVisit, Karyotype, Sex)
      )
    },
    cids = function(value) {
      return(
        self$base_data |>
          filter(Karyotype == "Control") |>
          select(LabID) |>
          distinct() |>
          pull()
      )
    },
    tids = function(value) {
      return(
        self$base_data |>
          filter(Karyotype != "Control") |>
          select(LabID) |>
          distinct() |>
          pull()
      )
    },
    data_with_counts = function(value) {
      return(
        self$base_data |>
          add_count(Karyotype, CellType)
      )
    },
    log_2_measurement_label = function(value) {
      return(
        self$base_data |>
          select(Measurement) |>
          first() |>
          mutate(Measurement = glue("log<sub>2</sub> ({Measurement})")) |>
          pull()
      )
    }
  ),
  public = list(
    Analyte = NULL,
    base_data = NULL,
    combined_data = NULL,
    initialize = function(app_config, analysis_config, input_config) {
      super$initialize(app_config, analysis_config, input_config)
      self$Platform <- self$remote_files$get_remote_file_data("inputs") |>
          pluck("platforms")
    },

    set_base_data = function() {

      self$base_data <- self$remoteDB$getQuery(
          "[shiny].[GetAnalyteDataByPlatform] ?,?",
          tibble(
            "Platform" = self$Platform,
            "Analyte" = self$Analyte,
          )
        ) |>
        left_join(self$ParticipantsWithEncounters, by = c("LabID", "record_id")) |>
        rename("CellType" = Specimen) |>
        filter(
          CellType %in% self$CellType,
          (Sex %in% self$Sex | is.na(Sex)),
          (Age >= min(self$Age) | is.na(Age)),
          (Age <= max(self$Age) | is.na(Age)),
          outlier == FALSE
        ) |>
        mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue("log<sub>2</sub> ({Measurement})"),
          Karyotype = fct_relevel(Karyotype, "Control")
        )
      return(invisible(self$base_data))
    },
    cell_type_data = function() {
      self$set_base_data()
      self$combined_data <- self$base_data |>
        select(CellType, LabID,  Analyte, log2MeasuredValue, Karyotype, Sex, Age) |>
        getGroupedStatTestByKeyGroup(
          groupVar = CellType,
          id = LabID,
          key = Analyte,
          group = Karyotype,
          baselineLabel = "Control",
          response = log2MeasuredValue,
          testMethod = self$StatTest,
          adjustmentMethod = self$AdjustmentMethod,
          independentVariable = Karyotype,
          covariates = self$Covariates
        ) |>
        mutate(
          p.value.text = unlist(
            pmap(
              .l = list(p.value, self$AdjustmentMethod),
              formatPValue
            )
          ),
          p.value.is.significant = ifelse(
            p.value.text %in% c("No significant difference", "Unable to compute p-value using chosen methods"),
            FALSE,
            TRUE
          ),
          p.value.adjustment.method = self$AdjustmentMethod
        ) |>
        inner_join(self$data_with_counts, by = "CellType") |>
        mutate(
          text = glue("
            Cell Type: {CellType}
            {self$log_2_measurement_label}: {round(log2MeasuredValue, 4)}
            {Karyotype} (n={n})
            {p.value.text}"
          )
        )

      return(invisible(self$combined_data))

    }

  )
)
