box::use(
  R6[R6Class],
  purrr[pmap],
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
        private$app_config$get_package_measurements_data() |>
          select(Specimen_type) |>
          distinct() |>
          pull() |>
          stats::na.omit() |>
          unname()
      )
    },
    Analytes = function() {
      measurements <- private$app_config$get_package_measurements_data()

      if (!is.null(self$CellType) && length(self$CellType) > 0) {
        measurements <- measurements |>
          filter(Specimen_type %in% self$CellType)
      }

      return(
        measurements |>
          select(Analyte) |>
          distinct() |>
          pull()
      )
    },
    Participants = function(value) {
      return(
        private$app_config$participant_data
      )
    },
    Encounters = function(value) {
      return(
        private$app_config$encounter_data
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
      self$Platform <- self$input_config$platforms
    },

    set_base_data = function() {
      measurements <- private$app_config$get_package_measurements_data()

      if (!is.null(self$Platform) && length(self$Platform) > 0 && "Platform" %in% names(measurements)) {
        measurements <- measurements |>
          filter(Platform %in% self$Platform)
      }

      self$base_data <- measurements |>
        filter(Analyte == self$Analyte) |>
        left_join(self$ParticipantsWithEncounters, by = c("LabID", "record_id")) |>
        rename("CellType" = Specimen_type) |>
        filter(
          CellType %in% self$CellType,
          (Sex %in% self$Sex | is.na(Sex)),
          (Age >= min(self$Age) | is.na(Age)),
          (Age <= max(self$Age) | is.na(Age))
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
