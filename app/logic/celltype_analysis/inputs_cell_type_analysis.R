box::use(
  R6[R6Class],
  purrr[pmap],
  dplyr[left_join, inner_join, select, first, rename,
    filter, mutate, distinct, pull, add_count, arrange],
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
        private$app_config$cell_types_data |>
          pull(cell_type) |>
          stats::na.omit() |>
          unname()
      )
    },
    AnalytesData = function() {
      return(
        private$app_config$analytes_data |>
          select(AnalyteName) |>
          distinct() |>
          arrange(AnalyteName)
      )
    },
    GeneLabel = function() {
      if (is.null(self$Analyte) || !nzchar(self$Analyte)) {
        return("")
      }
      self$Analyte
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
      # Derive package_id from catalog via the cell_type feature link
      datasets <- private$app_config$get_catalog_feature_datasets("cell_type", "cell_type_analysis")
      package_id <- datasets$package_id[[1]]

      # self$Analyte holds the Analyte bound from the virtualSelectInput value
      analyte <- self$Analyte
   
      # Pull filtered fact data using partition pushdown on cell_type and Analyte filter
      # Rename partition column cell_type → CellType for downstream consistency
      facts <- private$app_config$get_filtered_fact_data(
        package_id = package_id,
        cell_types  = self$CellType,
        analyte_name  = analyte
      ) |> rename(CellType = cell_type)

      self$base_data <- facts |>
        left_join(self$ParticipantsWithEncounters, by = c("LabID", "record_id")) |>
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
        select(CellType, LabID, AnalyteName, log2MeasuredValue, Karyotype, Sex, Age) |>
        getGroupedStatTestByKeyGroup(
          groupVar = CellType,
          id = LabID,
          key = AnalyteName,
          group = Karyotype,
          baselineLabel = "Control",
          response = log2MeasuredValue,
          testMethod = self$StatTest,
          adjustmentMethod = self$AdjustmentMethod,
          independentVariable = Karyotype,
          covariates = self$Covariates
        ) |>
        mutate(
          CellType = as.character(CellType)
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
