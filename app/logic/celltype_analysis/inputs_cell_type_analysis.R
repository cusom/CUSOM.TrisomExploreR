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
      analytes <- private$app_config$analytes_data
      analyte_col <- intersect(c("AnalyteName", "Analyte", "Gene", "Gene_name", "Feature", "feature", "analyte"), names(analytes))

      if (length(analyte_col) == 0) {
        return(tibble(AnalyteName = character(0)))
      }

      names(analytes)[names(analytes) == analyte_col[[1]]] <- "AnalyteName"

      return(
        analytes |>
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
      participants <- self$Participants
      encounters <- self$Encounters

      if (nrow(participants) == 0 || nrow(encounters) == 0) {
        return(tibble(
          LabID = character(0),
          record_id = character(0),
          Age = numeric(0),
          Karyotype = character(0),
          Sex = character(0)
        ))
      }

      participant_id_col <- intersect(c("record_id", "Record_ID", "participant_id", "ParticipantID"), names(participants))
      encounter_id_col <- intersect(c("record_id", "Record_ID", "participant_id", "ParticipantID"), names(encounters))
      lab_col <- intersect(c("LabID", "TOFA_LabID", "lab_id"), names(encounters))
      age_col <- intersect(c("AgeAtTimeOfVisit", "Age", "Age_at_visit_in_days"), names(encounters))

      if (length(participant_id_col) == 0 || length(encounter_id_col) == 0) {
        return(tibble(
          LabID = character(0),
          record_id = character(0),
          Age = numeric(0),
          Karyotype = character(0),
          Sex = character(0)
        ))
      }

      participants_join <- participants
      encounters_join <- encounters

      if (participant_id_col[[1]] != "record_id") {
        names(participants_join)[names(participants_join) == participant_id_col[[1]]] <- "record_id"
      }

      if (encounter_id_col[[1]] != "record_id") {
        names(encounters_join)[names(encounters_join) == encounter_id_col[[1]]] <- "record_id"
      }

      if (length(lab_col) > 0 && lab_col[[1]] != "LabID") {
        names(encounters_join)[names(encounters_join) == lab_col[[1]]] <- "LabID"
      }

      if (length(age_col) > 0 && age_col[[1]] != "Age") {
        names(encounters_join)[names(encounters_join) == age_col[[1]]] <- "Age"
      }

      joined <- participants_join |>
        inner_join(encounters_join, by = "record_id")

      if (!"LabID" %in% names(joined)) {
        joined$LabID <- NA_character_
      }

      if (!"Age" %in% names(joined)) {
        joined$Age <- NA_real_
      }

      return(
        joined |>
          select(LabID, record_id, Age, Karyotype, Sex)
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

      age_values <- suppressWarnings(as.numeric(self$Age))
      age_values <- age_values[is.finite(age_values)]

      join_keys <- intersect(c("LabID", "record_id"), intersect(names(facts), names(self$ParticipantsWithEncounters)))

      base_data <- if (length(join_keys) > 0) {
        facts |> left_join(self$ParticipantsWithEncounters, by = join_keys)
      } else {
        facts
      }

      if (!"Sex" %in% names(base_data)) {
        base_data$Sex <- NA_character_
      }

      if (!"Age" %in% names(base_data)) {
        base_data$Age <- NA_real_
      }

      if (!"Karyotype" %in% names(base_data)) {
        base_data$Karyotype <- NA_character_
      }

      base_data <- base_data |>
        filter(
          CellType %in% self$CellType,
          (Sex %in% self$Sex | is.na(Sex))
        )

      if (length(age_values) > 0) {
        age_min <- min(age_values)
        age_max <- max(age_values)
        base_data <- base_data |>
          filter((Age >= age_min) | is.na(Age), (Age <= age_max) | is.na(Age))
      }

      analyte_alias <- intersect(c("AnalyteName", "Analyte", "Gene", "Gene_name", "Feature", "feature", "analyte"), names(base_data))

      if (length(analyte_alias) > 0 && analyte_alias[[1]] != "AnalyteName") {
        names(base_data)[names(base_data) == analyte_alias[[1]]] <- "AnalyteName"
      }

      self$base_data <- base_data |>
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
