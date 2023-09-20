#' R6 Class to manage Comorbidity Analysis for Immune Maps data
#' @description
#' Subclass of ImmuneMapFeatureAnalysisManager R6 Class to enable Immune Map specific analysis.
#'
#' @field analysisMetadata -
#' @field analysisChoices -
#' @field Analysis -
#' @importFrom arrow open_dataset
#' @export
ImmuneMapComorbidityManager <- R6::R6Class(
  "ImmuneMapComorbidityManager",
  inherit = ImmuneMapFeatureAnalysisManager,
  private = list(),
  public = list(
    analysisMetadata = NULL,
    analysisChoices = NULL,
    Analysis = NULL,

    #' @description
    #' Create a new instance of a FeatureAnalysisManager
    #' @param applicationName string - name of application
    #' @param id string - namespace for this instance
    #' @param namespace_config list - configurations for this namespace
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){
      super$initialize(applicationName, id, namespace_config, remoteDB, localDB)
    },
    #' @description
    #' Get / set sample level data with filers applied
    #' Overrides super method with data model specific to Immune Maps data
    getBaseData = function() {

      base_data <- arrow::open_dataset("data/feature_data") |>
        dplyr::filter(
          ExperimentID == self$Study
        ) |>
        dplyr::collect() |>
        dplyr::select(
          record_id, LabID, Karyotype, Sex, Age, BMI,
          Analysis, CellType, Analyte, MeasuredValue, Measurement
        ) |>
        dplyr::filter(
          (
            Analysis == self$Analysis |
            Analysis == glue::glue("{self$Analysis};{glue::glue_collapse(self$CellType,sep=';')}")
          ),
          Age >= min(self$Age),
          Age <= max(self$Age),
          Sex %in% self$Sex,
          Karyotype %in% unlist(stringr::str_split(self$Karyotype, pattern = ";"))
        ) |>
        tidyr::separate(Analysis, into = c("Analysis1", "Analysis2"), sep = ";", remove = FALSE) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          CellTypeFilter = ifelse(self$Analysis == "Signaling", Analysis2, CellType)
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(CellTypeFilter %in% self$CellType) |>
        dplyr::select(-c(Analysis1, Analysis2)) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})")
        )

      condition_data <- arrow::open_dataset("data/participant_conditions") |>
        dplyr::collect() |>
        dplyr::select(record_id, Condition, HasCondition) |>
        dplyr::filter(
          Condition %in%  unlist(shinyTree::get_selected(self$Conditions))
        ) |>
        dplyr::mutate(HasConditionFlag = ifelse(HasCondition == "True", 1, ifelse(HasCondition == "False", 0, NA))) |>
        dplyr::select(record_id, Condition, HasCondition, HasConditionFlag) |>
        dplyr::group_by(record_id) |>
        dplyr::summarise(HasAnyConditionFlag = sum(HasConditionFlag), .groups = "drop") |>
        dplyr::mutate(HasAnyConditionFlag = ifelse(HasAnyConditionFlag > 0, "Yes", "No")) |>
        tidyr::drop_na()

      self$BaseData <- dplyr::inner_join(base_data, condition_data, by = "record_id") |>
        dplyr::mutate(HasAnyConditionFlag = forcats::fct_relevel(HasAnyConditionFlag, self$groupBaselineLabel)) |>
        dplyr::filter(!is.na(HasAnyConditionFlag))

    }

  )
)
