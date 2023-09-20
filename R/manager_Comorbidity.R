#' R6 Class to manage Comorbidity Analysis
#' @description
#' Subclass of Feature Analysis Manager R6 Class to enable comorbidity-specific analysis.
#'
#' @import dplyr
#' @import tibble
#' @importFrom stringr str_split
#' @import glue
#' @import tidyr
#' @import shinyTree
#' @importFrom arrow open_dataset
#' @export
ComorbidityManager <- R6::R6Class(
  "ComorbidityManager",
  inherit = FeatureAnalysisManager,
  private = list(),
  public = list(

    #' @description
    #' Create a new instance of ComorbidityManager object
    #' @param applicationName string - applicationName
    #' @param id string - namespace for this class
    #' @param namespace_config tibble - configuration values for this namespace instance of the object
    #' @param remoteDB R6 class to manage remote database queries
    #' @param localDB R6 class to manange local database queries
    #' @return A new `ComorbidityManager` object.
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB) {
      super$initialize(applicationName, id, namespace_config, remoteDB, localDB)
    },

    #' @description
    #' set BaseData for comorbidity analysis - custom logic that overrides `getBaseData` method from
    #'  parent `FeatureAnalysisManager` class
    #' @return none
    getBaseData = function() {

      base_data <- arrow::open_dataset("data/feature_data") |>
        dplyr::filter(
          ExperimentID == self$Study
        ) |>
        dplyr::collect() |>
        dplyr::select(record_id, LabID, Karyotype, Sex, Age, BMI, Analyte, MeasuredValue, Measurement) |>
        dplyr::filter(
          Age >= min(self$Age),
          Age <= max(self$Age),
          Sex %in% self$Sex,
          Karyotype %in% unlist(stringr::str_split(self$Karyotype, pattern = ";"))
        ) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})")
        )

      condition_data <- arrow::open_dataset("data/participant_conditions") |>
        dplyr::collect() |>
        dplyr::filter(Condition %in% unlist(shinyTree::get_selected(self$Conditions))) |>
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
