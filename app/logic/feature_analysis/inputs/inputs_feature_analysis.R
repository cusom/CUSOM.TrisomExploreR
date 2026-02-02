box::use(
  app/logic/feature_analysis/inputs/inputs_base[InputsManagerBase]
)

#' @export
FeatureAnalysisInputsManager <- R6::R6Class(
  "FeatureAnalysisInputsManager",
  inherit = InputsManagerBase,
  private = list(),
  active = list(),
  public = list(
    initialize = function(app_config, analysis_config, input_config) {
      super$initialize(app_config, analysis_config, input_config)
    },

    getGetDataButtonClass = function() {
      if (is.null(self$Study)) {
        return("refresh-btn shinyjs-disabled")
      } else {
        if (self$namespace == "Comorbidity" & is.null(self$Conditions)) {
          return("refresh-btn shinyjs-disabled")
        } else {
          return("refresh-ready-btn shinyjs-enabled")
        }
      }
    },

    setConditionTreeAttributes = function(tree) {

      # tree <- conditions |>
      #   CUSOMShinyHelpers::dfToTree()

      if (!is.null(self$Conditions)) {
        selected_nodes <- shinyTree::get_selected(self$Conditions, format = "classid") |>
          unlist() |>
          tibble::as_tibble() |>
          dplyr::pull()

        if (length(selected_nodes) > 0) {
          for (i in seq_along(tree)) {
            if (is.list(tree[i])) {
              for (node in names(tree[i][[1]])) {
                if (node %in% selected_nodes) {
                  attr(tree[[i]][[node]], "stselected") <- TRUE
                  attr(tree[[i]][[node]], "stopened") <- TRUE
                }
              }
            }
          }
        }
      }
      return(
        tree
      )
    },

    getDisabledInputClass = function(input_name) {
      if (self$analysisVariable == input_name) {
        return(
          "shinyjs-disabled"
        )
      }
    },

    getHiddenInputClass = function(input_name) {
      if (self$analysisVariable == input_name) {
        return(
          "shinyjs-hide"
        )
      }
    },

    validate_study_data = function() {
      return(self$Study != "")
    },

    get_study_data = function() {

      self$FeatureData <- self$StudyData |>
        dplyr::select(LabID, Karyotype, Sex, Age, BMI, Analyte, MeasuredValue, Measurement) |>
        dplyr::filter(
          Age >= min(self$Age),
          Age <= max(self$Age),
          Sex %in% self$Sex,
          Karyotype %in% unlist(stringr::str_split(self$Karyotype, pattern = ";"))
        ) |>
        dplyr::filter(!is.na(!!rlang::sym(self$analysisVariable))) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})")
        )

      return(invisible(self$FeatureData))

    }
  )
)
