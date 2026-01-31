box::use(
  R6[R6Class],
  glue[glue, glue_collapse],
  dplyr[filter, select, rename, mutate],
  purrr[pmap]
)

box::use(
  app/logic/inputs/inputs_base[InputsManagerBase]
)

#' @export
PreCalculatedFeatureAnalysisInputsManager <- R6Class(
  "PreCalculatedFeatureAnalysisInputsManager",
  inherit = InputsManagerBase,
  private = list(),
  active = list(
    params = function(value) {
      return(
        ifelse(
          is.null(self$Covariates),
          "none",
          glue_collapse(self$Covariates, ";")
        )
      )
    }
  ),
  public = list(
    initialize = function(app_config, analysis_config, input_config) {
      super$initialize(app_config, analysis_config, input_config)
    },
    validate_study_data = function() {
      return(self$Study != "")
    },
    get_study_data = function() {
      self$FeatureData <- self$remote_files$get_pre_calculated_data(self$namespace) |>
          filter(
            samples == glue_collapse(self$Karyotype, ";"),
            selected_parameters == self$params
          ) |>
          select(-c(samples, selected_parameters)) |>
          mutate(
            karyotypes = glue_collapse(self$Karyotype, ";"),
            ages = glue_collapse(self$Age, ";"),
            sexes = glue_collapse(self$Sex, ";")
          )
      return(invisible(self$FeatureData))

    }
  )
)
