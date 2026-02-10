box::use(
  R6[R6Class],
  glue[glue, glue_collapse],
  dplyr[filter, select, rename, mutate, case_when, bind_rows, arrange],
  tibble[tibble],
  purrr[pmap, pluck]
)

box::use(
  app/logic/feature_analysis/inputs/inputs_base[InputsManagerBase]
)

#' @export
PreCalculatedFeatureAnalysisInputsManager <- R6Class(
  "PreCalculatedFeatureAnalysisInputsManager",
  inherit = InputsManagerBase,
  private = list(),
  active = list(
    Karyotypes = function(value) {
      if (self$namespace == "Karyotype") {
        return(
          tibble(
            choiceNames = glue(
              '<div>
                {glue_collapse(self$input_config$karyotypes,sep = " vs. ")}
                <span
                  data-toggle="tooltip"
                  data-placement="auto right"
                  title=""
                  class="fas fa-info-circle gtooltip info-tooltip"
                  data-original-title="Test for differences between Trisomy 21 & Controls">
                </span>
              </div>'
            ),
            choiceValues = glue_collapse(self$input_config$karyotypes, sep = ";")
          )
        )
      } else {
        return(
          self$remote_files$get_remote_file_data("input") |>
            pluck("whole_blood_karyotype_counts") |>
            as.data.frame() |>
            mutate(
              sort = case_when(
                Karyotype == "Trisomy 21" ~ 1,
                TRUE ~ 99
              ),
              choiceNames = glue("{Karyotype} (n={n})"),
              choiceValues = Karyotype
            ) |>
            bind_rows(
              tibble(
                Karyotype = glue_collapse(self$input_config$karyotypes, sep = ","),
                n = NA,
                sort = 999,
                choiceNames =
                  glue(
                    '<div>{glue_collapse(self$input_config$karyotypes,sep = " vs. ")}
                      <span
                        data-toggle="tooltip"
                        data-placement="auto right"
                        title=""
                        class="fas fa-info-circle gtooltip info-tooltip"
                        data-original-title="Test for differences in trajectories between Trisomy 21 & Controls">
                      </span>
                    </div>'
                  ),
                choiceValues = glue_collapse(self$input_config$karyotypes, sep = ";")
              )
            ) |>
            arrange(sort)
        )
      }
    },
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
