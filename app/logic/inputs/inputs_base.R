#' @export
InputsManagerBase <- R6::R6Class(
  "InputsManagerBase",
  private = list(),
  active = list(
    Studies = function(value) {
      return(
        self$input_config$studies |>
          dplyr::filter(Values %in% self$experimentIDs)
      )
    },
    StudyLabel = function(value) {
      return(
        self$Studies |>
          dplyr::filter(Values == self$Study) |>
          dplyr::pull(Text)
      )
    },
    StudyData = function(value) {
      return(
        self$remote_files$get_experiment_data(self$Study)
      )
    },
    Karyotypes = function(value) {
      if (self$namespace == "Karyotype") {
        return(
          tibble::tibble(
            choiceNames = glue::glue(
              '<div>
                {glue::glue_collapse(self$input_config$karyotypes,sep = " vs. ")}
                <span
                  data-toggle="tooltip"
                  data-placement="auto right"
                  title=""
                  class="fas fa-info-circle gtooltip info-tooltip"
                  data-original-title="Test for differences between Trisomy 21 & Controls">
                </span>
              </div>'
            ),
            choiceValues = glue::glue_collapse(self$input_config$karyotypes, sep = ";")
          )
        )
      } else if (self$namespace == "Comorbidity") {
        return(
          tibble::tibble(
            choiceNames =  self$input_config$karyotypes[1],
            choiceValues = self$input_config$karyotypes[1]
          )
        )
      } else {
        karyotype_input_counts <- self$StudyData |>
          dplyr::group_by(Analyte, Karyotype) |>
          dplyr::summarise(
            n = dplyr::n_distinct(LabID), .groups = "drop"
          ) |>
          dplyr::ungroup() |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarise(
            n = round(stats::median(n)), .groups = "drop"
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            sort = dplyr::case_when(
              Karyotype == "Trisomy 21" ~ 1,
              TRUE ~ 99
            ),
            choiceNames = glue::glue("{Karyotype} (n={n})"),
            choiceValues = Karyotype
          ) |>
          dplyr::arrange(sort)

        if (self$analysisType == "Continuous") {
          return(
            karyotype_input_counts |>
              dplyr::bind_rows(
                tibble::tibble(
                  Karyotype = glue::glue_collapse(self$input_config$karyotypes, sep = ";"),
                  n = NA,
                  sort = 999,
                  choiceNames = glue::glue(
                    '<div>{glue::glue_collapse(self$input_config$karyotypes, sep = " vs. ")}
                        <span
                          data-toggle="tooltip"
                          data-placement="auto right"
                          title=""
                          class="fas fa-info-circle gtooltip info-tooltip"
                          data-original-title="Test for differences in {self$analysisVariable}
                          trajectories between Trisomy 21 & Controls">
                        </span>
                      </div>'
                  ),
                  choiceValues = glue::glue_collapse(self$input_config$karyotypes, sep = ";")
                )
              ) |>
              dplyr::arrange(sort)
          )
        } else {
          return(
            karyotype_input_counts
          )
        }
      }
    },
    Sexes = function() {
      return(
        self$input_config$sexes
      )
    },
    Ages = function() {
      return(
        c(min(self$input_config$ages), max(self$input_config$ages))
      )
    },
    ConditionChoices = function(value) {
      return(
        self$input_config$ConditionChoices |>
          dplyr::select(ConditionClass, Condition)
      )
    },
    SelectedConditionList = function(value) {
      return(
        shinyTree::get_selected(self$Conditions, "classid") |>
          unlist() |>
          tibble::tibble() |>
          purrr::set_names("selected") |>
          dplyr::distinct() |>
          dplyr::arrange() |>
          dplyr::summarise(text = stringr::str_c(selected, collapse = "<br />")) |>
          dplyr::pull()
      )
    },
    CovariateChoices = function(value) {
      return(
        setdiff(c("Age", "Sex"), self$analysisVariable)
      )
    },
    StatTestNames = function(value) {
      return(
        self$input_config$statTestschoiceNames
      )
    },
    StatTestValues = function(value) {
      return(
        self$input_config$statTests
      )
    }, 
    AdjustmentMethodNames = function(value) {
      return(
        self$input_config$adjustmentMethodsNames
      )
    },
    AdjustmentMethodValues = function(value) {
      return(
        self$input_config$adjustmentMethods
      )
    }

  ),
  public = list(
    application_id = NULL,
    applicationName = NULL,
    app_config = NULL,
    namespace = NULL,
    remoteDB = NULL,
    remote_files = NULL,
    localDB = NULL,
    input_config = NULL,
    analysisVariable = "",
    analysisVariableLabel = "",
    analysisType = "",
    experimentIDs = "",
    analytesLabel = "Analytes",
    groupBaselineLabel = "",
    FoldChangeVar = "log2FoldChange",
    SignificanceVariable = "-log10pvalue",

    Study = NULL,
    Platform = NULL,
    CellType = NULL,
    Karyotype = NULL,
    Conditions = NULL,
    Sex = NULL,
    Age = NULL,
    FilterLowCount = NULL,
    StatTest = NULL,
    Covariates = NULL,
    AdjustmentMethod = NULL,
    Adjusted = FALSE,
    SignificanceLabel = "p-value",

    FeatureData = NULL,
    initialize = function(app_config, analysis_config, input_config) {

      self$remote_files <- app_config$remote_files
      self$remoteDB <- app_config$remote_db
      self$input_config <- input_config
      namespace_config <- analysis_config
      self$application_id <- app_config$application_id
      self$applicationName <- namespace_config$ApplicationName
      self$namespace <- namespace_config$Namespace
      self$analysisVariable <- namespace_config$AnalysisVariableName
      self$analysisVariableLabel <- namespace_config$AnalysisVariableLabel
      self$analysisType <- namespace_config$AnalysisType
      self$experimentIDs <- stringr::str_split_1(namespace_config$ExperimentIDs, "\\|")

    },

    getGetDataButtonClass = function() {
      stop("implement getGetDataButtonClass")
    },

    setConditionTreeAttributes = function(tree) {
      stop("implement setConditionTreeAttributes")
    },

    getDisabledInputClass = function(input_name) {
      stop("implement getDisabledInputClass")
    },

    getHiddenInputClass = function(input_name) {
      stop("implement getHiddenInputClass")
    },

    addInputSpecialClass = function(input_name, class = c("disabled", "hide")) {
      stop("implement addInputSpecialClass")
    },

    validate_study_data = function() {
      stop("implement validate_study_data")
    },

    get_study_data = function() {
      stop("implement get_study_data")
    }
  )
)
