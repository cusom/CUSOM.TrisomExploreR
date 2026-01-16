#' @export
FeatureAnalysisInputsManager <- R6::R6Class(
  "FeatureAnalysisInputsManager",
  private = list(),
  active = list(
    Studies = function(value) {
      return(
        jsonlite::fromJSON("Remote_Data/inputs.json")$study_choices |>
          as.data.frame() |>
          dplyr::filter(Values %in% self$experimentIDs)
      )
    },
    StudyLabel = function(value) {
      return(
        jsonlite::fromJSON("Remote_Data/inputs.json", flatten = TRUE) |>
          purrr::pluck("study_choices") |>
          as.data.frame() |>
          dplyr::filter(Values == self$Study) |>
          dplyr::pull(Text)
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
  
        karyotype_input_counts <- arrow::open_dataset("Remote_Data/feature_data") |>
          dplyr::filter(ExperimentID == self$Study) |>
          dplyr::collect() |>
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
    applicationName = NULL,
    namespace = NULL,
    remoteDB = NULL,
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


    #' @description
    #' Create a new instance of a FeatureAnalysisManager
    #' @param applicationName string - name of application
    #' @param id string - namespace for this instance
    #' @param namespace_config list - configurations for this namespace
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(analysis_config, input_config) {
    
      self$input_config <- input_config
        
      namespace_config <- analysis_config

      self$applicationName <- namespace_config$ApplicationName

      self$namespace <- namespace_config$Namespace
      self$analysisVariable <- namespace_config$AnalysisVariableName
      self$analysisVariableLabel <- namespace_config$AnalysisVariableLabel
      self$analysisType <- namespace_config$AnalysisType
  
      self$experimentIDs <- stringr::str_split_1(namespace_config$ExperimentIDs, "\\|")
    
    },

    #' @description
    #' helper function to toggle "Get Data" button class based on conditions
    #' enabled / green if study is chosen, organge / disabled otherwise
    #' enabled / green if namespace is comorb / conditions are chosen, diabled otherwise
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

    #' @description
    #' helper function to get hierarchical condition input values
    #' @param conditions - tibble of condition hierarchy choices
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

    #' @description
    #' helper function to disable an input if it matches the name of the namespace
    #' @param input_name name of input widget
    getDisabledInputClass = function(input_name) {
      if (self$analysisVariable == input_name) {
        return(
          "shinyjs-disabled"
        )
      }
    },

    #' @description
    #' helper function to hide an input if it matches the name of the namespace
    #' @param input_name name of input widget
    getHiddenInputClass = function(input_name) {
      if (self$analysisVariable == input_name) {
        return(
          "shinyjs-hide"
        )
      }
    },

    #' @description
    #' helper function to add a diabled or hidden class to an input if it matches the name of the namespace
    #' @param input_name name of input widget
    #' @param class string - one of `disabled` or `hide`
    addInputSpecialClass = function(input_name, class = c("disabled", "hide")) {
      class <- match.arg(class)
      if (self$analysisVariable == input_name) {
        return(
          glue::glue("shinyjs-{class}")
        )
      }
    },

    validate_study_data = function() {
      return(self$Study != "")
    },

    #' @description
    #' Get / set sample level data with filers applied
    get_study_data = function() {
  
      self$FeatureData <- arrow::open_dataset("Remote_Data/feature_data") |>
        dplyr::filter(
          ExperimentID == self$Study
        ) |>
        dplyr::collect() |>
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
