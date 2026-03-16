box::use(
  dplyr[select]
)

box::use(
  app/logic/shared/ui_utils,
  app/logic/app_resources/data_services[AzureRemoteDataFileManager, ODBCQueryManager]
)

#' @export
create_app_settings <- function(application_id,
                                app_config = NULL,
                                config_file_name = "config.yml") {

  class_name <- "TrisomExplorerAppManager"
  class_module <- "app/logic/app_resources/app_configs"

  if (!is.null(app_config$app_settings_class) && nzchar(app_config$app_settings_class)) {
    class_name <- app_config$app_settings_class
  }

  if (!is.null(app_config$app_settings_class_module) && nzchar(app_config$app_settings_class_module)) {
    class_module <- app_config$app_settings_class_module
  }

  class_generator <- NULL

  if (identical(class_name, "TrisomExplorerAppManager") &&
      identical(class_module, "app/logic/app_resources/app_configs")) {
    class_generator <- TrisomExplorerAppManager
  } else {
    eval(parse(text = glue::glue("box::use({class_module}[{class_name}])")), envir = environment())
    class_generator <- get(class_name, envir = environment(), inherits = TRUE)
  }

  init_args <- app_config$app_settings_args

  if (is.null(init_args)) {
    init_args <- list()
  }

  if (!is.list(init_args)) {
    stop("app_settings_args must be a list.", call. = FALSE)
  }

  if (is.null(init_args$application_id)) {
    init_args$application_id <- application_id
  }

  if (is.null(init_args$config_file_name)) {
    init_args$config_file_name <- config_file_name
  }

  do.call(class_generator$new, init_args)
}

#' @export
TrisomExplorerAppManager <- R6::R6Class(
  "TrisomExplorerAppManager",
  private = list(

  ),
  active = list(
    inputs = function(value) {
      return(
        self$remote_files$get_remote_file_data("inputs.json")
      )
    },
    participant_data = function(value) {
      return(
        self$remote_files$get_remote_file_data("participants")
      )
    },
    encounter_data = function(value) {
      return(
        self$remote_files$get_remote_file_data("encounter")
      )
    },
    condition_data = function(value) {
      return(
        self$remote_files$get_remote_file_data("conditions")
      )
    }
  ),
  public = list(
    application_id = NULL,
    remote_db = NULL,
    remote_files = NULL,
    namespace_config = NULL,

    app_config = list(
      application_id = NULL,
      applicationTitle = NULL,
      applicationLabel = NULL,
      applicationURL = NULL,
      applicationLinks = NULL,
      tutorials = NULL,
      Namespaces = tibble::tibble()
    ),

    module_config = NULL,
    analysis_config = NULL,

    input_config = list(
      statTests = c("Linear Model", "Wilcoxon test"),
      statTestTibble = NULL,
      statTestschoiceNames = NULL,
      adjustmentMethods = c("Benjamini-Hochberg (FDR)", "none"),
      adjustmentMethodsTibble = NULL,
      adjustmentMethodsNames = NULL,

      platforms = NULL,
      PlatformExperiments = NULL,
      Queryplatforms = NULL,
      Comparisonplatforms = NULL,
      experimentIDs = NULL,
      studies = NULL,
      studiesTibble = NULL,
      studyChoiceNames = NULL,
      studyNames= NULL,
      LabIDs = NULL,
      karyotypes = NULL,
      sexes = NULL,
      ages = NULL,
      Conditions = NULL,
      ConditionClasses = NULL,
      ConditionChoices = NULL,
      CellTypes = NULL,
      Genes = NULL,
      Analytes = NULL
    ),

    #' @description
    #' Create a new instance of a TrisomExplorerAppManager
    #' @param ApplicationId - string - application id
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(
      application_id,
      config_file_name = "config.yml",
      load_inputs = TRUE,
      load_participant_data = TRUE,
      load_encounter_data = TRUE,
      load_condition_data = TRUE
    ) {
      self$application_id <- application_id

      self$remote_db <- ODBCQueryManager$new(
        conn_args = config::get(file = config_file_name, "database")
      )

      self$remote_files <- AzureRemoteDataFileManager$new(
        account_name = config::get(file = config_file_name, "remote_storage")$storage_account_name,
        key = config::get(file = config_file_name, "remote_storage")$storage_key,
        container_name = glue::glue("htp-{tolower(application_id)}"),
        download_mode = "on demand"
      )

      self$app_config$application_id <- application_id

      self$namespace_config <- self$remote_db$getQuery(
        "SELECT * FROM [te].[vw_ApplicationNamespaceConfig]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY DisplayOrder",
        tibble::tibble("ApplicationId" = application_id)
      )

      self$app_config$Namespaces <- self$namespace_config |>
        dplyr::arrange(DisplayOrder) |>
        dplyr::select(DisplayOrder, Namespace) |>
        tibble::deframe()

      self$app_config$applicationTitle <- self$namespace_config$applicationName[1]
      self$app_config$applicationLabel <-  self$namespace_config$applicationLabel[1]
      self$app_config$applicationURL <- ifelse(
        self$namespace_config$environment[1] == "Production",
        "https://www.trisome.org/explorer",
        "https://www.trisome.org/explorer-internal"
      )

      self$app_config$applicationLinks <- self$remote_db$getQuery(
        "SELECT [LinkedApplicationLabel] [label], [LinkedApplicationImageURL][imageURL],
          [LinkedApplicationURL] [link], [IsCurrentApplication]
          FROM [app].[vw_ShinyApplicationApplicationLinks]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY LinkDisplayOrder",
        tibble::tibble("ApplicationId" = application_id)
      )

      self$module_config <- self$namespace_config |>
        dplyr::select(ApplicationId, Namespace, TabText, TabIcon,
        ModuleServerName, UseR6Class, R6ClassName)

      self$analysis_config <- self$namespace_config |>
        select(
          ApplicationId, Namespace, ExperimentIDs, UsesPreCalculatedData,
          AnalysisVariableName, AnalysisVariableLabel, AnalysisType,
          AnalysisVariableBaselineLabel, AnalysisVolcanoPlotTopAnnotation
        )

      if (load_inputs) {
        self$load_inputs()
      }

      if (load_participant_data) {
        self$load_participant_data()
      }

      if (load_encounter_data) {
        self$load_encounter_data()
      }

      if (load_condition_data) {
        self$load_condition_data()
      }

    },

    load_inputs = function() {
      self$input_config$studies <- self$inputs$study_choices |>
          as.data.frame()

      self$input_config$statTestschoiceNames <- purrr::pmap(
        self$inputs$stat_tests,
        ui_utils$createTooltip
      )

      self$input_config$adjustmentMethodsNames <- purrr::pmap(
        self$inputs$adj_methods,
        ui_utils$createTooltip
      )

      self$input_config$platforms <- self$inputs$platforms

      self$input_config$experimentIDs <- self$inputs$experiment_ids
    },

    load_participant_data = function() {
      self$input_config$karyotypes <- self$participant_data |>
        dplyr::collect() |>
        dplyr::distinct(Karyotype) |>
        dplyr::pull()

      self$input_config$sexes <- self$participant_data |>
        dplyr::collect() |>
        dplyr::distinct(Sex) |>
        dplyr::pull()
    },

    load_encounter_data = function() {
      self$input_config$ages <- self$encounter_data |>
        dplyr::collect() |>
        tidyr::drop_na() |>
        dplyr::summarise(
          min = round(min(AgeAtTimeOfVisit)),
          max = round(max(AgeAtTimeOfVisit)) + 1
        ) |>
        dplyr::reframe(
          age = seq(min, max, 1)
        ) |>
        dplyr::pull()
    },

    load_condition_data = function() {
      self$input_config$Conditions <- self$condition_data |>
        dplyr::collect() |>
        dplyr::distinct(Condition) |>
        dplyr::pull()

      self$input_config$ConditionClasses <- self$condition_data |>
        dplyr::collect() |>
        dplyr::distinct(ConditionClass) |>
        tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
        tidyr::drop_na() |>
        dplyr::select(ConditionClass) |>
        dplyr::distinct() |>
        dplyr::pull()

      self$input_config$ConditionChoices <- self$condition_data |>
        dplyr::collect() |>
        dplyr::filter(HasCondition == "True") |>
        dplyr::select(LabID, ConditionClass, Condition) |>
        dplyr::select(LabID, ConditionClass, Condition) |>
        dplyr::group_by(ConditionClass, Condition) |>
        dplyr::summarize(n = dplyr::n_distinct(LabID), .groups = "drop")  |>
        dplyr::filter(n >= 5) |>
        dplyr::left_join(
          self$condition_data |>
            dplyr::collect() |>
            dplyr::filter(!is.na(ConditionCensorshipAgeGroup)) |>
            dplyr::distinct(Condition, ConditionCensorshipAgeGroup)

          , by = "Condition"
        ) |>
        dplyr::mutate(
          AgeCensor = dplyr::case_when(
            !is.na(ConditionCensorshipAgeGroup) ~ ConditionCensorshipAgeGroup,
            TRUE ~ ""
          )
        ) |>
        dplyr::select(-ConditionCensorshipAgeGroup, n) |>
        tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE)
    },

    get_module_config = function(namespace) {
      self$app_module_config |>
          dplyr::filter(tolower(Namespace) == tolower(namespace))
    },

    get_analysis_config = function(namespace) {
      return(
        self$analysis_config |>
          dplyr::mutate(applicationName = self$app_config$applicationTitle) |>
          dplyr::filter(tolower(Namespace) == tolower(namespace))
      )
    },

    get_input_config = function(namespace) {
      return(self$input_config)
    }
  )
)


#' @export
TOFAAppManager <- R6::R6Class(
  "TOFAAppManager",
  inherit = TrisomExplorerAppManager,
  private = list(

  ),
  active = list(
    inputs = function(value) {
      return(
        self$remote_files$get_remote_file_data("inputs.json")
      )
    },
    participant_data = function(value) {
      return(
        self$remote_files$get_remote_file_data("PARTICIPANTS")
      )
    },
    encounter_data = function(value) {
      return(
        self$remote_files$get_remote_file_data("VISITS")
      )
    },
    datasets = function(value) {
      return(
        self$remote_files$get_remote_file_data("DATASETS")
      )
    },
    conditions = function(value) {
        return(
            self$participant_data |>
                select("condition" = Qualifying_feature) |>
                separate_rows(condition, sep = "; ") |>
                distinct()
        )
    },
    participant_conditions = function(value) {
        return(
            self$participant_data |>
                select(External_ParticipantID, Internal_ParticipantID, "condition" = Qualifying_feature) |>
                separate_rows(condition, sep = "; ") |>
                distinct()
        )
    },

    all_data = function(value) {
      return(
          self$participant_data |>
              inner_join(self$encounter_data, join_by(Internal_ParticipantID, External_ParticipantID)) |>
              left_join(self$datasets, join_by(Internal_ParticipantID, External_ParticipantID,
                  RecordID, TOFA_LabID, HTP_LabID, External_VisitID, Event_Name))
      )
    }
  ),
  public = list(
    initialize = function(application_id, config_file_name = "config.yml") {
      super$initialize(application_id, config_file_name, FALSE, FALSE, FALSE, FALSE)

      # additional initialization for TOFA app can go here

    },
    load_participant_data = function() {
      self$input_config$karyotypes <- self$participant_data |>
        dplyr::distinct(DownSyndromeStatus) |>
        dplyr::pull()

      self$input_config$sexes <- self$participant_data |>
        dplyr::distinct(Sex) |>
        dplyr::pull()
    },
    load_encounter_data = function() {
      self$input_config$ages <- self$encounter_data |>
        tidyr::drop_na() |>
        dplyr::summarise(
          min = round(min(Age_at_visit_in_days)),
          max = round(max(Age_at_visit_in_days)) + 1
        ) |>
        dplyr::reframe(
          age = seq(min, max, 1)
        ) |>
        dplyr::pull()
    }
  )
)
