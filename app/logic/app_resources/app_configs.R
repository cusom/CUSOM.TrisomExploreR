box::use(
  R6[R6Class],
  config[get],
  glue[glue],
  dplyr[select, arrange, distinct, pull, filter, left_join, mutate, 
    group_by, collect, summarise, n, n_distinct, reframe, case_when,
    bind_rows, rename, rename_with],
  tibble[tibble, deframe],
  tidyr[drop_na, separate_rows],
  purrr[pmap]
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
    eval(parse(text = glue("box::use({class_module}[{class_name}])")), envir = environment())
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
TrisomExplorerAppManager <- R6Class(
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
    config_file_name = NULL,
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
      Namespaces = tibble()
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
      load_condition_data = TRUE,
      clear_data_dir = TRUE
    ) {
      self$application_id <- application_id
      self$config_file_name <- config_file_name

      self$remote_db <- ODBCQueryManager$new(
        conn_args = get(file = self$config_file_name, "database")
      )

      self$remote_files <- AzureRemoteDataFileManager$new(
        account_name = get(file = self$config_file_name, "remote_storage")$storage_account_name,
        key = get(file = self$config_file_name, "remote_storage")$storage_key,
        container_name = glue("htp-{tolower(application_id)}"),
        download_mode = "on demand",
        clear_data_dir = clear_data_dir
      )

      self$app_config$application_id <- application_id

      self$namespace_config <- self$remote_db$getQuery(
        "SELECT * FROM [te].[vw_ApplicationNamespaceConfig]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY DisplayOrder",
        tibble("ApplicationId" = application_id)
      )

      self$app_config$Namespaces <- self$namespace_config |>
        arrange(DisplayOrder) |>
        select(DisplayOrder, Namespace) |>
        deframe()

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
        tibble("ApplicationId" = application_id)
      )

      self$module_config <- self$namespace_config |>
        select(ApplicationId, Namespace, TabText, TabIcon,
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

      self$input_config$statTestschoiceNames <- pmap(
        self$inputs$stat_tests,
        ui_utils$createTooltip
      )

      self$input_config$adjustmentMethodsNames <- pmap(
        self$inputs$adj_methods,
        ui_utils$createTooltip
      )

      self$input_config$platforms <- self$inputs$platforms

      self$input_config$experimentIDs <- self$inputs$experiment_ids
    },

    load_participant_data = function() {
      self$input_config$karyotypes <- self$participant_data |>
        collect() |>
        distinct(Karyotype) |>
        pull()

      self$input_config$sexes <- self$participant_data |>
        collect() |>
        distinct(Sex) |>
        pull()
    },

    load_encounter_data = function() {
      self$input_config$ages <- self$encounter_data |>
        collect() |>
        drop_na() |>
        summarise(
          min = round(min(AgeAtTimeOfVisit)),
          max = round(max(AgeAtTimeOfVisit)) + 1
        ) |>
        reframe(
          age = seq(min, max, 1)
        ) |>
        pull()
    },

    load_condition_data = function() {
      self$input_config$Conditions <- self$condition_data |>
        collect() |>
        distinct(Condition) |>
        pull()

      self$input_config$ConditionClasses <- self$condition_data |>
        collect() |>
        distinct(ConditionClass) |>
        separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
        drop_na() |>
        select(ConditionClass) |>
        distinct() |>
        pull()

      self$input_config$ConditionChoices <- self$condition_data |>
        collect() |>
        filter(HasCondition == "True") |>
        select(LabID, ConditionClass, Condition) |>
        select(LabID, ConditionClass, Condition) |>
        group_by(ConditionClass, Condition) |>
        summarise(n = n_distinct(LabID), .groups = "drop")  |>
        filter(n >= 5) |>
        left_join(
          self$condition_data |>
            collect() |>
            filter(!is.na(ConditionCensorshipAgeGroup)) |>
            distinct(Condition, ConditionCensorshipAgeGroup)

          , by = "Condition"
        ) |>
        mutate(
          AgeCensor = case_when(
            !is.na(ConditionCensorshipAgeGroup) ~ ConditionCensorshipAgeGroup,
            TRUE ~ ""
          )
        ) |>
        select(-ConditionCensorshipAgeGroup, n) |>
        separate_rows(sep = ";", "ConditionClass", convert = TRUE)
    },

    get_module_config = function(namespace) {
      self$app_module_config |>
          filter(tolower(Namespace) == tolower(namespace))
    },

    get_analysis_config = function(namespace) {
      return(
        self$analysis_config |>
          mutate(applicationName = self$app_config$applicationTitle) |>
          filter(
            tolower(Namespace) == tolower(namespace) |
              tolower(AnalysisVariableLabel) == tolower(namespace) |
              tolower(AnalysisVariableName) == tolower(namespace)
          ) |>
          distinct()
      )
    },

    get_input_config = function(namespace) {
      return(self$input_config)
    }
  )
)


#' @export
TOFAAppManager <- R6Class(
  "TOFAAppManager",
  inherit = TrisomExplorerAppManager,
  private = list(),
  active = list(
    datasets = function(value) {
      return(
        self$remote_files$blobs |>
          select(ExperimentID) |>
          drop_na() |>
          rename(Values = ExperimentID) |>
          mutate(
            Text = gsub("TOFA_data_DCC_TrisomExplorer_v2.3_DATASETS_", "", Values),
            URL = NA, 
            TooltipText = "", 
            ShowTooltip = FALSE, 
            FieldSet = case_when(
              grepl("Endpoints", Values,  ignore.case = TRUE) ~ "Endpoints",
              grepl("Nulisa", Values,  ignore.case = TRUE) ~ "Nulisa",
              grepl("Olink", Values, ignore.case = TRUE) ~ "Olink",
              TRUE ~ "Other"
            )
          ) |>
          select(Values, Text, URL, TooltipText, ShowTooltip, FieldSet)
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
    initialize = function(app_config) {
      
      super$initialize(app_config$application_id, config_file_name = "config.yml", TRUE, FALSE, FALSE, FALSE, FALSE)

      self$remote_files <- AzureRemoteDataFileManager$new(
        account_name = get(file = self$config_file_name, "remote_storage")$storage_account_name,
        key = get(file = self$config_file_name, "remote_storage")$storage_key,
        container_name = "htp-0847b484-bf9a-4d3d-9c96-992fc10d445d",
        download_mode = "on demand",
        clear_data_dir = FALSE
      )

    },
    load_participant_data = function() {
      self$input_config$karyotypes <- self$participant_data |>
        distinct(DownSyndromeStatus) |>
        pull()

      self$input_config$sexes <- self$participant_data |>
        distinct(Sex) |>
        pull()
    },
    load_encounter_data = function() {
      self$input_config$ages <- self$encounter_data |>
        drop_na() |>
        summarise(
          min = round(min(Age_at_visit_in_days)),
          max = round(max(Age_at_visit_in_days)) + 1
        ) |>
        reframe(
          age = seq(min, max, 1)
        ) |>
        pull()
    }
  )
)
