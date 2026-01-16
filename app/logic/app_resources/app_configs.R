box::use(
  app/view/custom_ui/ui_utils
)

box::use(
  app/logic/app_resources/data_services[AzureRemoteDataFileManager, ODBCQueryManager]
)

#' R6 Class to manage core TrisomExploreR applications
#' @description
#'  R6 Class to manage core TrisomExploreR applications
#' @field app_config - list -
#' @field module_config - list -
#' @field analysis_config - list -
#' @field input_config - list -
#' @importFrom arrow open_dataset
#' @export
TrisomExplorerAppManager <- R6::R6Class(
  "TrisomExplorerAppManager",
  private = list(

  ),
  active = list(

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
    initialize = function(application_id, config_file_name = "config.yml") {

      self$application_id <- application_id

      self$remote_db = ODBCQueryManager$new(
        conn_args = config::get(file = config_file_name, "database")
      )

      self$remote_files = AzureRemoteDataFileManager$new(
        account_name = config::get(file = config_file_name, "remote_storage")$storage_account_name,
        key = config::get(file = config_file_name, "remote_storage")$storage_key,
        container_name = glue::glue("htp-{tolower(application_id)}")
      )

      self$app_config$application_id <- application_id

      download_files <- self$remote_db$getQuery(
          "SELECT [downloadRemoteFiles] FROM [te].[Application] WHERE ApplicationID = ?",
          tibble::tibble("ApplicationId" = application_id)
        ) |> dplyr::pull()

      self$remote_files$download_files(download_files)

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
        dplyr::select(ApplicationId, Namespace, ExperimentIDs,
            AnalysisVariableName, AnalysisVariableLabel, AnalysisType,
            AnalysisVariableBaselineLabel, AnalysisVolcanoPlotTopAnnotation)

      inputs <- jsonlite::fromJSON("Remote_Data/inputs.json")

      self$input_config$statTestschoiceNames <- purrr::pmap(
        inputs$stat_tests,
        ui_utils$createTooltip
      )

      self$input_config$adjustmentMethodsNames <- purrr::pmap(
        inputs$adj_methods,
        ui_utils$createTooltip
      )

      self$input_config$platforms <- inputs$platforms

      self$input_config$experimentIDs <- inputs$experiment_ids

      self$input_config$karyotypes <- arrow::open_dataset("Remote_Data/participants") |>
        dplyr::collect() |>
        dplyr::distinct(Karyotype) |>
        dplyr::pull()

      self$input_config$sexes <- arrow::open_dataset("Remote_Data/participants") |>
        dplyr::collect() |>
        dplyr::distinct(Sex) |>
        dplyr::pull()

      self$input_config$ages <- arrow::open_dataset("Remote_Data/participant_encounter") |>
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

      self$input_config$Conditions <- arrow::open_dataset("Remote_Data/participant_conditions") |>
        dplyr::collect() |>
        dplyr::distinct(Condition) |>
        dplyr::pull()

      self$input_config$ConditionClasses <- arrow::open_dataset("Remote_Data/participant_conditions") |>
        dplyr::collect() |>
        dplyr::distinct(ConditionClass) |>
        tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
        tidyr::drop_na() |>
        dplyr::select(ConditionClass) |>
        dplyr::distinct() |>
        dplyr::pull()

      self$input_config$ConditionChoices <- arrow::open_dataset("Remote_Data/participant_conditions") |>
        dplyr::collect() |>
        dplyr::filter(HasCondition == "True") |>
        dplyr::select(LabID, ConditionClass, Condition) |>
        dplyr::select(LabID, ConditionClass, Condition) |>
        dplyr::group_by(ConditionClass, Condition) |>
        dplyr::summarize(n = dplyr::n_distinct(LabID), .groups = "drop")  |>
        dplyr::filter(n >= 5) |>
        dplyr::left_join(
          arrow::open_dataset("Remote_Data/participant_conditions") |>
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
