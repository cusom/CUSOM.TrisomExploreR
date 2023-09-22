#' R6 Class to manage Clincal Data Application
#' @description
#' Sets application-wide configurations, module configurations,
#' analysis configurations and default input values
#' @field app_config - list - application-wide configrations (applicationid, title, label, etc.)
#' @field module_config - list - module-namespace configurations
#' @field analysis_config - list - namespace-specific analysis options / configurations
#' @field input_config - list - application-wide default input values
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @importFrom arrow open_dataset
#' @export
ClinicalDataAppManager <- R6::R6Class(
  "ClinicalDataAppManager",
  private = list(),
  public = list(
    app_config = list(
      ApplicationId = NULL,
      localDBLocation = NULL,
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
      Enrollments = NULL,
      SampleDetail = NULL,
      AnalysisAvailable = NULL,
      probandRelationships = NULL,
      karyotypes = NULL,
      sexes = NULL,
      ages = NULL,
      ParticipantConditions = NULL,
      Conditions = NULL,
      ConditionClasses = NULL,
      ConditionChoices_df = NULL,
      ConditionsChoices = NULL,
      DiagnosedConditions = NULL,
      ConditionClassData = NULL,
      PlatformExperiments = NULL
    ),

    #' @description
    #' Create a new instance of a ClinicalDataAppManager object
    #' @param ApplicationId string - ApplicationId
    #' @param remoteDB R6 class to manage remote database queries
    #' @param localDB R6 class to manange local database queries
    #' @return A new `ClinicalDataAppManager` object.
    initialize = function(ApplicationId, remoteDB, localDB) {

      self$app_config$ApplicationId <- ApplicationId

      application_namespace_config <- remoteDB$getQuery(
        "SELECT * FROM [app].[ShinyAppConfig]
           WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))",
        tibble::tibble("ApplicationId" = ApplicationId)
      )

      self$app_config$Namespaces <- NULL
      self$app_config$applicationTitle <- application_namespace_config$applicationName[1]
      self$app_config$applicationLabel <-  application_namespace_config$applicationLabel[1]
      self$app_config$applicationURL <- ifelse(
        application_namespace_config$Environment[1] == "Production",
        "https://www.trisome.org/explorer",
        "https://www.trisome.org/explorer-internal"
      )

      self$app_config$applicationLinks <- remoteDB$getQuery(
        "SELECT [LinkedApplicationLabel] [label], [LinkedApplicationImageURL][imageURL],
          [LinkedApplicationURL] [link], [IsCurrentApplication]
          FROM [app].[vw_ShinyApplicationApplicationLinks]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY LinkDisplayOrder",
        tibble::tibble("ApplicationId" = ApplicationId)
      )

      self$module_config <- NULL

      self$analysis_config <- NULL

      inputs <- jsonlite::fromJSON("Remote_Data/inputs.json")

      self$input_config$platforms <- inputs$platforms

      self$input_config$PlatformExperiments <- arrow::open_dataset("Remote_Data/platform_experiments") |>
        dplyr::collect()

      self$input_config$Enrollments <- arrow::open_dataset("Remote_Data/participants") |>
        dplyr::collect() |>
        dplyr::group_by(Karyotype) |>
        dplyr::summarise(ParticipantCount = n())

      self$input_config$SampleDetail <- arrow::open_dataset("Remote_Data/sample_detail") |>
        dplyr::collect()

      self$input_config$AnalysisAvailable <- self$input_config$PlatformExperiments |>
        dplyr::filter(!is.na(PlatformGroup), !is.na(TotalSamples), !is.na(ExperimentStudyName)) |>
        dplyr::mutate(label = glue::glue("{ExperimentStudyName} - {PlatformDisplayName}")) |>
        dplyr::arrange(PlatformGroup) |>
        dplyr::group_by(PlatformGroup) |>
        dplyr::group_map(
          ~ purrr::set_names(.x$ExperimentID, .x$label)
        ) |>
        purrr::set_names(
          self$input_config$PlatformExperiments |>
            dplyr::filter(!is.na(PlatformGroup), !is.na(TotalSamples), !is.na(ExperimentStudyName)) |>
            dplyr::distinct(PlatformGroup) |>
            dplyr::arrange(PlatformGroup) |>
            dplyr::pull()
        )

      self$input_config$probandRelationships <- arrow::open_dataset("Remote_Data/participants") |>
        dplyr::collect() |>
        dplyr::distinct(ProbandRelationship)

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
        dplyr::distinct(AgeAtTimeOfVisit) |>
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
        dplyr::arrange(ConditionClass) |>
        dplyr::pull()

      self$input_config$ConditionsChoices <- arrow::open_dataset("Remote_Data/participant_conditions") |>
        dplyr::collect() |>
        dplyr::filter(HasCondition == "True") |>
        dplyr::select(record_id, ConditionClass, Condition) |>
        dplyr::group_by(ConditionClass, Condition) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")  |>
        dplyr::filter(n >= 5) |>
        tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
        tidyr::drop_na() |>
        dplyr::group_by(ConditionClass) |>
        dplyr::group_map(
          ~ purrr::set_names(stringr::str_c(.x$Condition))
        ) |>
        purrr::set_names(
          arrow::open_dataset("Remote_Data/participant_conditions") |>
            dplyr::collect() |>
            dplyr::filter(HasCondition == "True") |>
            dplyr::select(ConditionClass) |>
            tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
            dplyr::distinct() |>
            tidyr::drop_na() |>
            dplyr::arrange(ConditionClass) |>
            dplyr::pull()
        )

    }
  )
)
