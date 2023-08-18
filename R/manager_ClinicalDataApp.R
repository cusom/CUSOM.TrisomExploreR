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
    initialize = function(ApplicationId, remoteDB, localDB){

      self$app_config$ApplicationId <- ApplicationId

      ApplicationNamespaceConfig <- remoteDB$getQuery(
        "SELECT * FROM [app].[ShinyAppConfig]
           WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))"
        ,tibble::tibble('ApplicationId' = ApplicationId)
      )

      self$app_config$Namespaces <- NULL
      self$app_config$applicationTitle <- ApplicationNamespaceConfig$applicationName[1]
      self$app_config$applicationLabel <-  ApplicationNamespaceConfig$applicationLabel[1]
      self$app_config$applicationURL <- ifelse(ApplicationNamespaceConfig$Environment[1] == "Production",'https://www.trisome.org/explorer','https://www.trisome.org/explorer-internal')

      self$app_config$applicationLinks <- remoteDB$getQuery(
        "SELECT [LinkedApplicationLabel] [label], [LinkedApplicationImageURL][imageURL],[LinkedApplicationURL] [link], [IsCurrentApplication]
          FROM [app].[vw_ShinyApplicationApplicationLinks]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY LinkDisplayOrder"
        , tibble::tibble('ApplicationId' = ApplicationId)
      )

      self$app_config$tutorials <- remoteDB$getQuery(
        "[shiny].[GetApplicationTutorials] ?"
        , tibble::tibble('ApplicationId' = ApplicationId)
      )

      self$module_config <- NULL

      self$analysis_config <- NULL

      #self$plotlyCustomIcons = readRDS('config/plotlyCustomIcons.rds')

      self$input_config$statTests <- c("Linear Model","Wilcoxon test")

      self$input_config$statTestTibble <- tibble::tibble(
        "Text" = c("Linear Model","Wilcoxon test"),
        "URL" = c("https://en.wikipedia.org/wiki/Linear_regression","https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test"),
        "TooltipText" = c("Linear regression is a linear approach for modelling the relationship between a numerical response and one or more explanatory variables.<br /> <br />Click this icon to learn more..."
                          ,"The Wilcoxon Two-Sample signed-rank test (aka Mann-Whitney U test) is a non-parametric statistical hypothesis test. <br /> <br />Click this icon to learn more...")
      )

      self$input_config$statTestschoiceNames <- purrr::pmap(self$input_config$statTestTibble,CUSOMShinyHelpers::createTooltip)

      self$input_config$adjustmentMethodsTibble <- tibble::tibble(
          "Text" = c("Benjamini-Hochberg (FDR)","none"),
          "URL" = c("https://en.wikipedia.org/wiki/False_discovery_rate#Benjamini%E2%80%93Hochberg_procedure",""),
          "TooltipText" = c("Multiple testing correction refers to making statistical tests more stringent in order to counteract the problem of multiple testing.<br /> <br />The false discovery rate (FDR) is a method of conceptualizing the rate of type I errors in null hypothesis testing when conducting multiple comparisons. <br /> <br />Click here to learn more..."
                            ,""),
          "ShowTooltip" = c(TRUE,FALSE)
        )

      self$input_config$adjustmentMethods = c("Benjamini-Hochberg (FDR)","none")

      self$input_config$adjustmentMethodsNames <- purrr::pmap(self$input_config$adjustmentMethodsTibble,CUSOMShinyHelpers::createTooltip)

      self$input_config$platforms <- remoteDB$getQuery(
        "[shiny].[GetApplicationPlatforms] ?",
        tibble::tibble("ApplicationID" = ApplicationId)
      ) |>
        dplyr::pull()

      self$input_config$PlatformExperiments <- remoteDB$getQuery(
        " SELECT * FROM [shiny].[vw_ApplicationExperiments]"
        , NULL
      )

      self$input_config$Enrollments <- localDB$getQuery(
        "SELECT Karyotype, count(1) as [ParticipantCount]
        FROM AllParticipants
        GROUP BY Karyotype"
        )

      self$input_config$SampleDetail <- remoteDB$getQuery(
        "SELECT *
         FROM [shiny].[vw_FreezerProSampleDetail]",
        NULL
        ) |>
       dplyr::rename("record_id" = GUI, "LabID" = `Lab ID`)

      choices <- self$input_config$PlatformExperiments |>
        dplyr::filter(!is.na(TotalSamples), !is.na(ExperimentStudyName)) |>
        dplyr::select(PlatformGroup, PlatformDisplayName, ExperimentID, ExperimentStudyName) |>
        dplyr::arrange(PlatformGroup)

      self$input_config$AnalysisAvailable <- split(
        setNames(
          choices$ExperimentID, glue::glue("{choices$ExperimentStudyName} - {choices$PlatformDisplayName}")
        ),
        choices$PlatformGroup
      )

      rm(choices)
      
      self$input_config$probandRelationships <- localDB$getQuery(
        "SELECT distinct ProbandRelationship
          FROM AllParticipants"
      )

      self$input_config$karyotypes <- localDB$getQuery(
        "SELECT distinct Karyotype
          FROM AllParticipants"
      ) |>
        dplyr::pull()

      self$input_config$sexes <- localDB$getQuery(
        "SELECT distinct Sex
          FROM AllParticipants"
        ) |>
        dplyr::pull()

      self$input_config$ages <- localDB$getQuery(
        "SELECT distinct AgeAtTimeOfVisit
          FROM ParticipantEncounter"
        ) |>
        tidyr::drop_na() |>
        dplyr::summarise(
          min = round(min(AgeAtTimeOfVisit)),
          max = round(max(AgeAtTimeOfVisit)) + 1
        ) |>
        dplyr::reframe(
          age = seq(min, max, 1)
        ) |>
        dplyr::pull()

      self$input_config$Conditions <- localDB$getQuery(
        "SELECT distinct Condition
          FROM ParticipantConditions"
        ) |>
        dplyr::pull()

      self$input_config$ConditionClasses <- localDB$getQuery(
        "SELECT distinct ConditionClass
          FROM ParticipantConditions"
        ) |>
        tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
        tidyr::drop_na() |>
        dplyr::select(ConditionClass) |>
        dplyr::distinct() |>
        dplyr::pull()

      self$input_config$ConditionChoices_df <- localDB$getQuery(
          "SELECT *
            FROM ParticipantConditions"
        ) |>
        dplyr::filter(HasCondition == "True") |>
        dplyr::select(record_id, ConditionClass, Condition) |>
        dplyr::group_by(ConditionClass, Condition) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")  |>
        dplyr::filter(n >= 5) |>      
        tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE)

      self$input_config$ConditionsChoices <- split(
        setNames(
          self$input_config$ConditionChoices_df$Condition,
          glue::glue("{self$input_config$ConditionChoices_df$Condition}")
        ),
        self$input_config$ConditionChoices_df$ConditionClass
      )

    }
  )
)
