#' R6 Class to manage core TrisomExploreR applications
#' @description
#'  R6 Class to manage core TrisomExploreR applications
#' @field app_config - list -
#' @field module_config - list -
#' @field analysis_config - list -
#' @field input_config - list -
#' @export
TrisomExplorerAppManager <- R6::R6Class(
  "TrisomExplorerAppManager",
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
      statTests = c("Linear Model","Wilcoxon test"),
      statTestTibble = NULL,
      statTestschoiceNames = NULL,
      adjustmentMethods = c("Benjamini-Hochberg (FDR)","none"),
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
    initialize = function(ApplicationId, remoteDB, localDB){

      self$app_config$ApplicationId <- ApplicationId

      ApplicationNamespaceConfig <- remoteDB$getQuery(
        "SELECT * FROM [te].[vw_ApplicationNamespaceConfig]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))"
        ,tibble::tibble('ApplicationId' = ApplicationId)
      )

      self$app_config$Namespaces <- ApplicationNamespaceConfig$Namespace
      self$app_config$applicationTitle <- ApplicationNamespaceConfig$applicationName[1]
      self$app_config$applicationLabel <-  ApplicationNamespaceConfig$applicationLabel[1]
      self$app_config$applicationURL <- ifelse(ApplicationNamespaceConfig$environment[1] == "Production",'https://www.trisome.org/explorer','https://www.trisome.org/explorer-internal')

      self$app_config$applicationLinks <- remoteDB$getQuery(
        "SELECT [LinkedApplicationLabel] [label], [LinkedApplicationImageURL][imageURL],[LinkedApplicationURL] [link], [IsCurrentApplication]
          FROM [app].[vw_ShinyApplicationApplicationLinks]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY LinkDisplayOrder"
        ,tibble::tibble('ApplicationId' = ApplicationId)
      )

      self$app_config$tutorials <- remoteDB$getQuery(
        "[shiny].[GetApplicationTutorials] ?"
        ,tibble::tibble('ApplicationId' = ApplicationId)
      )

      self$module_config <- ApplicationNamespaceConfig |>
        dplyr::select(Namespace, TabText, TabIcon, ModuleServerName, UseR6Class, R6ClassName )

      self$analysis_config <- ApplicationNamespaceConfig |>
        dplyr::select(Namespace, AnalysisVariableName, AnalysisVariableLabel, AnalysisType, AnalysisVariableBaselineLabel, AnalysisVolcanoPlotTopAnnotation)

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

      ## CROSS OMICS INPUTS
      ## SLOW - LOAD TO MODULE?
      self$input_config$PlatformExperiments <- remoteDB$getQuery(
        "[shiny].[GetAplicationPlatformExperiments] ?"
        ,tibble::tibble("ApplicationID" = ApplicationId)
      )

      self$input_config$Queryplatforms <- remoteDB$getQuery(
        "[shiny].[GetQueryPlatforms] ?",
        tibble::tibble("ApplicationID" = ApplicationId)
      ) |>
        dplyr::arrange(QueryPlatform) |>
        dplyr::pull()

      ## SLOW -- LOAD TO MODULE?
      self$input_config$Comparisonplatforms <- remoteDB$getQuery(
        "[shiny].[GetComparisonPlatforms] ?",
        tibble::tibble("ApplicationID" = ApplicationId)
      ) |>
        dplyr::arrange(ComparisonPlatform) |>
        dplyr::pull()

      self$input_config$experimentIDs <- remoteDB$getQuery(
        "[shiny].[GetApplicationStudies] ?",
        tibble::tibble("ApplicationID" = ApplicationId)
      ) |>
        dplyr::filter(!is.na(ExperimentStudyName))

      self$input_config$studies <- remoteDB$getQuery(
        "[shiny].[GetStudyDetailsByExperimentID] ?",
        tibble::tibble("ExperimentIDList" = glue::glue_collapse(self$input_config$experimentIDs$ExperimentID,sep=';'))
      )

      self$input_config$studiesTibble <- self$input_config$studies |>
        dplyr::mutate(
          TooltipText = dplyr::case_when(
            !is.na(ExperimentStudyTitle)  ~ glue::glue("{ExperimentStudyTitle} <br /><br /> {ExperimentStudyAdditionalText} <br /><br />Total Samples: {TotalSamples}"),
            TRUE ~ glue::glue("{ExperimentStudyName} <br /><br />Total Samples: {TotalSamples}")
          ),
          ShowTooltip = TRUE,
          PlatformGroupDisplayName = ifelse(is.na(PlatformGroupDisplayName),"Study",PlatformGroupDisplayName)
        ) |>
        dplyr::select("Text" = "ExperimentStudyName", "URL" = "ExperimentStudyURL", "TooltipText" = "TooltipText", "ShowTooltip", "FieldSet" = "PlatformGroupDisplayName")

      #self$studyChoiceNames <- purrr::pmap(self$studiesTibble,CUSOMShinyHelpers::createTooltip)
      #self$studyNames <- self$studies$ExperimentStudyName


      self$input_config$LabIDs <- localDB$getQuery(
        "SELECT distinct LabID FROM ParticipantEncounter"
        ) |>
        dplyr::pull()

      self$input_config$karyotypes <- localDB$getQuery(
        "SELECT distinct Karyotype FROM allParticipants"
        ) |>
        dplyr::pull()

      self$input_config$sexes <- localDB$getQuery(
       "SELECT distinct Sex FROM allParticipants WHERE Sex IS NOT NULL"
        ) |>
        dplyr::distinct() |>
        dplyr::pull()

      self$input_config$ages <- localDB$getQuery(
        "SELECT AgeAtTimeOfVisit FROM ParticipantEncounter"
      ) |>
        tidyr::drop_na() |>
        dplyr::summarise(
          min = round(min(AgeAtTimeOfVisit)),
          max = round(max(AgeAtTimeOfVisit))+1
        ) |>
        dplyr::reframe(
          age = seq(min,max,1)
        ) |>
        dplyr::pull()

      self$input_config$Conditions <- localDB$getQuery(
        "SELECT distinct Condition FROM ParticipantConditions"
        ) |>
        dplyr::pull()

      self$input_config$ConditionClasses <- localDB$getQuery(
        "SELECT distinct ConditionClass FROM ParticipantConditions"
        ) |>
        tidyr::separate_rows(sep = ';', 'ConditionClass', convert = TRUE) |>
        tidyr::drop_na() |>
        dplyr::select(ConditionClass) |>
        dplyr::distinct() |>
        dplyr::pull()

      self$input_config$ConditionChoices <- localDB$getQuery(
        "SELECT LabID,ConditionClass,Condition
         FROM ParticipantConditions
         WHERE HasCondition = 'True'"
        ) |>
        dplyr::select(LabID, ConditionClass, Condition) |>
        dplyr::group_by(ConditionClass, Condition) |>
        dplyr::summarize(n = dplyr::n_distinct(LabID), .groups = 'drop')  |>
        dplyr::filter(n >= 5) |>
        dplyr::left_join(
          localDB$getQuery(
            "SELECT distinct Condition,ConditionCensorshipAgeGroup
             FROM ParticipantConditions
             WHERE ConditionCensorshipAgeGroup IS NOT NULL "
          )
          , by = "Condition"
        ) |>
        dplyr::mutate(
          AgeCensor = dplyr::case_when(
            !is.na(ConditionCensorshipAgeGroup) ~ ConditionCensorshipAgeGroup,
            TRUE ~ ""
          )
        ) |>
        dplyr::select(-ConditionCensorshipAgeGroup,n) |>
        tidyr::separate_rows(sep = ';', 'ConditionClass',convert=TRUE)

    }
  )
)
