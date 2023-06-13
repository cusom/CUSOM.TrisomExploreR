#' R6 Class to manage Cell Type Analysis Modules
#' @description
#' R6 Class to manage analysis within the Cell Type module.
#' Retains chosen values from inputs, performs data retrieval, analysis, and plotting.
#'
#' @field applicationName - string - name of application
#' @field namespace - string - namespace for this instance
#' @field remoteDB - R6 class to manage remote database queries
#' @field localDB - R6 class to manage local database queries
#' @field analysisVariable - string - target feature for analysis in this instance-namespace
#' @field analysisVariableLabel - string - label for analysis variable
#' @field analysisType - string - type of analysis - should be one of Categorical or Continuous
#' @field analytesLabel - string - label to be used to refer to set of anlaysis variables
#' @field groupBaselineLabel - string - if analysis type if continous, label for baseline level (Control, Female, Without Condition, etc.)
#' @field FoldChangeVar - string - name of variable indicating fold change or difference (log2FoldChange)
#' @field SignificanceVariable - string - name of variable indiciating significance value (p-value)
#' @field Platform - string vector - Platform values chosen for analysis
#' @field CellType - string vector - Cell Type values chosen for analysis
#' @field Sex - string vector - Sex values chosen for analysis
#' @field Age - numeric vector - Age values chosen fo analysis
#' @field StatTest - string - name of statistical test to apply for analysis (Linear Model, etc.)
#' @field Covariates - string vector - names of features to include as covariates in Linear Model analysis
#' @field AdjustmentMethod - string - name of multiple hypothesis correction method to apply to statistical output
#' @field Analyte - character vector - name(s) of Analytes chosen for analysis
#' @field AnalyteData - tibble - sample level data for chosen analyte(s)
#' @field cids - character vector - control id values chosen to highlight on plot
#' @field tids - character vector - trisomy 21 id values chosen to highlight on plot
#'
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import plotly
#' @import glue
#' @importFrom CUSOMShinyHelpers getGroupedStatTestByKeyGroup
#' @importFrom CUSOMShinyHelpers getGroupedBoxplot
#' @importFrom CUSOMShinyHelpers getStatAnnotationAnchorLines
#' @importFrom CUSOMShinyHelpers getGroupedStatAnnotations
#' @export
CellTypeAnalysisManager <- R6::R6Class(
  "CellTypeAnalysisManager",
  private = list(),
  public = list(
    applicationName = NULL,
    namespace = NULL,
    remoteDB = NULL,
    localDB = NULL,

    analysisVariable = "",
    analysisVariableLabel = "",
    analysisType = "",
    analytesLabel = "s",
    groupBaselineLabel = "",
    FoldChangeVar = "log2FoldChange",
    SignificanceVariable = "-log10pvalue",

    Platform = NULL,
    CellType = NULL,
    Sex = NULL,
    Age = NULL,
    StatTest = NULL,
    Covariates = NULL,
    AdjustmentMethod = NULL,
    Analyte = NULL,
    AnalyteData = NULL,
    cids = NULL,
    tids = NULL,

    #' @description
    #' Create a new CellTypeAnalysisManager object
    #' @param applicationName string - applicationName
    #' @param id string - namespace for this class
    #' @param namespace_config tibble - configuration values for this namespace instance of the object
    #' @param remoteDB R6 class to manage remote database queries
    #' @param localDB R6 class to manange local database queries
    #' @return A new `CellTypeAnalysisManager` object.
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB) {

      self$applicationName <- applicationName
      self$remoteDB <- remoteDB
      self$localDB <- localDB

    },

    #' @description
    #' Get data for chosen analyte and platform
    #' @return tibble
    getAnalyteData = function() {

      dataframe <- self$remoteDB$getQuery(
        "[shiny].[GetAnalyteDataByPlatform] ?,?",
          tibble::tibble(
          'Platform' = self$Platform,
          'Analyte' = self$Analyte,
          )
        ) |>
        dplyr::left_join(
          self$localDB$getQuery(
            "SELECT pe.LabID, pe.record_id, pe.AgeAtTimeOfVisit as [Age], p.Karyotype, p.Sex
              FROM ParticipantEncounter pe
              INNER JOIN allPArticipants p ON p.record_id = pe.record_id"
          ),
          by = c("LabID", "record_id")
        ) |>
        dplyr::rename( "CellType" = Specimen ) |>
        dplyr::filter(
          CellType %in% self$CellType,
          (Sex %in% self$Sex | is.na(Sex)),
          (Age >= min(self$Age) | is.na(Age)),
          (Age <= max(self$Age) | is.na(Age)),
          outlier == FALSE
        ) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub> ({Measurement})"),
          Karyotype = forcats::fct_relevel(Karyotype, "Control")
        )

      self$cids <- dataframe |>
        dplyr::filter(Karyotype == "Control") |>
        dplyr::select(LabID) |>
        dplyr::distinct() |>
        dplyr::pull()

      self$tids <- dataframe |>
        dplyr::filter(Karyotype != "Control") |>
        dplyr::select(LabID) |>
        dplyr::distinct() |>
        dplyr::pull()

      if(nrow(dataframe)>0) {

        countData <- dataframe |>
          dplyr::add_count(Karyotype, CellType)

        statsData <- dataframe |>
          dplyr::select(CellType, LabID,  Analyte, log2MeasuredValue, Karyotype, Sex, Age) |>
          CUSOMShinyHelpers::getGroupedStatTestByKeyGroup(
            groupVar = CellType,
            id = LabID,
            key = Analyte,
            group = Karyotype,
            baselineLabel = "Control",
            response = log2MeasuredValue,
            testMethod = self$StatTest,
            adjustmentMethod = self$AdjustmentMethod,
            independentVariable = Karyotype,
            covariates = self$Covariates
          ) |>
          dplyr::mutate(
            p.value.text = unlist(purrr::pmap(.l = list(p.value,self$AdjustmentMethod), CUSOMShinyHelpers::formatPValue)),
            p.value.is.significant = ifelse(p.value.text %in% c('No significant difference','Unable to compute p-value using chosen methods'),FALSE,TRUE),
            p.value.adjustment.method = self$AdjustmentMethod
          )

        self$AnalyteData <- dplyr::inner_join(countData, statsData, by = "CellType") |>
          dplyr::mutate(
            text = glue::glue('{CellType}\n{MeasuredValue}\n{Karyotype} (n={n}) \n{p.value.text}')
          )

      }

      else {

        NULL
      }


    },

    #' @description
    #' get Cell Type plot
    #' @param .data - tibble of analyte data to use for plot
    #' @param ns - namespace to apply to plot
    #' @return plotly object
    getPlot = function(.data, ns) {

      CellTypes <- self$localDB$getQuery(
        "SELECT distinct [CellType] FROM CellTypes"
        ) |>
        dplyr::pull()

      AllCellTypes <- intersect( CellTypes, unique(self$CellType))

      p <- .data |>
        CUSOMShinyHelpers::getGroupedBoxplot(
          LabID,
          CellType,
          Karyotype,
          log2MeasuredValue,
          log2Measurement,
          text,
          LabID,
          TRUE,
          "CellTypeBoxplot"
        ) |>
        plotly::layout(
          showlegend = TRUE,
          legend = list(
            itemclick = "toggleothers",
            title = list(
              text = "<b>Karyotype</b>",
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 16
              )
            ),
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 16
            )
          ),
          title = list(
            text = HTML(glue::glue('Effect of trisomy 21 on {self$Analyte} mRNA expression')),
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 24
            ),
            pad = list(
              t = 10,
              b = 50
            ),
            y = 1
          ),
          xaxis = list(
            categoryorder = "array",
            categoryarray = AllCellTypes,
            fixedrange= TRUE
          ),
          yaxis = list(
            title = list(
              text = glue::glue("{.data$log2Measurement[1]}"),
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 18
              )
            ),
            tickfont = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            ),
            fixedrange = TRUE
          )
        )

      lines <- .data |>
        CUSOMShinyHelpers::getStatAnnotationAnchorLines(
          group = CellType,
          groupMembers = CellTypes,
          significanceVariable = p.value,
          groupIsSignificant = p.value.is.significant,
          includeInsignificantValues = TRUE
        )

      if(length(lines)>0) {

        significanceAnnotations <- CUSOMShinyHelpers::getGroupedStatAnnotations(
          AnnotationAnchorLines = lines,
          statTest = self$StatTest,
          covariates = self$Covariates,
          adjustmentMethod = self$AdjustmentMethod,
          formatInsignificantValues = TRUE
        )

        p <- p |>
          plotly::layout(
            shapes = lines,
            annotations = significanceAnnotations
          )
      }

      p <- p |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - {self$Analyte} Cell Type Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      p$x$source <- ns("AnalyteBoxPlot")

      return(p)

    }
  )
)
