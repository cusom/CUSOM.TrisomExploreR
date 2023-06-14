#' R6 Class to manage Correlates Analysis
#' @description
#' Enables analysis of cross-omics correlations
#'
#' @field applicationName - string - name of application
#' @field ApplicationId - string - application ID
#' @field namespace - string - namespace for this instance
#' @field remoteDB - R6 class to manage remote database queries
#' @field localDB - R6 class to manage local database queries
#' @field analysisType - string - type of analysis for this instance/namespace
#' @field FoldChangeVar - string - name of variable indicating fold change or difference (log2FoldChange)
#' @field SignificanceVariable - string - name of variable indiciating significance value (p-value)
#' @field Study - string - selected study
#' @field QueryPlatform - string - selected query platform
#' @field ComparisonPlatform - string - selected comparison platform
#' @field Experiment - string - chosen experimentID
#' @field QueryAnalyte - string - chosen Query Analyte
#' @field Analyte - string - chosen Comparison Analyte
#' @field AnalyteSearchName - string - name of analyte used for external links
#' @field CorrelationMeasureName - string - name of correlation measure used in analysis - `rho` or `Fold Change`
#' @field Sex - string vector - Sex values chosen for analysis
#' @field Age - numeric vector - Age values chosen fo analysis
#' @field StatTest - string - name of statistical test to apply for analysis (Linear Model, etc.)
#' @field Covariates - string vector - names of features to include as covariates in Linear Model analysis
#' @field AdjustmentMethod - string - name of multiple hypothesis correction method to apply to statistical output
#' @field Adjusted - logical - whether the statistical test includes multiple hypothesis correction or not
#' @field SignificanceLabel - string - if adjusted, `q-value`, otherwise `p-value`
#' @field ComparisonAnalytePlotStatAnnotation - string - annotation label to show at top of volcano plot
#' @field AnalyteData - tibble - sample level data for chosen analyte(s)
#' @field AnalyteDataDownload - deprecated?
#' @field AnalytePlotMethod - string - type of plot to show for chosen analyte
#' @field CorrelationAnalytePlotTitle - string - title to show for analyte plot
#' @field QueryMeasurement - string - measurement used for query analyte sample level data
#' @field ComparisonMeasurement - string - measurement used for comparison analyte sample level data
#' @field QueryAnalyteLabel - string - label used for query analyte
#' @field ComparisonAnalyteLabel - string - labe used for comparison analyte
#' @field VolcanoSummaryData - tibble - Fold Change summary data used for volcano plot
#' @field VolcanoSummaryDataXAxisLabel - string - volcano plot x-axis
#' @field VolcanoSummaryDataYAxisLabel - string - volcano plot y-axis
#' @field VolcanoSummaryMaxFoldChange - numeric - maxiumum abs. value of fold change
#' @field VolcanoPlotTitle - string - title to show above volcano plot
#' @field volcanoPlotExpectedTraceCount - numeric - number of base traces present in the active volcano plot (usually between 1 - 3)
#' @field HeatmapData - tibble - data to use for heatmap plot when multiple analytes are chosen
#' @field HeatmapText - string vector - text used for hover on heatmap plot
#' @field HeatmapLimit - numeric - maxiumum abs correlation value used to determine upper/lower bounds of heatmap
#' @field GSEAData - list of ranks, hallmarks, and gsea results
#' @field GSEAAnalytes - character vector - matching analytes for for chosen GSEA pathway
#' @field GSEATraceName - string - name of chosen GSEA pathway
#' @field GSEAGenesetName - string - formatted version of GSEA Trace Name
#' @field GSEAPathwayData - pathway specific data for chosen GSEA pathway
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import purrr
#' @import glue
#' @import plotly
#' @import htmlwidgets
#' @importFrom plyr round_any
#' @importFrom forcats fct_inorder
#' @importFrom heatmaply heatmaply
#' @export
CorrelatesManager <- R6::R6Class(
  "CorrelatesManager",
  private = list(),
  public = list(
    applicationName = NULL,
    ApplicationId = NULL,
    namespace = NULL,
    remoteDB = NULL,
    localDB = NULL,

    analysisType = "Continuous",
    FoldChangeVar = "CorrelationValue",
    SignificanceVariable = "-log10pvalue",

    Study = "",
    QueryPlatform = "",
    ComparisonPlatform = "",
    Experiment = "",
    QueryAnalyte = "",
    Analyte = "",
    AnalyteSearchName = "",
    CorrelationMeasureName = "",
    Sex = NULL,
    Age = NULL,
    StatTest = NULL,
    Covariates = NULL,
    AdjustmentMethod = NULL,
    Adjusted = FALSE,
    SignificanceLabel = "p-value",

    ComparisonAnalytePlotStatAnnotation = NULL,

    VolcanoSummaryData = NULL,
    VolcanoSummaryDataXAxisLabel = "",
    VolcanoSummaryDataYAxisLabel = "",
    VolcanoSummaryMaxFoldChange = 0,
    VolcanoPlotTitle = "",
    volcanoPlotExpectedTraceCount = 3,

    AnalyteData = NULL,
    AnalyteDataDownload = NULL,
    AnalytePlotMethod = "boxplot",
    CorrelationAnalytePlotTitle = "",
    QueryMeasurement = "",
    ComparisonMeasurement ="",
    QueryAnalyteLabel = "",
    ComparisonAnalyteLabel = "",

    HeatmapData = NULL,
    HeatmapText = NULL,
    HeatmapLimit = 0,

    GSEAData = NULL,
    GSEAAnalytes = "",
    GSEATraceName = "",
    GSEAGenesetName = "",
    GSEAPathwayData = NULL,

    #' @description
    #' Create a new instance of CorrelatesManager object
    #' @param applicationName string - applicationName
    #' @param id string - namespace for this class
    #' @param namespace_config tibble - configuration values for this namespace instance of the object
    #' @param remoteDB R6 class to manage remote database queries
    #' @param localDB R6 class to manange local database queries
    #' @return A new `CorrelatesManager` object.
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){

      self$applicationName <- applicationName
      self$remoteDB <- remoteDB
      self$localDB <- localDB

      namespace_config <- namespace_config |>
        dplyr::filter(Namespace == id)
      self$namespace <- namespace_config$Namespace
      self$ApplicationId <- namespace_config$ApplicationId
    },

    #' @description
    #' gets query platforms for cross-omics analysis
    getQueryPlatforms = function() {

      self$remoteDB$getQuery(
        "[shiny].[GetQueryPlatforms] ?",
        tibble::tibble("ApplicationID" = self$ApplicationId)
      ) |>
        dplyr::arrange(QueryPlatform) |>
        dplyr::pull()
    },

    #' @description
    #' return hidden /shown / disabled / enabled class for GSEA button based on
    #'  chosen comparison platform and whether or not Volcano Plot is rendered
    #'
    #' @return string
    addGSEAInputClass = function() {
      hide <- "hide"
      disabled <- "disabled"
      if (!is.null(self$ComparisonPlatform)) {
        if (grepl('SOMA',self$ComparisonPlatform) | grepl('RNA', self$ComparisonPlatform)) {
          hide <- "show"
        }
        if (!is.null(self$VolcanoSummaryData)) {
          disabled <- "enabled"
        }
      }
      return(
        glue::glue("shinyjs-{hide} shinyjs-{disabled}")
      )
    },

    #' @description
    #' Get query analyte choices for the chosen QueryPlatform
    #'
    #' @return string vector
    getQueryAnalytes = function() {
      return(
        self$remoteDB$getQuery(
          "[shiny].[GetQueryAnalytes] ?",
          tibble::tibble("QueryPlatform" = self$QueryPlatform)
          ) |>
          dplyr::arrange(QueryAnalyte) |>
          dplyr::pull()
      )
    },

    #' @description
    #' gets comparison platforms for cross-omics analysis
    getComparisonPlatforms = function() {
      self$remoteDB$getQuery(
        "[shiny].[GetComparisonPlatforms] ?",
        tibble::tibble("ApplicationID" = self$ApplicationId)
      ) |>
        dplyr::arrange(ComparisonPlatform) |>
        dplyr::pull()
    },

    #' @description
    #' Set Volcano Summary Data along with other volcano plot properties
    #'
    #' @return none
    getVolcanoSummaryData = function() {

      self$Adjusted <- self$AdjustmentMethod != "none"

      correlationData <- self$remoteDB$getQuery(
        "[shiny].[GetCorrelationDataset] ?, ?, ?",
        tibble::tibble(
          "QueryPlatform" = self$QueryPlatform,
          "QueryAnalyte" = self$QueryAnalyte,
          "ComparisonPlatform" = self$ComparisonPlatform
        )
      ) |>
      dplyr::rename(
        "Analyte" = ComparisonAnalyte,
        "AnalyteID" = ComparisonAnalyteID
      )

      self$CorrelationMeasureName <- unique(correlationData$CorrelationMeasureName)

      max_finite <- correlationData |>
        dplyr::filter(p.value > 0) |>
        dplyr::pull(p.value) |>
        min() |>
        (\(x) {-log10(x)})()

      self$VolcanoSummaryData <- correlationData |>
        dplyr::mutate(
          shape = ifelse(p.value == 0, "triangle-up", "circle"),
          p.value = ifelse(p.value == 0, 10^-(max_finite * 1.05), p.value),
          `-log10pvalue` = -log10(p.value)
        ) |>
        dplyr::group_by(Analyte) |>
        dplyr::mutate(rank = dplyr::row_number(-abs(CorrelationValue))) |>
        dplyr:: filter(rank == 1) |>
        dplyr::select(-rank) |>
        dplyr::mutate(
          "p.value.adjustment.method" = "BH",
          formattedPValue = unlist(purrr::pmap(.l = list(p.value,p.value.adjustment.method), CUSOMShinyHelpers::formatPValue)),
          text = glue::glue('Analyte: {Analyte} <br />{self$CorrelationMeasureName}:{round(CorrelationValue,2)} <br />{formattedPValue}')
        ) |>
        dplyr::ungroup()

      self$VolcanoPlotTitle <- glue::glue('Correlation between {CUSOMShinyHelpers::parseDelimitedString(self$QueryAnalyte,1)} and {self$ComparisonPlatform}')
      self$VolcanoSummaryMaxFoldChange <- max(abs(self$VolcanoSummaryData$CorrelationValue))
      self$VolcanoSummaryDataXAxisLabel <- self$CorrelationMeasureName
      self$VolcanoSummaryDataYAxisLabel <- glue::glue("-log<sub>10</sub>({ifelse(self$Adjusted,\"q-value \",\"p-value \")})")

    },

    #' @description
    #' Get volcano plot
    #' @param .data tibble - data for volcano plot
    #' @param ns - namespace to apply to plot object
    #'
    #' @return plotly object
    getVolcanoPlot = function(.data, ns) {

      .data <- .data |>
        dplyr::mutate(
          selectedPoint = 0
        )

      a <- .data |>
        CUSOMShinyHelpers::getCorrelationVolcanoAnnotations(
          foldChangeVar = CorrelationValue,
          significanceVariable = `-log10pvalue`,
          selected = selectedPoint,
          arrowLabelTextVar = Analyte,
          titleText = glue::glue("Correlation with {self$QueryAnalyte}:") ,
          includeThresholdLabel = FALSE
        )

      .data <- .data |>
        CUSOMShinyHelpers::addSignificanceGroup(
          foldChangeVar = CorrelationValue,
          significanceVariable = `-log10pvalue`,
          adjustedInd = a$parameters$adjustedInd,
          significanceThreshold = a$parameters$significanceThresholdTransformed,
          originalSignificanceThreshold = a$parameters$significanceThreshold
        )

      self$volcanoPlotExpectedTraceCount <- .data |>
        dplyr::distinct(significanceGroup, shape) |>
        nrow()

      p <- .data |>
        CUSOMShinyHelpers::getVolcanoPlot(
          foldChangeVariable = CorrelationValue,
          significanceVariable = `-log10pvalue`,
          significanceGroup =  significanceGroup,
          text = text,
          key = Analyte,
          color = color,
          shape = shape,
          plotName = ""
        )  |>
        plotly::layout(
          showlegend = TRUE,
          legend = list(
            orientation = 'h',
            itemclick = "toggleothers",
            itemsizing = "constant",
            valign = "middle",
            xanchor = "center",
            x = 0.5,
            y = -0.12,
            title = list(
              text = "",
              side = "left",
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 14
              )
            ),
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          title = list(
            text = self$VolcanoPlotTitle,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 18
            ),
            pad = list(
              t = 10,
              l = 5
            ),
            x = 0,
            xanchor = "left",
            xref = "container",
            y = 1
          ),
          xaxis = list(
            title = list(
              text = self$VolcanoSummaryDataXAxisLabel,
              standoff = 0,
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 14
              )
            ),
            tickfont = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 10
            ),
            fixedrange = FALSE
          ),
          yaxis = list(
            title = list(
              text = self$VolcanoSummaryDataYAxisLabel,
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 14
              )
            ),
            tickfont = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 10
            ),
            fixedrange = FALSE
          ),
          annotations=c(a$annotations, a$arrow),
          margin = list( t = 75)
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - Volcano Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}'),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            #list(plotlyCustomIcons$VolcanoPlotTutorial),
            list("select2d"),
            list("lasso2d"),
            list("zoom2d"),
            list("zoomIn2d"),
            list("zoomOut2d"),
            list("resetScale2d"),
            list("toImage")
          )
        ) |>
        htmlwidgets::onRender('
          function(el) {
            el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
          }'
        )

      p$x$source <- ns("VolcanoPlot")

      p

    },

    #' @description
    #' Get formatted volcano plot data for data table
    #' @param .data tibble - data for volcano plot
    #'
    #' @return tibble
    getFormattedVolcanoSummaryData =  function(.data) {

      oldNames = c("QueryPlatform","QueryAnalyte","ComparisonPlatform","Analyte","p.value","-log10pvalue")
      newNames = c("Query Platform","Query Analyte","Comparison Platform","Comparison Analyte","q-value (BH Adjusted)","-log10(q-value)")

      .data |>
        dplyr::rename_with(~ newNames, all_of(oldNames)) |>
        dplyr::rename(`:=`(!!rlang::quo_name(self$CorrelationMeasureName), CorrelationValue))

    },

    #' @description
    #' update analyte fields
    #'
    #' @return none
    updateAnalyteAttributes = function() {

      self$AnalyteSearchName <- CUSOMShinyHelpers::parseDelimitedString(self$Analyte,1)

      if(length(self$Analyte) == 1 ) {

        self$ComparisonAnalytePlotStatAnnotation <- self$VolcanoSummaryData |>
          dplyr::filter(Analyte == self$Analyte) |>
          dplyr::ungroup() |>
          dplyr::select(p.value,p.value.adjustment.method) |>
          dplyr::mutate(formatted.p.value = CUSOMShinyHelpers::formatPValue(p.value,p.value.adjustment.method)) |>
          dplyr::select(formatted.p.value)
      }
    },

    #' @description
    #' set analyte sample level data
    #'
    #' @return none
    getAnalyteData = function() {

      self$AnalytePlotMethod <- getAnalytePlotMethod(self$analysisType, length(self$Analyte))
      self$CorrelationAnalytePlotTitle <- glue::glue('{CUSOMShinyHelpers::parseDelimitedString(self$Analyte,1)} vs {CUSOMShinyHelpers::parseDelimitedString(self$QueryAnalyte,1)}')

      if(length(self$Analyte) == 1) {
        # Query on X-axis, Comparison on y-axis
        self$AnalyteData <- self$remoteDB$getQuery(
            "[shiny].[GetAnalyteDataByPlatform] ?, ?",
            tibble::tibble(
              "Platform" =  self$ComparisonPlatform,
              "Analyte" = self$Analyte
            )
          )   |>
          dplyr::filter(outlier == FALSE) |>
          dplyr::select(LabID, "ComparisonAnalyte" = Analyte,  MeasuredValue, Measurement) |>
          dplyr::rename(y = MeasuredValue) |>
          dplyr::inner_join(
            self$remoteDB$getQuery(
              "[shiny].[GetAnalyteDataByPlatform] ?, ?",
              tibble::tibble(
                "Platform" =  self$QueryPlatform,
                "Analyte" = self$QueryAnalyte
              )
            ) |>
              dplyr::filter(outlier==FALSE) |>
              dplyr::select(LabID,  "QueryAnalyte" = Analyte, MeasuredValue, Measurement) |>
              dplyr::rename(x = MeasuredValue)
            , by="LabID"
          ) |>
          dplyr::mutate(
            log2x = log2(x),
            log2y = log2(y),
            "ComparisonPlatform" = self$ComparisonPlatform,
            "QueryPlatform" = self$QueryPlatform,
            xLabel = CUSOMShinyHelpers::parseDelimitedString(QueryAnalyte,1),
            yLabel = CUSOMShinyHelpers::parseDelimitedString(ComparisonAnalyte,1)
          )

        self$ComparisonMeasurement <- self$AnalyteData[1,'Measurement.y']
        self$ComparisonAnalyteLabel <- self$AnalyteData[1,'yLabel']

        self$QueryMeasurement <- self$AnalyteData[1,'Measurement.x']
        self$QueryAnalyteLabel <- self$AnalyteData[1,'xLabel']

      }

      else {

        dataset <- self$VolcanoSummaryData |>
          dplyr::filter(AnalyteID %in% self$Analyte) |>
          dplyr::select(Analyte,AnalyteID, CorrelationValue, text) |>
          dplyr::arrange(-CorrelationValue) |>
          dplyr::mutate(Analyte = forcats::fct_inorder(Analyte), "Analysis" = glue::glue("{self$CorrelationMeasureName}"))

        self$HeatmapLimit <- dataset |>
          dplyr::pull(CorrelationValue) |>
          abs() |>
          max() |>
          plyr::round_any(0.01, f = ceiling)

        self$HeatmapText <- self$VolcanoSummaryData |>  dplyr::select(text)

        self$AnalyteData <- dataset |>
          dplyr::select(name = Analyte, variable = Analysis, value = CorrelationValue)

        self$HeatmapData <- dataset |>
          dplyr::select(Analyte, AnalyteID, "z" = CorrelationValue) |>
          dplyr::arrange(z) |>
          dplyr::mutate(r = dplyr::row_number())

      }

    },

    #' @description
    #' Get analyte plot
    #' @param .data tibble - data for analyte plot
    #' @param ns - namespace to apply to plot object
    #' @return plotly object
    getAnalytePlot = function(.data, ns) {

      if(self$AnalytePlotMethod == "scatterplot") {

        p <- .data |>
          dplyr::mutate(Density = CUSOMShinyHelpers::getDensityColors(x, y, transform = TRUE)) |>
          dplyr::arrange(Density) |>
          dplyr::ungroup() |>
          dplyr::mutate(text = glue::glue("{xLabel} log<sub>2</sub>({self$ComparisonMeasurement}): {round(log2x,2)}<br>{yLabel} log<sub>2</sub>({self$QueryMeasurement}): {round(log2y,2)}<br>Density: {Density}")) |>
          CUSOMShinyHelpers::getScatterPlotWithSmoothing(
            xVar = log2x,
            yVar = log2y,
            colorVar = Density,
            textVar =  text,
            smoothingMethod = "lm"
          )

        p <- plotly::ggplotly(p, tooltip="text") |>
          plotly::layout(
            showlegend = TRUE,
            legend = list(
              orientation = 'h',
              itemclick = "toggleothers",
              itemsizing = "constant",
              itemwidth = 30,
              valign = "middle",
              xanchor = "center",
              x = 100,
              y = 0.1,
              title = list(
                text = "",
                font = list(
                  family = "Arial",
                  color = "rgb(58, 62, 65)",
                  size = 14
                )
              ),
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 14
              )
            ),
            title = list(
              text = HTML(self$CorrelationAnalytePlotTitle),
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 18
              ),
              pad = list(
                t = 10,
                l = 5
              ),
              x = 0,
              xanchor = "left",
              xref = "container",
              y = 1
            ),
            xaxis = list(
              title = list(
                text = glue::glue("{self$QueryAnalyteLabel} log<sub>2</sub>({self$QueryMeasurement})") ,
                standoff = 0,
                font = list(
                  family = "Arial",
                  color = "rgb(58, 62, 65)",
                  size = 14
                )
              ),
              tickfont = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 10
              ),
              fixedrange = TRUE
            ),
            yaxis = list(
              title = list(
                text = glue::glue("{self$ComparisonAnalyteLabel} log<sub>2</sub>({self$ComparisonMeasurement})"),
                font = list(
                  family = "Arial",
                  color = "rgb(58, 62, 65)",
                  size = 14
                )
              ),
              tickfont = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 10
              ),
              fixedrange = TRUE
            ),
            annotations = list(
              list(
                x = 0.5,
                y = 1.025,
                text = glue::glue("{self$ComparisonAnalytePlotStatAnnotation}"),
                xref = "paper",
                yref = "paper",
                axref = "x",
                ayref = "y",
                ax = 0,
                ay = 0,
                font = list(
                  family = "Arial",
                  color = "rgb(58, 62, 65)",
                  size = 12
                )
              )
            ),
            margin = list( t = 75)
          ) |>
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue::glue('{self$applicationName} - Analyte Plot {format(Sys.time(),"%Y%m%d_%H%M%S") }'),
              width = NULL,
              height = NULL
            ),
            modeBarButtons = list(
              #list(plotlyCustomIcons$AnalytePlotTutorial),
              list("toImage")
            )
          ) |> htmlwidgets::onRender(
            'function(el) {
              el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
            }'
          )

        p$x$source <- ns("AnalytePlot")

        p

      }

      else {
        # heatmap

        p <- heatmaply::heatmaply(
          long_data = .data,
          dendrogram = "none",
          xlab = "",
          ylab = "",
          key = ~ name,
          showticklabels = c(FALSE, TRUE),
          main = glue::glue("{self$CorrelationMeasureName}"),
          margins = c(60,100,40,20),
          subplot_widths = 0.65,
          yaxis_width = 10,
          grid_color = "white",
          grid_width = 0.001,
          titleX = TRUE,
          limits = c(-self$HeatmapLimit, self$HeatmapLimit),
          col = RColorBrewer::brewer.pal(11, "RdBu") |> rev(),
          scale_fill_gradient_fun = circlize::colorRamp2(
            seq(-self$HeatmapLimit, self$HeatmapLimit, length.out = 11),
            RColorBrewer::brewer.pal(11, "RdBu") |> rev()
          ),
          key.title = glue::glue("{self$CorrelationMeasureName}"),
          branches_lwd = 0.1,
          fontsize_row = 10,
          fontsize_col = 1,
          heatmap_layers = theme(axis.line=element_blank()),
          plot_method = "plotly",
          colorbar_len = 0.5,
          colorbar_yanchor = "middle",
          colorbar_ypos = 0.5,
          custom_hovertext = as.matrix(
            self$HeatmapText
          )
        ) |>
        plotly::colorbar(
          tick0 = -self$HeatmapLimit,
          dtick = self$HeatmapLimit
        ) |>
        plotly::layout(
          title = list(
            text = glue::glue("{self$QueryAnalyte} vs. selected {self$ComparisonPlatform} Analytes"),
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 18
            ),
            pad = list(
              t = 10,
              l = 5
            ),
            x = 0,
            xanchor = "left",
            xref = "container",
            y = 1
          ),
          xaxis = list(
            list(fixedrange = TRUE)
          )
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - Heatmap {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        ) |>
        htmlwidgets::onRender('
          function(el) {
            el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
          }'
        )

        p$x$source <- ns("HeatmapPlot")

        p

      }

    },

    #' @description
    #' Get formatted analyte plot data for data table
    #'
    #' @return tibble
    getFormattedAnalyteSummaryData =  function() {

      xlabel <- glue::glue("{self$QueryAnalyteLabel} log<sub>2</sub>({self$QueryMeasurement})")
      ylabel <- glue::glue("{self$ComparisonAnalyteLabel} log<sub>2</sub>({self$ComparisonMeasurement})")

      if(length(self$Analyte) == 1) {

        return(
          self$AnalyteData |>
            dplyr::select(-c(Measurement.x, Measurement.y,xLabel,yLabel,x,y)) |>
            dplyr::rename_with(~gsub("(?<!^|\\s)([A-Z]+)", " \\1", ., perl = T), everything()) |>
            dplyr::rename(`:=`(!!xlabel,log2x), `:=`(!!ylabel, log2y))
        )

      } else {

        self$AnalyteData |>
          dplyr::inner_join(self$VolcanoSummaryData, by = c("name" = "AnalyteID")) |>
          dplyr::select(QueryPlatform, QueryAnalyte, ComparisonPlatform, Analyte, CorrelationValue) |>
          dplyr::rename(`:=`(!!self$CorrelationMeasureName,CorrelationValue)) |>
          dplyr::rename_with(~gsub("(?<!^|\\s)([A-Z]+)", " \\1", ., perl=T), everything())
      }

    },

    #' @description
    #' Get GSEA data
    #' ** INCLUDES penalized calculation for ranks = `-log10pvalue` * CorrelationValue
    #'
    #' @return list of 3 tibbles:
    #' * ranks - ranked analytes
    #' * GSEA_hallmarks - geneset used
    #' * gsea - gene set enrichment analysis
    getGSEAData = function() {

      ranks <- self$VolcanoSummaryData |>
        dplyr::rowwise() |>
        dplyr::mutate(ParsedComparisonAnalyte = CUSOMShinyHelpers::parseDelimitedString(Analyte,1)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          ID = ParsedComparisonAnalyte,
          t = (`-log10pvalue` * CorrelationValue)
        ) |>
        dplyr::select(ID,t) |>
        #dplyr::select(ID = ParsedComparisonAnalyte, t = CorrelationValue) |>
        dplyr::filter(!is.na(t)) |>
        dplyr::arrange(-abs(t)) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        tibble::deframe()

      gsea <- CUSOMShinyHelpers::runfGSEA(geneset = TrisomExploreR:::GSEA_hallmarks, ranks = ranks) |>
        dplyr::mutate(
          Leading.edge.genes = purrr::map_chr(leadingEdge, toString),
          Leading.edge.genes = gsub(' ','',Leading.edge.genes)
        ) |>
        dplyr::select("Gene.set" = pathway, "Size" = size, ES, NES, "p.value" = pval, "q.value" = padj, Leading.edge.genes) |>
        dplyr::mutate(Gene.set = stringr::str_to_title(trimws(gsub('_',' ',gsub('HALLMARK','',Gene.set)))))

      self$GSEAData <- list(
        "ranks" = ranks,
        "hallmarks" = TrisomExploreR:::GSEA_hallmarks,
        "gsea" = gsea
      )
    },

    #' @description
    #' Get GSEA plot - top 25
    #' @param .data - data for plot
    #' @param ns - namespace to apply to plot object
    #' @return plotly object
    getGSEAPlot = function(.data, ns) {

      data <- .data$gsea |>
        dplyr::mutate(
          `-log10qvalue` = -log(q.value),
          text = glue::glue("Gene Set: {Gene.set}<br />NES: {NES} <br />-log<sub>10</sub>(q-value): {`-log10qvalue`}")
        ) |>
        dplyr::top_n(25, wt = abs(NES))

      limit <- data |>
        dplyr::pull(NES) |>
        abs() |>
        max() |>
        plyr::round_any(1, f = ceiling) * 1.15

      p <- data |>
        dplyr::arrange(`-log10qvalue`) |>
        plotly::plot_ly(
          type = "bar",
          x = ~ `-log10qvalue`,
          y = ~ reorder(Gene.set,NES),
          hoverinfo = "text",
          hovertext = ~ text,
          customdata = ~ Leading.edge.genes,
          marker = list(
            color = ~ NES,
            autocolorscale = FALSE,
            colorscale = "RdBlu",
            cauto  = FALSE,
            cmax = limit,
            cmid = 0,
            cmin = -limit,
            colorbar = list(
              title = 'NES',
              tickmode = "auto",
              len = 0.5,
              yanchor = "middle",
              y = 0.5
            )
          )
        ) |>
        plotly::layout(
          title = list(
            text = "GSEA: Top 25 Hallmark gene sets <br />T21 vs. Control"
          ),
          showlegend = FALSE,
          xaxis = list(
            title = list(
              text = "-log<sub>10</sub>(q-value)"
            ),
            showlines = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            title = list(
              text = ""
            ),
            showlines = FALSE
          ),
          margin = list(
            t = 65
          ),
          shapes = list(
            list(
              type = "line",
              xref = "x",
              yref = "paper",
              axref = "paper",
              ayref = "y",
              y0 = 0,
              y1 = 1,
              x0 = -log(0.1),
              x1 = -log(0.1),
              line = list(
                color = "black",
                dash = "dot"
              )
            )
          )
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - GSEA Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      p$x$source <- ns("GSEAPlot")

      return(p)

    },

    #' @description
    #' Set selected GSEA pathway data
    #' @param pathName - string - selected pathway name
    #' @return none
    getGSEAPathwayData = function(pathName) {

      gseaParam <- 0

      stats <- self$GSEAData$ranks

      pathwayNammed <- self$GSEAData$gsea |>
        dplyr::filter(Gene.set == pathName) |>
        dplyr::select(Leading.edge.genes) |>
        dplyr::mutate(id = dplyr::row_number()) |>
        tidyr::separate_rows(Leading.edge.genes, sep = ",") |>
        dplyr::select(-id) |>
        purrr::simplify()

      rnk <- rank(-stats)
      ord <- order(rnk)
      statsAdj <- stats[ord]
      statsAdj <- sign(statsAdj) * (abs(statsAdj)^gseaParam)
      statsAdj <- statsAdj/max(abs(statsAdj))

      pathway <- unname(as.vector(na.omit(match(pathwayNammed, names(statsAdj)))))
      pathway <- sort(pathway)

      gseaRes <- fgsea::calcGseaStat(
        statsAdj,
        selectedStats = pathway,
        returnAllExtremes = TRUE
      )

      bottoms <- gseaRes$bottoms
      tops <- gseaRes$tops
      n <- length(statsAdj)
      xs <- as.vector(rbind(pathway - 1, pathway))
      ys <- as.vector(rbind(bottoms, tops))

      GSEAScores <- tibble::tibble(
        x = c(0, xs, n + 1),
        y = c(0, ys, 0)
      ) |>
      dplyr::inner_join(
        tibble::tibble(x = pathway, Gene = pathwayNammed),
        by = "x"
      ) |>
      dplyr::rename("Rank" = x, "ES" = y) |>
      dplyr::relocate("Gene")

      self$GSEAPathwayData <- self$VolcanoSummaryData |>
        dplyr::filter(Analyte %in% self$Analyte) |>
        dplyr::select(Analyte, CorrelationValue,"-log<sub>10</sub>(q-value)" = `-log10pvalue`) |>
        tidyr::separate(col = "Analyte", into = "Gene", remove = FALSE) |>
        dplyr::left_join(GSEAScores, by = "Gene" ) |>
        dplyr::group_by(Gene) |>
        dplyr::arrange(-CorrelationValue) |>
        dplyr::mutate(r = dplyr::row_number()) |>
        dplyr:: ungroup() |>
        dplyr::filter(r == 1) |>
        dplyr::select(-c(ES,r)) |>
        dplyr::mutate(
          CorrelationValue = format(CorrelationValue,scientific = TRUE),
          `-log<sub>10</sub>(q-value)` = format(`-log<sub>10</sub>(q-value)`,scientific = TRUE),
          Gene = glue::glue("<a href=\"https://www.genecards.org/Search/Keyword?queryString={Gene}\" target=\"_blank\">{Gene}</a>")
        ) |>
        dplyr::relocate(Gene) |>
        dplyr::arrange(Rank) |>
        dplyr::rename(
          `:=`(!!self$CorrelationMeasureName, CorrelationValue)
        )

      self$GSEAAnalytes <- self$GSEAPathwayData |>
        dplyr::select(Analyte) |>
        dplyr::summarise(text = toString(Analyte)) |>
        dplyr::mutate(text = gsub(', ','|',text)) |>
        dplyr::pull()

      self$GSEAGenesetName <- glue::glue("HALLMARK_{gsub(' ','_',stringr::str_to_upper(self$GSEATraceName))}")

    },

    #' @description
    #' Get selected GSEA pathway enrichment plot
    #' @param .data - data used for plot
    #' @param ns - namespace to apply to plot object
    #' @return plotly object
    getGSEAEnrichmentPlot = function(.data, ns) {

      p <- CUSOMShinyHelpers::plotGSEAEnrichment(
        pathName = self$GSEATraceName,
        stats = .data$ranks,
        res = .data$gsea,
        title = glue::glue("T21 vs. Control:\n{self$GSEATraceName}")
      ) |>
      plotly::layout(
        margin = list(
          autoexpand = TRUE,
          l = 10,
          r = 30,
          t = 75
        )
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "svg",
          filename = glue::glue('{self$applicationName} - {self$Study} GSEA Enrichment Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
          width = NULL,
          height = NULL
        ),
        modeBarButtons = list(
          list("zoom2d"),
          list("zoomIn2d"),
          list("zoomOut2d"),
          list("resetScale2d"),
          list("toImage")
        )
      )

      p$x$source <- ns("GSEAEnrichmentPlot")

      p

    }

  )
)

#' small helper function to determin plot method for instance/namespace
#' @param analysisType - string - analysis type for this instance/namespace
#' @param analyteCount int - number of selected analytes
#' @return string
getAnalytePlotMethod <- function(analysisType, analyteCount) {
  if(analyteCount > 1) {
    return("Heatmap")
  }
  else if(analysisType == "Categorical") {
    return("boxplot")
  }
  else if(analysisType == "Continuous") {
    return("scatterplot")
  }
  else {
    return("Unknown")
  }
}
