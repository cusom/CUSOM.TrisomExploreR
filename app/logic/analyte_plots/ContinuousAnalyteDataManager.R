box::use(
  dplyr[select, filter, mutate, case_when],
  glue[glue],
  plotly[layout, config],
  htmlwidgets[onRender],
  rlang[sym]
)

box::use(
  app/logic/analyte_plots/AnalyteDataManager[FeatureAnalysisAnalyteDataManager],
  app/logic/shared/statistical_analysis[getStatTestByKeyGroup, getLinearModelWithInteraction],
  app/logic/analyte_plots/analyte_plots[getScatterPlotByGroup],
)

#' @export
ContinuousFeatureAnalysisAnalyteDataManager <- R6::R6Class(
  "ContinuousFeatureAnalysisAnalyteDataManager",
  inherit = FeatureAnalysisAnalyteDataManager,
  private = list(),
  active = list(
    AnalytePlotMethod = function(value) {
      if (missing(value)) {
        if (self$AnalysisMode == "single") {
          return("scatterplot")
        } else {
          return("boxplot")
        }
      }
    },
    AnalytePlotTitle =  function(value) {
      if (missing(value)) {
        if (self$GroupVariableCount == 1) {
          return(
            glue(
              "Effect of {self$analysisVariableLabel} in {self$Karyotype} on {self$Analyte()}")
            )
        } else {
          return(
            glue(
              "Comparison of {self$analysisVariableLabel} trajectories between karyotype for {self$Analyte()}"
            )
          )
        }
      }
    }
  ),
  public = list(
    initialize = function(analysis_config, study_data, analyte, summary_data) {
      super$initialize(analysis_config, study_data, analyte, summary_data)
    },
    set_single_analyte_data = function() {
      self$AnalyteData <- self$StudyData() |>
        filter(
          Analyte %in% self$Analyte(),
          log2MeasuredValue != Inf,
          log2MeasuredValue != -Inf
        ) |>
        mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue("log<sub>2</sub>({Measurement})"),
          highlightGroup = case_when(
            1 == 1 ~ NA
          )
        )
      return(invisible(self$AnalyteData))
    },
    plot_single = function(.data, ns) {
      p <- .data |>
        mutate(
          text = glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
        ) |>
        getScatterPlotByGroup(
          key = LabID,
          x = !!sym(self$analysisVariable),
          y = log2MeasuredValue,
          group = Karyotype,
          groupBaselineLabel = "Control",
          text = text,
          addFitLines = TRUE,
          plotName = "ScatterPlot"
        ) |>
        layout(
          legend = list(
            orientation = "h",
            itemclick = "toggleothers",
            itemsizing = "constant",
            itemwidth = 30,
            valign = "middle",
            xanchor = "center",
            x = 0.5,
            y = -0.10,
            title = list(
              text = ""
            )
          ),
          title = list(
            text = self$AnalytePlotTitle
          ),
          xaxis = list(
            title = list(
              text = self$analysisVariable
            )
          ),
          yaxis = list(
            title = list(
              text = glue("{.data$log2Measurement[1]}")
            )
          ),
          annotations = list(
            list(
              x = 0.5,
              y = 1.025,
              text = glue("{self$AnalytePlotStatAnnotation}"),
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
          margin = list(t = 75)
        ) |>
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue(
              "{self$applicationName} - {self$Analyte()} Analyte Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
            ),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            #list(plotlyCustomIcons$AnalytePlotTutorial),
            list("toImage")
          )
        ) |>
        onRender(
        'function(el) {
            el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
          }'
        )

      p$x$source <- ns("ScatterPlot")

      return(p)

    }
  )
)
