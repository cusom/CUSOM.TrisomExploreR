box::use(
  dplyr[select, filter, mutate, case_when, rowwise, ungroup],
  glue[glue],
  plotly[layout, config],
  htmlwidgets[onRender],
  rlang[sym]
)

box::use(
  app/logic/analyte_plots/AnalyteDataManager[FeatureAnalysisAnalyteDataManager],
  app/logic/shared/statistical_analysis[addGroupCount],
  app/logic/analyte_plots/analyte_plots[getBoxPlotWithHighlightGroup],
)

#' @export
CategoricalFeatureAnalysisAnalyteDataManager <- R6::R6Class(
  "CategoricalFeatureAnalysisAnalyteDataManager",
  inherit = FeatureAnalysisAnalyteDataManager,
  private = list(),
  active = list(
    AnalytePlotMethod = function(value) {
      if (missing(value)) {
        if (self$AnalysisMode == "single") {
          return("heatmap")
        } else {
          return("boxplot")
        }
      }
    },
    AnalytePlotTitle =  function(value) {
      if (missing(value)) {
        return(glue("Effect of {self$analysisVariableLabel} on {self$Analyte()}"))
      }
    }
  ),
  public = list(
    initialize = function(app_config, analysis_config, study, study_data, analyte, summary_data) {
      super$initialize(app_config, analysis_config, study, study_data, analyte, summary_data)
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
        ) |>
        rowwise() |>
        addGroupCount(group = !!sym(self$analysisVariable), addLineBreak = FALSE) |>
        select(-n) |>
        ungroup() |>
        mutate(text = glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}"))
      return(invisible(self$AnalyteData))
    },
    plot_single = function(.data, ns) {
      p <- .data |>
        getBoxPlotWithHighlightGroup(
          key = LabID,
          group = !!sym(self$analysisVariable),
          groupBaselineLabel = self$formattedGroupBaselineLabel,
          value = log2MeasuredValue,
          valueLabel = log2Measurement,
          text = text,
          highlightGroup = highlightGroup,
          plotName = glue("{self$namespace}Analyte")
        ) |>
        layout(
          showlegend = TRUE,
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
          annotations = list(
            list(
              x = 0.5,
              y = -0.07,
              text = glue("<b>{self$AnalytePlotXAxisLabel}</b>"),
              xref = "paper",
              yref = "paper",
              axref = "x",
              ayref = "y",
              showarrow = FALSE,
              ax = 0,
              ay = 0,
              font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 14
              )
            ),
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
            ),
            list(
              x = 0.5,
              y = 1,
              xref = "x domain",
              yref = "paper",
              axref = "x domain",
              ax = 1.5,
              ay = 1,
              showarrow = TRUE,
              arrowcolor = "black",
              arrowhead = 0,
              arrowwidth = 0.9
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
              "{self$applicationName} - {self$Analyte()} Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
            ),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            ## list(plotlyCustomIcons$AnalytePlotTutorial),
            list("select2d"),
            list("lasso2d"),
            list("toImage")
            # list(appConfig$plotlyCustomIcons$BoxplotCompareGroup),
            # list(appConfig$plotlyCustomIcons$BoxplotClear)
          )
        ) |>
        onRender(
        'function(el) {
            el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
          }'
        )

      p$x$source <- ns("BoxPlot")

      return(p)

    }
  )
)
