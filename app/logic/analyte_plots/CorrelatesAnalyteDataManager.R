box::use(
  app/logic/analyte_plots/AnalyteDataManager[FeatureAnalysisAnalyteDataManager],
  app/logic/app_resources/data_services[ODBCQueryManager],
  app/logic/shared/string_helper_functions[parse_delimited_string],
  app/logic/analyte_plots/analyte_plot_helpers[getDensityColors],
  app/logic/analyte_plots/analyte_plots[getScatterPlotWithSmoothing]
)

#' @export
CorrelatesAnalyteDataManager <- R6::R6Class(
  "CorrelatesAnalyteDataManager",
  inherit = FeatureAnalysisAnalyteDataManager,
  private = list(),
  active = list(
    AnalytePlotMethod = function(value) {
      return("scatterplot")
    },
    CompareExperiment = function(value) {
      if (missing(value)) {
        return(
          self$StudyData() |>
            dplyr::distinct(ComparisonExperimentID) |>
            dplyr::pull()
        )
      }
    },
    ComparisonAnalyteKey = function(value) {
      return(
        self$Analyte()
      )
    },
    ComparisonAnalyteLabel = function(value) {
      return(self$AnalyteData[1, "yLabel"])
    },
    ComparisonMeasurement = function(value) {
      return(self$AnalyteData[1, "Measurement.y"])
    },
    QueryExperiment = function(value) {
      return(
        self$StudyData() |>
          dplyr::distinct(QueryExperimentID) |>
          dplyr::pull()
      )
    },
    QueryAnalyte = function(value) {
      return(
        self$StudyData() |>
          dplyr::distinct(QueryAnalyteKey) |>
          dplyr::pull() |>
          as.integer()
      )
    },
    QueryAnalyteLabel = function(value) {
      return(
        self$StudyData() |>
          dplyr::distinct(QueryAnalyte) |>
          dplyr::pull()
      )
    },
    QueryMeasurement = function(value) {
      return(self$AnalyteData[1, "Measurement.x"])
    },
    CorrelationAnalytePlotTitle = function(value) {
      return(
        glue::glue(
          "{parse_delimited_string(self$Analyte(), 1)} \\
          vs. {parse_delimited_string(self$QueryAnalyteLabel, 1)}"
        )
      )
    }
  ),
  public = list(
    remote_db = NULL,
    initialize = function(app_config, analysis_config, study, study_data, analyte, summary_data) {

      super$initialize(app_config, analysis_config, study, study_data, analyte, summary_data)

    },
    getAnalyteData = function() {
      ## Query on X-axis, Comparison on y-axis
      self$AnalyteData <- self$remote_db$getQuery(
          "[shiny].[GetAnalyteDataByExperiment] ?, ?",
          tibble::tibble(
            "ExperimentID" =  self$CompareExperiment,
            "Analyte" = self$ComparisonAnalyteKey
          )
        ) |>
        dplyr::filter(outlier == FALSE) |>
        dplyr::select(LabID, "ComparisonAnalyte" = Analyte,  MeasuredValue, Measurement) |>
        dplyr::rename(y = MeasuredValue) |>
        dplyr::inner_join(
          self$remote_db$getQuery(
            "[shiny].[GetAnalyteDataByExperiment] ?, ?",
            tibble::tibble(
              "ExperimentID" = self$QueryExperiment,
              "Analyte" = self$QueryAnalyte
            )
          ) |>
            dplyr::filter(outlier == FALSE) |>
            dplyr::select(LabID, "QueryAnalyte" = Analyte, MeasuredValue, Measurement) |>
            dplyr::rename(x = MeasuredValue)
          , by = "LabID"
        ) |>
        dplyr::mutate(
          log2x = log2(x),
          log2y = log2(y),
          xLabel = parse_delimited_string(QueryAnalyte, 1),
          yLabel = parse_delimited_string(ComparisonAnalyte, 1)
        )

      return(invisible(self$AnalyteData))
    },

    getAnalytePlot = function(.data, ns) {

      p <- .data |>
        dplyr::mutate(Density = getDensityColors(x, y, transform = TRUE)) |>
        dplyr::arrange(Density) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          text = glue::glue(
            "{xLabel} log<sub>2</sub>({self$ComparisonMeasurement}):{round(log2x,2)}
            {yLabel} log<sub>2</sub>({self$QueryMeasurement}):{round(log2y,2)}
            Density: {Density}")
          ) |>
        getScatterPlotWithSmoothing(
          xVar = log2x,
          yVar = log2y,
          colorVar = Density,
          textVar =  text,
          smoothingMethod = "lm"
        )

      p <- plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          showlegend = TRUE,
          legend = list(
            orientation = "h",
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
            text = shiny::HTML(self$CorrelationAnalytePlotTitle),
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
              text = glue::glue("{self$QueryAnalyteLabel} log<sub>2</sub>({self$QueryMeasurement})"),
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
          margin = list(t = 75)
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


  )
)
