#' R6 Class to manage Correlates Analysis for Immune Maps data
#' @description
#' Enables analysis of cross-omics correlations for Immune Maps
#' Subclass of CorrelatesManager
#' @export
ImmuneMapCorrelatesManager <- R6::R6Class(
  "ImmuneMapCorrelatesManager",
  inherit = CorrelatesManager,
  private = list(),
  public = list(

    #' @description
    #' Create a new instance of ImmuneMapCorrelatesManager object
    #' @param applicationName string - applicationName
    #' @param id string - namespace for this class
    #' @param namespace_config tibble - configuration values for this namespace instance of the object
    #' @param remoteDB R6 class to manage remote database queries
    #' @param localDB R6 class to manange local database queries
    #' @return A new `ImmuneMapCorrelatesManager` object.
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB) {
      super$initialize(applicationName, id, namespace_config, remoteDB, localDB)
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
          dplyr::rename("QueryAnalyteID" = QueryAnalyte) |>
          tidyr::separate(
            QueryAnalyteID,
            sep = ";",
            into = c("CellType", "Lineage", "Analyte"),
            remove = FALSE
          ) |>
          dplyr::mutate(QueryAnalyte = glue::glue("{Lineage};{Analyte}")) |>
          dplyr::select(QueryAnalyte, QueryAnalyteID) |>
          dplyr::arrange(QueryAnalyte) |>
          tibble::deframe()
      )
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
          shape = "circle",
          selectedPoint = 0
        )

      a <- .data |>
        CUSOMShinyHelpers::getCorrelationVolcanoAnnotations(
          foldChangeVar = CorrelationValue,
          significanceVariable = `-log10pvalue`,
          selected = selectedPoint,
          arrowLabelTextVar = Analyte,
          titleText = glue::glue("Correlation with {self$QueryAnalyte}:"),
          includeThresholdLabel = FALSE
        )

      p <- .data |>
        dplyr::mutate(log2CorrelationValue = log2(CorrelationValue)) |>
        CUSOMShinyHelpers::addSignificanceGroup(
          foldChangeVar = log2CorrelationValue,
          significanceVariable = `-log10pvalue`,
          adjustedInd = a$parameters$adjustedInd,
          significanceThreshold = a$parameters$significanceThresholdTransformed,
          originalSignificanceThreshold = a$parameters$significanceThreshold
        ) |>
        CUSOMShinyHelpers::getVolcanoPlot(
          foldChangeVariable = log2CorrelationValue,
          significanceVariable = `-log10pvalue`,
          significanceGroup =  significanceGroup,
          text = text,
          key = AnalyteID,
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

    }
  )
)
