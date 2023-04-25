#' @export
condition_correlates_heatmap_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      id = ns("AnalyteContentBox"),
      title = "",
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinycustomloader::withLoader(
        plotly::plotlyOutput(
          ns("Heatmap"),
          height = "600px",
          width = "99%"
        ),
        type = "html",
        loader = "dnaspin"
      )
    )
  )
}

#' @export
condition_correlates_heatmap_plot_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    AnalyteData <- eventReactive(c(gargoyle::watch("get_volcano_data", session = session)),{

      r6$AnalyteData

    })

    output$Heatmap <- plotly::renderPlotly({

      validate(
        need(!is.null(AnalyteData()),"")
      )
      AnalyteData() |>
        r6$getHeatmapPlot(ns)
    })

    observeEvent(plotly::event_data("plotly_click", priority = "event", source = ns("HeatmapPlot"), session = session), {

      validate(
        need(!is.null(r6$HeatmapData),"")
      )

      e <- plotly::event_data("plotly_click", priority = "event", source = ns("HeatmapPlot"), session = session)

      key <- r6$HeatmapData |>
        dplyr::mutate(z = round(z, 6)) |>
        dplyr::filter(
          r == e$y,
          z == round(e$z, 6)
        ) |>
        dplyr::select(Analyte) |>
        dplyr::pull() |>
        as.character()

      r6$Analyte <- key

      gargoyle::trigger("sync_analyte_choice", session = session)

    }, ignoreInit = TRUE)

  })

}
