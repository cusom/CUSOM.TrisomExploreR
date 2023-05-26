#' Create heatmap plot for TrisomExploreR condition correlates analysis
#' @param id - string - id for this module namespace
#' @importFrom shinydashboardPlus box
#' @importFrom shinycustomloader withLoader
#' @importFrom plotly plotlyOutput
#' @export
condition_correlates_heatmap_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
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

#' Server logic for TrisomExploreR condition correlates heatmap plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @import dplyr
#' @importFrom plotly renderPlotly
#' @importFrom plotly event_data
#' @importFrom gargoyle watch
#' @importFrom gargoyle trigger
#' @export
condition_correlates_heatmap_plot_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    AnalyteData <- shiny::eventReactive(
      c(gargoyle::watch("get_volcano_data", session = session)), {
      r6$AnalyteData
    })

    output$Heatmap <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(AnalyteData()), "")
      )
      AnalyteData() |>
        r6$getHeatmapPlot(ns)
    })

    shiny::observeEvent(
      plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("HeatmapPlot"),
        session = session
      ), {

      shiny::validate(
        shiny::need(!is.null(r6$HeatmapData), "")
      )

      e <- plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("HeatmapPlot"),
        session = session
      )

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
