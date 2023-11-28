#' Create celltype boxplots for TrisomExploreR cell type analysis
#' @param id - string - id for this module namespace
#' @export
cell_type_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      id = ns("AnalyteContent"),
      title = "",
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinycustomloader::withLoader(
        plotly::plotlyOutput(
          ns("AnalyteBoxPlot"),
          height = "700px"
        ),
        type = "html",
        loader = "dnaspin"
      )
    )
  )
}

#' Server logic for TrisomExploreR celltype boxplots
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @importFrom gargoyle watch
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyProxy
#' @importFrom plotly plotlyProxyInvoke
#' @importFrom shinyjs runjs
#' @importFrom promises future_promise %...>%
#' @export
cell_type_plot_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    AnalyteData <- shiny::eventReactive(
      c(gargoyle::watch("render_analyte_plot", session = session)), {

      shiny::validate(
        shiny::need(r6$Platform != "", ""),
        shiny::need(r6$Analyte != "", ""),
        shiny::need(r6$CellType != "", "")
      )

      promises::future_promise({
        r6$getAnalyteData()
      })

    }, ignoreInit = TRUE)

    output$AnalyteBoxPlot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(AnalyteData()), "")
      )
      AnalyteData() %...>%
          r6$getPlot(ns)
    })

  })


}
