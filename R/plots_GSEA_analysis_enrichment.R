#' Create enrichment plot for TrisomExploreR GSEA pathway analysis
#' @param id - string - id for this module namespace
#' @importFrom shinydashboardPlus box
#' @importFrom shinycustomloader withLoader
#' @importFrom plotly plotlyOutput
#' @export
feature_analysis_GSEA_enrichment_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("GSEAEnrichmentPlotBox"),
      shinydashboardPlus::box(
        title = "GSEA Enrichment Plot",
        height = "auto",
        width = NULL,
        closable = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        shinycustomloader::withLoader(
          plotly::plotlyOutput(
            outputId = ns("GSEAEnrichmentPlot"),
            width = "99%",
            height = "400px"
          ),
          type = "html",
          loader = "loader4"
        )
      )
    )
  )
}

#' Server logic / processing for TrisomExploreR GSEA enrichment plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @param parent shiny session - parent shiny session
#' @importFrom gargoyle watch
#' @importFrom plotly renderPlotly
#' @export
feature_analysis_GSEA_enrichment_plot_server <- function(id, r6, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    GSEAEnrichmentData <- shiny::eventReactive(
      c(gargoyle::watch("get_GSEA_path_data", session = session)), {
      r6$GSEAData
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$GSEAEnrichmentPlot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(GSEAEnrichmentData()), "")
      )
      GSEAEnrichmentData() |>
        r6$getGSEAEnrichmentPlot(ns)
    })

  })
}
