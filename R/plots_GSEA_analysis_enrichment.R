#' @export
feature_analysis_GSEA_enrichment_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
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

#' @export
feature_analysis_GSEA_enrichment_plot_server <- function(id, r6, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    GSEAEnrichmentData <- eventReactive(c(gargoyle::watch("get_GSEA_path_data", session = session)),{
      r6$GSEAData
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$GSEAEnrichmentPlot <- plotly::renderPlotly({
      validate(
        need(!is.null(GSEAEnrichmentData()), "")
      )

      GSEAEnrichmentData() |>
        r6$getGSEAEnrichmentPlot(ns)

    })

  })
}
