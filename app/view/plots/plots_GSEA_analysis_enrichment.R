
#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("GSEAEnrichment"),
      shinycustomloader::withLoader(
        plotly::plotlyOutput(
          outputId = ns("plot"),
          width = "99%",
          height = "auto"
        ),
        type = "html",
        loader = "loader4"
      )
    )
  )
}

#' @export
server <- function(id, r6, pathway_data, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$plot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(nrow(pathway_data()) > 0, "")
      )

      r6$getGSEAEnrichmentPlot(
        pathway_data(),
        ns
      )

    })

  })
}
