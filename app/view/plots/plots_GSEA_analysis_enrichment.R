
box::use(
  shiny[NS, tagList, tags, moduleServer, validate, need],
  shinycustomloader[withLoader],
  plotly[plotlyOutput, renderPlotly]
)

box::use(
    app/logic/shared/plot_utils[object_is_rendered, set_plot_source],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      id = ns("GSEAEnrichment"),
      withLoader(
        plotlyOutput(
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

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$plot <- renderPlotly({
      validate(
        need(nrow(pathway_data()) > 0, "")
      )

      r6()$render_enrichment_plot() |>
        set_plot_source(ns("plot"))

    })

  })
}
