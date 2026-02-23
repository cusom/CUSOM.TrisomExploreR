box::use(
  shiny[moduleServer, NS, tagList, reactive, observeEvent, validate, need],
  plotly[plotlyOutput, renderPlotly, event_data],
  promises[future_promise, `%...>%`],
  shinybusy[show_modal_spinner, remove_modal_spinner],
  glue[glue],
)

box::use(
  app/logic/shared/plot_utils[toggle_GSEA_volcano_plot_trace, object_is_rendered, set_plot_source],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(
      ns("plot"),
      height = "650px",
      width = "99%"
    )
  )
}

#' @export
server <- function(id, r6, gsea_data, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$plot <- renderPlotly({
      validate(
        need(!is.null(gsea_data()), "")
      )
      gsea_data() |>
        r6()$render_gsea_plot() |>
        set_plot_source(ns("plot"))
    })

    plot_click_data <- reactive({
      validate(
        need(object_is_rendered(session, ns("plot")), "")
      )
      event_data(
        "plotly_click",
        priority = "event",
        source = ns("plot")
      )
    })

    observeEvent(c(plot_click_data()), {
      validate(
        need(nrow(plot_click_data()) > 0, "")
      )

      r6()$set_event_data(plot_click_data())

      show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = glue("Fetching {r6()$gsea_trace_name} data...")
      )
      on.exit(remove_modal_spinner(), add = TRUE)

      future_promise({
        r6()$set_GSEA_pathway_data(r6()$gsea_trace_name)
      }) %...>% {
        toggle_GSEA_volcano_plot_trace(
          session = session,
          ns = ns,
          namespace = ns(id),
          plot_name = "VolcanoPlot",
          expected_trace_count = 3,
          analytes = r6()$gsea_analytes,
          trace_name = r6()$gsea_trace_name,
          action = "add"
        )
      }

    }, ignoreNULL = TRUE, ignoreInit = TRUE, domain = session)

    return(
      list(
        "plot_click_data" = plot_click_data
      )
    )

  })
}
