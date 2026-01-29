box::use(
  shiny[moduleServer, NS, tagList, tags, uiOutput, renderUI, actionButton, 
      icon, selectizeInput, updateSelectizeInput, validate, need],
  plotly[renderPlotly, event_data],
  promises[future_promise, `%...>%`],
)

box::use(
  app/logic/shared/plot_utils[toggle_GSEA_volcano_plot_trace, object_is_rendered],
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(
      ns("plot"),
      height = "650px",
      width = "99%"
    )
  )
}

#' @export
server <- function(id, r6, gsea_data, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$plot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(gsea_data()), "")
      )
      gsea_data() |>
        r6$getGSEAPlot(ns("plot"))
    })

    plot_click_data <- shiny::reactive({
      shiny::validate(
        shiny::need(object_is_rendered(session, ns("plot")), "")
      )
      plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("plot")
      )
    })

    shiny::observeEvent(c(plot_click_data()), {
      shiny::validate(
        shiny::need(nrow(plot_click_data()) > 0, "")
      )

      r6$event_data <- plot_click_data()

      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = glue::glue("Fetching {r6$GSEATraceName} data...")
      )

      future_promise({
        r6$set_GSEA_pathway_data(r6$GSEATraceName)
      }) %...>% {
        toggle_GSEA_volcano_plot_trace(
          session = session,
          ns = ns,
          namespace = ns(id),
          plot_name = "VolcanoPlot",
          expected_trace_count = 3,
          analytes = r6$GSEAAnalytes,
          trace_name = r6$GSEATraceName,
          action = "add"
        )
      }

      shinybusy::remove_modal_spinner()

    }, ignoreNULL = TRUE, ignoreInit = TRUE, domain = session)

    return(
      list(
        "plot_click_data" = plot_click_data
      )
    )

  })
}
