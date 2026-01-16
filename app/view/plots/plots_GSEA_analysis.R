box::use(
  shiny[moduleServer, NS, tagList, tags, uiOutput, renderUI, actionButton, icon, selectizeInput, updateSelectizeInput, validate, need],
  plotly[renderPlotly, event_data],
)

box::use(
  app/logic/plots/plot_utils[toggle_GSEA_volcano_plot_trace],
)


#' Create GSEA plot for TrisomExploreR GSEA pathway analysis
#' @param id - string - id for this module namespace
#' @importFrom shinydashboardPlus box
#' @importFrom shinyjs hidden
#' @importFrom plotly plotlyOutput
#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::hidden(
      shiny::selectizeInput(
        inputId = ns("GSEASelectedAnalytes"),
        label = "",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      )
    ),
    plotly::plotlyOutput(
      ns("GSEAPlot"),
      height = "650px",
      width = "99%"
    )
  )
}

#' Server logic / processing for TrisomExploreR GSEA plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @param parent shiny session - parent shiny session
#' @importFrom gargoyle watch
#' @importFrom gargoyle trigger
#' @importFrom plotly renderPlotly
#' @importFrom plotly event_data
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom CUSOMShinyHelpers parseDelimitedString
#' @importFrom stringr str_split
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy remove_modal_spinner
#' @export
server <- function(id, r6, GSEAData, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$GSEAPlot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(GSEAData()), "")
      )

      GSEAData() |>
        r6$getGSEAPlot(ns)
    })

    shiny::observeEvent(
      plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("GSEAPlot"),
        session = session
        ), {

      e <- plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("GSEAPlot"),
        session = session
      )

      r6$getSelectedGSEAPathwayData(e)
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "GSEASelectedAnalytes",
        choices = e$customdata,
        selected = e$customdata
      )

    })

    shiny::observeEvent(c(input$GSEASelectedAnalytes), {
      shiny::validate(
        shiny::need(!is.null(input$GSEASelectedAnalytes), "")
      )

      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = glue::glue("Fetching {r6$GSEATraceName} data...")
      )
    
      r6$getGSEAPathwayData(r6$GSEATraceName)

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

      shinybusy::remove_modal_spinner()

    }, ignoreNULL = TRUE, ignoreInit = TRUE, domain = session)

  })
}
