
#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("AnalyteContent"),
      shinydashboardPlus::box(
        id = ns("AnalyteContentBox"),
        title = shiny::tags$div(
          id = ns("AnalyteContentBoxTitle"),
          style = "font-size:12px;display:flex;align-items:center",
          shiny::uiOutput(ns("toggleSidebarLinks")) |>
          bsplus::bs_embed_tooltip(
            title = "Click here to learn more about the selected analyte",
            placement = "right",
            html = TRUE
          )
        ),
        height = "auto",
        width = NULL,
        closable = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        sidebar = shinydashboardPlus::boxSidebar(
          id = ns("sidebarLinks"),
          icon = shiny::icon("cogs", class = "hidden"),
          width = 50,
          shinyWidgets::actionBttn(
            inputId = ns("sidebarLinksCloseBar"),
            label = "close",
            style = "simple",
            color = "primary",
            icon = shiny::icon("bars")
          ),
          shiny::tags$hr(),
          shiny::htmlOutput(ns("ExternalLinksText")),
          shiny::uiOutput(ns("ExternalLinks"))
        ),
        shinycustomloader::withLoader(
          plotly::plotlyOutput(
            ns("AnalytePlot"),
            height = "605px",
            width = "99%"
          ),
          type = "html",
          loader = "dnaspin"
        )
      )
    )
  )
}

#' @export
server <- function(id, r6, analyte, analyte_input_name, analyte_session) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    analyte_data <- shiny::reactive({
      shiny::validate(
        shiny::need(analyte() != "", "")
      )
      # Get Analyte Data
      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = ifelse(
          length(analyte()) == 1,
          glue::glue("Fetching {analyte()} Data..."),
          "Fetching Data..."
        )
      )

      r6$getAnalyteData()

      shinybusy::remove_modal_spinner()

      r6$AnalyteData

    }) |>
      shiny::bindEvent(analyte())

    output$AnalytePlot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(analyte_data()), "")
      )
      analyte_data() |>
        r6$getAnalytePlot(ns)
    })

    # shiny::observeEvent(
    #   plotly::event_data(
    #     "plotly_click",
    #     priority = "event",
    #     source = ns("HeatmapPlot"),
    #     session = session
    #   ), {
    #   shiny::validate(
    #     shiny::need(!is.null(analyte_data()), ""),
    #     shiny::need(!is.null(r6$HeatmapData), "")
    #   )

    #   e <- plotly::event_data(
    #     "plotly_click",
    #     priority = "event",
    #     source = ns("HeatmapPlot"),
    #     session = session
    #   )

    #   key <- r6$HeatmapData |>
    #     dplyr::mutate(z = round(z, 6)) |>
    #     dplyr::filter(
    #       r == e$y,
    #       z == round(e$z, 6)
    #     ) |>
    #     dplyr::select(Analyte) |>
    #     dplyr::pull() |>
    #     as.character()
  
    #   # update analyte source input
    #   shiny::updateSelectizeInput(
    #     session = analyte_session,
    #     inputId = analyte_input_name,
    #     selected = key
    #   )

    # }, domain = session)

    analyteSearchName <- shiny::reactive({#shiny::eventReactive(
      #c(gargoyle::watch("show_analyte_plot", session = session)), {
        r6$AnalyteSearchName
    }) |>
      shiny::bindEvent(analyte(), ignoreInit = FALSE)

    output$toggleSidebarLinks <- shiny::renderUI({
      shiny::validate(
        shiny::need(!is.null(analyteSearchName()), "")
      )
      btn <- shiny::actionButton(
        ns("toggleSidebarLinks"),
        label = glue::glue("Learn more about {analyteSearchName()}"),
        class = "toggle-btn"
      )

      if (r6$AnalytePlotMethod == "Heatmap") {
        shiny::tags$div(
          style = "padding-bottom: 65px;",
          shinyjs::hidden(
            btn
          )
        )
      } else {
        btn
      }
    })

    # output$ExternalLinksText <- shiny::renderText({
    #   CUSOMShinyHelpers::getExternalLinkTooltip(analyteSearchName())
    # })

    shiny::observeEvent(input$toggleSidebarLinks, {
      shinydashboardPlus::updateBoxSidebar(
        id = "sidebarLinks",
        session = session
      )
    })

    shiny::observeEvent(c(input$sidebarLinksCloseBar), {
      shinydashboardPlus::updateBoxSidebar(
        id = "sidebarLinks",
        session = session
      )
    }, ignoreInit = TRUE)

    # output$ExternalLinks <- shiny::renderUI({
    #   CUSOMShinyHelpers::getExternalLinkActionLinks(analyteSearchName(), ns)
    # })

    table_data <- shiny::reactive({
      r6$get_table_data()
    })

    return(
      list(
        "analyte_data" = analyte_data,
        "table_data" = table_data
      )
    )

  })
}
