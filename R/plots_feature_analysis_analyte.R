#' @export
feature_analysis_analyte_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("AnalyteContent"),
      shinydashboardPlus::box(
        id = ns("AnalyteContentBox"),
        title = div(
          id = ns("AnalyteContentBoxTitle"),
          style = "font-size:12px;display:flex;align-items:center",
          uiOutput(ns("toggleSidebarLinks")) |>
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
            icon = icon("bars")
          ),
          tags$hr(),
          htmlOutput(ns("ExternalLinksText")),
          uiOutput(ns("ExternalLinks"))
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
feature_analysis_analyte_plot_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    AnalyteData <- eventReactive(c(gargoyle::watch("show_analyte_plot", session = session)),{
      
      validate(
        need(r6$Analyte != "", "")
      )

      # Get Analyte Data
      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = ifelse(length(r6$Analyte) == 1, glue::glue("Fetching {r6$Analyte} Data..."), "Fetching Data...")
      )

      r6$updateAnalyteAttributes()

      r6$getAnalyteData()

      shinybusy::remove_modal_spinner()

      shiny::isolate(r6$AnalyteData)

    }, ignoreInit = TRUE )

    output$AnalytePlot <- plotly::renderPlotly({

      validate(
        need(!is.null(AnalyteData()), "")
      )

      p <- AnalyteData() |>
        r6$getAnalytePlot(ns)

    })

    observeEvent(plotly::event_data("plotly_click", priority = "event", source = ns("HeatmapPlot"), session = session), {

      validate(
        need(!is.null(r6$HeatmapData),"")
      )

      e <- plotly::event_data("plotly_click", priority = "event", source = ns("HeatmapPlot"), session = session)

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

    }, domain = session)

    analyteSearchName <- eventReactive(c(gargoyle::watch("show_analyte_plot", session = session)),
      {
        r6$AnalyteSearchName
      },
      ignoreInit = TRUE
    )

    output$toggleSidebarLinks <- renderUI({
      validate(
        need(!is.null(analyteSearchName()), "")
      )
      btn <- actionButton(
        ns("toggleSidebarLinks"),
        label = glue::glue("Learn more about {analyteSearchName()}"),
        class = "toggle-btn"
      )

      if (r6$AnalytePlotMethod == "Heatmap") {
        tags$div(
          style = "padding-bottom: 65px;",
          shinyjs::hidden(
            btn
          )
        )
      } else {
        btn
      }
    })

    output$ExternalLinksText <- renderText({
      CUSOMShinyHelpers::getExternalLinkTooltip(analyteSearchName())
    })

    observeEvent(input$toggleSidebarLinks, {
      shinydashboardPlus::updateBoxSidebar(
        id = "sidebarLinks",
        session = session
      )
    })

    observeEvent(c(input$sidebarLinksCloseBar),
      {
        shinydashboardPlus::updateBoxSidebar(
          id = "sidebarLinks",
          session = session
        )
      },
      ignoreInit = TRUE
    )

    output$ExternalLinks <- renderUI({
      CUSOMShinyHelpers::getExternalLinkActionLinks(analyteSearchName(), ns)
    })
  })
}
