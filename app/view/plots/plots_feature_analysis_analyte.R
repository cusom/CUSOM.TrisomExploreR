#' Create analyte plot for TrisomExploreR feature analysis
#' @param id - string - id for this module namespace
#' @importFrom shinydashboardPlus box
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom shinydashboardPlus boxSidebar
#' @importFrom shinycustomloader withLoader
#' @importFrom plotly plotlyOutput
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

#' Server logic / processing for TrisomExploreR feature analysis analyte plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @import dplyr
#' @import glue
#' @importFrom gargoyle watch
#' @importFrom gargoyle trigger
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom plotly renderPlotly
#' @importFrom plotly event_data
#' @importFrom shinyjs hidden
#' @importFrom CUSOMShinyHelpers getExternalLinkTooltip
#' @importFrom CUSOMShinyHelpers getExternalLinkActionLinks
#' @importFrom shinydashboardPlus updateBoxSidebar
#' @export
server <- function(id, r6, feature, feature_input_name, feature_session) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    feature_data <- shiny::reactive({
      shiny::validate(
        shiny::need(feature() != "", "")
      )
      # Get Analyte Data
      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = ifelse(
          length(feature()) == 1,
          glue::glue("Fetching {feature()} Data..."),
          "Fetching Data..."
        )
      )

      r6$getAnalyteData()

      shinybusy::remove_modal_spinner()

      r6$AnalyteData

    }) |>
      shiny::bindEvent(feature())

    output$AnalytePlot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(feature_data()), "")
      )
      feature_data() |>
        r6$getAnalytePlot(ns)
    })

    shiny::observeEvent(
      plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("HeatmapPlot"),
        session = session
      ), {

      shiny::validate(
        shiny::need(!is.null(r6$HeatmapData), "")
      )

      e <- plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("HeatmapPlot"),
        session = session
      )

      key <- r6$HeatmapData |>
        dplyr::mutate(z = round(z, 6)) |>
        dplyr::filter(
          r == e$y,
          z == round(e$z, 6)
        ) |>
        dplyr::select(Analyte) |>
        dplyr::pull() |>
        as.character()
  
      # update feature source input
      shiny::updateSelectizeInput(
        session = feature_session,
        inputId = feature_input_name,
        selected = key
      )

    }, domain = session)

    analyteSearchName <- shiny::reactive({#shiny::eventReactive(
      #c(gargoyle::watch("show_analyte_plot", session = session)), {
        r6$AnalyteSearchName
    }) |>
      shiny::bindEvent(feature(), ignoreInit = FALSE)

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
  })
}
