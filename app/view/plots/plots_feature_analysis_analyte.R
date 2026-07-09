box::use(
  shiny[NS, tagList, tags, uiOutput, htmlOutput, icon, moduleServer, reactive,
    reactiveVal, observeEvent, req, validate, need, bindEvent, renderUI,
    actionButton],
  shinydashboardPlus[box, boxSidebar, updateBoxSidebar],
  shinyWidgets[actionBttn],
  shinycustomloader[withLoader],
  plotly[plotlyOutput, renderPlotly],
  shinybusy[show_modal_spinner, remove_modal_spinner],
  glue[glue],
  bsplus[bs_embed_tooltip],
  shinyjs[hidden]
)

box::use(
  app/logic/feature_analysis/analyte/FeatureAnalysisAnalyte[getFeatureAnalysisForAnalyte],
  app/logic/shared/string_utils[parse_delimited_string]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      id = ns("AnalyteContent"),
      box(
        id = ns("AnalyteContentBox"),
        title = tags$div(
          id = ns("AnalyteContentBoxTitle"),
          style = "font-size:12px;display:flex;align-items:center",
          uiOutput(ns("toggleSidebarLinks")) |>
          bs_embed_tooltip(
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
        sidebar = boxSidebar(
          id = ns("sidebarLinks"),
          icon = icon("cogs", class = "hidden"),
          width = 50,
          actionBttn(
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
        withLoader(
          plotlyOutput(
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
server <- function(id, analysis_config, app_config, analyte, feature, study, study_data,
  summary_data, analyte_input_name, analyte_session, ...) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    r6_obj <- reactiveVal(NULL)

    # Recreate the R6 instance when Feature changes
    observeEvent(analyte(), {
      req(analyte())
      inst <- getFeatureAnalysisForAnalyte(
        analysis_config = analysis_config$get_analysis_config(feature()),
        analyte = analyte(),
        app_config = app_config,
        study = study(),
        study_data = study_data(),
        summary_data = summary_data(),
        ...
      )
      r6_obj(inst)
    })

    #expose a reactive that always reads the current instance
    r6 <- reactive({
      req(r6_obj())
      r6_obj()
    })

    analyte_data <- reactive({
      validate(
        need(analyte() != "", "")
      )
      # Get Analyte Data
      show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = ifelse(
          length(analyte()) == 1,
          glue("Fetching {analyte()} Data..."),
          "Fetching Data..."
        )
      )

      data <- r6()$get_analyte_data(analyte())

      remove_modal_spinner()

      data

    }) |>
      bindEvent(analyte())

    output$AnalytePlot <- renderPlotly({
      validate(
        need(!is.null(analyte_data()), "")
      )

      analyte_data() |>
        r6()$get_analyte_plot()
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

    analyteSearchName <- reactive({
        parse_delimited_string(analyte(), 1)
    }) |>
      bindEvent(analyte(), ignoreInit = FALSE)

    output$toggleSidebarLinks <- renderUI({
      validate(
        need(!is.null(analyteSearchName()), "")
      )
      btn <- actionButton(
        ns("toggleSidebarLinks"),
        label = glue("Learn more about {analyteSearchName()}"),
        class = "toggle-btn"
      )

      if (length(analyte()) > 1) {
        tags$div(
          style = "padding-bottom: 65px;",
          hidden(
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

    observeEvent(input$toggleSidebarLinks, {
      updateBoxSidebar(
        id = "sidebarLinks",
        session = session
      )
    })

    observeEvent(c(input$sidebarLinksCloseBar), {
      updateBoxSidebar(
        id = "sidebarLinks",
        session = session
      )
    }, ignoreInit = TRUE)

    # output$ExternalLinks <- shiny::renderUI({
    #   CUSOMShinyHelpers::getExternalLinkActionLinks(analyteSearchName(), ns)
    # })

    table_data <- reactive({
      r6()$get_table_data()
    })

    return(
      list(
        "analyte_data" = analyte_data,
        "table_data" = table_data
      )
    )

  })
}
