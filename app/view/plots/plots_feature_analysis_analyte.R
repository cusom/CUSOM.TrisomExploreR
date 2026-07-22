box::use(
  shiny[NS, tagList, tags, fluidRow, column, uiOutput, htmlOutput, icon, 
    moduleServer, reactive,reactiveVal, observeEvent, req, validate, need, 
    bindEvent, renderUI, actionButton],
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
  app/logic/shared/string_utils[parse_delimited_string],
  app/view/inputs/inputs_analyte_links,
  app/view/tables/table_analyte,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "align-items-center justify-content-center",
      tags$h4("Sample Level Data and Analyte Plot"),
      tags$div(
        class = "align-items-center mx-auto mt-2",
        fluidRow(
          column(
            width = 12,
            offset = 0,
            class = "col-xs-12 col-lg-12 vh-95 pl-0 pr-0 ml-0 mr-0",
            tags$div(
              class = "container-fluid plot-toolbar-row",
              style = "padding-bottom: 2px;",
              tags$div(
                class = "container d-flex align-items-left justify-content-between flex-wrap",
                tags$ul(
                  class = "nav navdcc d-flex align-items-center justify-content-between flex-wrap mx-auto",
                  tags$li(
                    inputs_analyte_links$ui(
                      ns("analyte-links")
                    )
                  ),
                  tags$li(
                    table_analyte$ui(ns("analyte-data"))
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            offset = 0,
            class = "col-xs-12 col-lg-12 vh-95 pl-0 pr-0 ml-0 mr-0",
            tags$div(
              withLoader(
                plotlyOutput(
                  ns("AnalytePlot"),
                  height = "600px",
                  width = "99%"
                ),
                type = "html",
                loader = "dnaspin"
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, analysis_config, app_config, analyte, feature, study, karyotype = NULL, study_data, study_plan = NULL,
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
        selected_karyotypes = if (is.null(karyotype)) NULL else karyotype(),
        study_data = study_data(),
        study_plan = if (is.null(study_plan)) NULL else study_plan(),
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

    table_data <- reactive({
      r6()$get_table_data()
    })

    inputs_analyte_links$server(
      id = "analyte-links",
      analyte = analyte
    )

    table_analyte$server(
      id = "analyte-data",
      analyte = analyte,
      table_data = table_data
    )

    return(
      list(
        "analyte_data" = analyte_data,
        "table_data" = table_data
      )
    )

  })
}
