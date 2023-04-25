#' @export
GSEA_analysis_inputs_ui <- function(id, input_config) {
  ns <- shiny::NS(id)
  shiny::tagList(
    uiOutput(ns("ConfigureGSEA"))
  )
}

#' @export
GSEA_analysis_inputs_server <-  function(id, r6, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    GSEAButtonClass <- eventReactive(c(gargoyle::watch("validate_GSEA", session = session)),{
      r6$addGSEAInputClass()
    })

    output$ConfigureGSEA <- renderUI({

      shiny::actionButton(
        inputId = ns("ConfigureGSEA"),
        label = "GSEA",
        icon = icon("cogs")
      ) |>
        shiny::tagAppendAttributes(class = GSEAButtonClass())

    })

    observeEvent(c(input$ConfigureGSEA),{

      validate(
        need(input$ConfigureGSEA > 0, "")
      )

      shiny::showModal(
        shiny::modalDialog(
          title = htmltools::tags$h3(glue::glue("GSEA options:")),
          size = "m",
          easyClose = TRUE,
          list(
            htmltools::tags$div(
              shiny::actionButton(
                inputId = ns("RunGSEA"),
                label = "Run New GSEA Analysis",
                style = "float:left;",
                icon = icon("play")
              ),
              shiny::actionButton(
                inputId = ns("ClearGSEA"),
                label = "Clear All GSEA Analysis",
                style = "float:right;",
                icon = icon("eraser")
              )
            ),
            tags$br(),
            tags$br()
          ),
          footer = shiny::tagList(
            shiny::modalButton(label = "Cancel")
          )
        )
      )

    }, ignoreInit = TRUE)

    observeEvent(c(input$RunGSEA), {

      validate(
        need(input$RunGSEA > 0, "")
      )

      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = "Calculating GSEA Data..."
      )

      shiny::insertTab(
        session = parent,
        inputId = "AnalytePlotBox",
        shiny::tabPanel(
          title = "GSEA Plot",
          TrisomExploreR::feature_analysis_GSEA_plot_ui(ns("GSEA-plot"))
        ),
        target = NULL,
        select = TRUE
      )

      shiny::insertUI(
        session = parent,
        selector = paste0("#", parent$ns("GSEA-Placeholder")),
        where = "afterEnd",
        ui = tags$div(
          id = parent$ns("GSEA-Content"),
          fluidRow(
            column(
              width = 12, class = "col-lg-5", offset = 2,
              TrisomExploreR::feature_analysis_GSEA_summary_data_ui(ns("GSEA-summary-data"))
            ),
            column(
              width = 12, class = "col-lg-5",
              TrisomExploreR::feature_analysis_GSEA_enrichment_plot_ui(ns("GSEA-enrichment-plot"))
            )
          )
        )
      )

      r6$getGSEAData()

      shinybusy::remove_modal_spinner()

      shiny::removeModal()

    }, ignoreInit = TRUE)

    observeEvent(c(input$ClearGSEA),{

      validate(
        need(input$ClearGSEA > 0, "")
      )

      shiny::removeUI(
        session = parent,
        selector = paste0("#", parent$ns("GSEA-Content"))
      )

      shiny::removeTab(
        session = parent,
        inputId = "AnalytePlotBox",
        target = "GSEA Plot"
      )

      toggle_GSEA_volcano_plot_trace(
        session = session,
        ns = ns,
        plot_name = "VolcanoPlot",
        r6 = r6,
        action = "remove"
      )

      shiny::removeModal()

    }, ignoreInit = TRUE)

    TrisomExploreR::feature_analysis_GSEA_plot_server(id = "GSEA-plot", r6 = r6, parent = parent)

    TrisomExploreR::feature_analysis_GSEA_enrichment_plot_server(id = "GSEA-enrichment-plot", r6 = r6, parent = parent)

    TrisomExploreR::feature_analysis_GSEA_summary_data_server(id = "GSEA-summary-data", r6 = r6, parent = parent)

  })

}
