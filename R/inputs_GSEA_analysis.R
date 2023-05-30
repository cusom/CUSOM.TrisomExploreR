#' Create input widgets for TrisomExploreR GSEA pathway analysis
#' @param id - string - id for this module namespace
#' @param input_config - list - list of default values for various input widgets
#' @export
GSEA_analysis_inputs_ui <- function(id, input_config) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("ConfigureGSEA"))
  )
}

#' Server-side logic / processing for TrisomExploreR GSEA pathway analysis inputs
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic for inputs
#' @param parent - session object - parent session
#' @import glue
#' @importFrom gargoyle watch
#' @importFrom bsplus bs_modal
#' @importFrom bsplus bs_modal_closebutton
#' @importFrom bsplus bs_attach_modal
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy remove_modal_spinner
#' @export
GSEA_analysis_inputs_server <-  function(id, r6, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    GSEAButtonClass <- shiny::eventReactive(
      c(gargoyle::watch("validate_GSEA", session = session)), {
      r6$addGSEAInputClass()
    })

    output$ConfigureGSEA <- shiny::renderUI({
      shiny::tagList(
        bsplus::bs_modal(
          id = ns("configure-GSEA"),
          title = shiny::tags$h3(glue::glue("Pathway Analysis Options:")),
          size = "medium",
          body = list(
            shiny::tags$div(
              shiny::actionButton(
                inputId = ns("RunGSEA"),
                label = "Run New GSEA Analysis",
                style = "float:left;",
                icon = shiny::icon("play")
              ),
              shiny::actionButton(
                inputId = ns("ClearGSEA"),
                label = "Clear All GSEA Analysis",
                style = "float:right;",
                icon = shiny::icon("eraser")
              )
            ),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          footer = bsplus::bs_modal_closebutton(label = "Cancel")
        ),
        shiny::actionButton(
          inputId = ns("ConfigureGSEA"),
          label = "Pathways",
          icon = shiny::icon("network-wired")
        ) |>
          shiny::tagAppendAttributes(class = GSEAButtonClass()) |>
          bsplus::bs_attach_modal(id_modal = ns("configure-GSEA"))
      )

    })

    shiny::observeEvent(c(input$RunGSEA), {

      shiny::validate(
        shiny::need(input$RunGSEA > 0, "")
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
        ui = shiny::tags$div(
          id = parent$ns("GSEA-Content"),
          shiny::fluidRow(
            shiny::column(
              width = 12, class = "col-lg-5", offset = 2,
              TrisomExploreR::feature_analysis_GSEA_summary_data_ui(ns("GSEA-summary-data"))
            ),
            shiny::column(
              width = 12, class = "col-lg-5",
              TrisomExploreR::feature_analysis_GSEA_enrichment_plot_ui(ns("GSEA-enrichment-plot"))
            )
          )
        )
      )

      r6$getGSEAData()

      shinybusy::remove_modal_spinner()

      shinyjs::click("configure-GSEA")

    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$ClearGSEA), {

      shiny::validate(
        shiny::need(input$ClearGSEA > 0, "")
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

      shinyjs::click("configure-GSEA")

    }, ignoreInit = TRUE)

    TrisomExploreR::feature_analysis_GSEA_plot_server(
      id = "GSEA-plot",
      r6 = r6,
      parent = parent
    )

    TrisomExploreR::feature_analysis_GSEA_enrichment_plot_server(
      id = "GSEA-enrichment-plot",
      r6 = r6,
      parent = parent
    )

    TrisomExploreR::feature_analysis_GSEA_summary_data_server(
      id = "GSEA-summary-data",
      r6 = r6,
      parent = parent
    )

  })

}
