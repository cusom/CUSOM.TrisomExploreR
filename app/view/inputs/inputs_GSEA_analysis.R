box::use(
  app/logic/gsea_analysis/GSEAManager[GSEAManager],
  app/view/plots/plots_GSEA_analysis,
  app/view/plots/plots_GSEA_analysis_enrichment
  # app/logic/table_GSEA_analysis[feature_analysis_GSEA_summary_data_ui, feature_analysis_GSEA_summary_data_server],
  # app/logic/plots_GSEA_analysis_enrichment[feature_analysis_GSEA_enrichment_plot_ui, feature_analysis_GSEA_enrichment_plot_server]
)

#' @export
ui <- function(id, input_config) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("ConfigureGSEA"))
  )
}

#' @export
server <-  function(id, Study, VolcanoSummaryData, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    r6 <- GSEAManager$new(
      Study = Study,
      VolcanoSummaryData = VolcanoSummaryData
    )

    output$ConfigureGSEA <- shiny::renderUI({
      shiny::validate(
        shiny::need(!is.null(Study()), "")
      )

      shiny::tagList(
        bsplus::bs_modal(
          id = ns("configure-GSEA"),
          title = shiny::tags$h3(glue::glue("Pathway Analysis Options:")),
          size = "medium",
          body = list(
            shiny::tags$div(
              shiny::actionButton(
                inputId = ns("run"),
                label = "Run New GSEA Analysis",
                style = "float:left;",
                icon = shiny::icon("play")
              ),
              shiny::actionButton(
                inputId = ns("clear"),
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
          inputId = ns("configure"),
          label = "Pathways",
          icon = shiny::icon("network-wired")
        ) |>
          shiny::tagAppendAttributes(class = r6$addGSEAInputClass()) |>
          bsplus::bs_attach_modal(id_modal = ns("configure-GSEA"))
      )

    })

    shiny::observeEvent(c(input$run), {

      shiny::validate(
        shiny::need(input$run > 0, "")
      )
      shiny::insertTab(
        session = parent,
        inputId = "AnalytePlotBox",
        shiny::tabPanel(
          title = "GSEA Hallmarks",
          plots_GSEA_analysis$ui(ns("hallmarks"))
        ),
        target = NULL,
        select = TRUE
      )

      shiny::insertTab(
        session = parent,
        inputId = "AnalytePlotBox",
        tab = shiny::tabPanel(
          title = "GSEA Enrichment",
          plots_GSEA_analysis_enrichment$ui(ns("enrichment"))
        ),
        target = NULL,
        select = FALSE
      )
      
      # shiny::insertUI(
      #   session = parent,
      #   selector = paste0("#", parent$ns("GSEA-Placeholder")),
      #   where = "afterEnd",
      #   ui = shiny::tags$div(
      #     id = parent$ns("GSEA-Content"),
      #     shiny::fluidRow(
      #       shiny::column(
      #         width = 12, class = "col-lg-5", offset = 2,
      #         #feature_analysis_GSEA_summary_data_ui(ns("GSEA-summary-data"))
      #       ),
      #       shiny::column(
      #         width = 12, class = "col-lg-5",
      #         plots_GSEA_analysis_enrichment$ui(ns("GSEA-enrichment-plot"))
      #       )
      #     )
      #   )
      # )


      shinyjs::click("configure")

    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$clear), {

      shiny::validate(
        shiny::need(input$clear > 0, "")
      )

      shiny::removeUI(
        session = parent,
        selector = paste0("#", parent$ns("GSEA-Content"))
      )

      shiny::removeTab(
        session = parent,
        inputId = "AnalytePlotBox",
        target = "GSEA Hallmarks"
      )

      # toggle_GSEA_volcano_plot_trace(
      #   session = session,
      #   ns = ns,
      #   plot_name = "VolcanoPlot",
      #   r6 = r6,
      #   action = "remove"
      # )

      shinyjs::click("configure")

    }, ignoreInit = TRUE)

    gsea_data <- shiny::reactive({
      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = "Calculating GSEA Data..."
      )
      r6$getGSEAData()
      data <- r6$GSEAData
      shinybusy::remove_modal_spinner()
      return(data)
    }) |>
      shiny::bindEvent(input$run)

    gsea_plot_output <- plots_GSEA_analysis$server(
      id = "hallmarks",
      r6 = r6,
      gsea_data = gsea_data,
      parent = parent
    )

    shiny::observeEvent(gsea_plot_output$plot_click_data(), {
      # sub-plot had click:
      if (nrow(gsea_plot_output$plot_click_data()) > 0) {
        shiny::updateTabsetPanel(
          session = parent,
          inputId = "AnalytePlotBox",
          selected = "GSEA Enrichment"
        )
      } else {
        shiny::updateTabsetPanel(
          session = parent,
          inputId = "AnalytePlotBox",
          selected = "GSEA Hallmarks"
        )
      }
    }, ignoreInit = TRUE)

    plots_GSEA_analysis_enrichment$server(
      id = "enrichment",
      r6 = r6,
      pathway_data = gsea_plot_output$plot_click_data,
      parent = parent
    )

    # feature_analysis_GSEA_summary_data_server(
    #   id = "GSEA-summary-data",
    #   r6 = r6,
    #   parent = parent
    # )

  })

}
