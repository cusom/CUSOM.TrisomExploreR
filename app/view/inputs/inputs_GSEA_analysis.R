box::use(
  shiny[NS, tagList, uiOutput, moduleServer, renderUI, validate, need, tags,
    actionButton, icon, tagAppendAttributes, observeEvent, insertTab, tabPanel,
    removeUI, removeTab, reactive, bindEvent, updateTabsetPanel, reactiveVal, req],
  bsplus[bs_modal, bs_modal_closebutton, bs_attach_modal],
  glue[glue],
  shinyjs[click],
  shinybusy[show_modal_spinner, remove_modal_spinner]
)

box::use(
  app/logic/gsea_analysis/GSEAPathway[getGSEAPathwayAnalysis],
  app/logic/shared/plot_utils[toggle_GSEA_volcano_plot_trace],
  app/view/plots/plots_GSEA_analysis,
  app/view/plots/plots_GSEA_analysis_enrichment
)

#' @export
ui <- function(id, input_config) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ConfigureGSEA"))
  )
}

#' @export
server <-  function(id, study, summary_data, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    is_supported_study <- reactive({
      study_value <- study()
      !is.null(study_value) &&
        nzchar(study_value) &&
        grepl("SOMA|RNA", study_value, ignore.case = TRUE)
    })

    output$ConfigureGSEA <- renderUI({

      is_button_disabled <- !is_supported_study() || is.null(summary_data())

      configure_button <- actionButton(
        inputId = ns("configure"),
        label = "Pathways",
        icon = icon("network-wired")
      )

      if (is_button_disabled) {
        disabled_tooltip <- if (!is_supported_study()) {
          "Pathway analysis is available only for studies containing SOMA or RNA"
        } else {
          "Pathway analysis is unavailable until summary data is loaded"
        }

        configure_button <- configure_button |>
          tagAppendAttributes(
            disabled = "disabled",
            title = disabled_tooltip
          )
      }

      tagList(
        bs_modal(
          id = ns("configure-GSEA"),
          title = tags$h3(glue("Pathway Analysis Options:")),
          size = "medium",
          body = list(
            tags$div(
              actionButton(
                inputId = ns("run"),
                label = "Run New GSEA Analysis",
                style = "float:left;",
                icon = icon("play")
              ),
              actionButton(
                inputId = ns("clear"),
                label = "Clear All GSEA Analysis",
                style = "float:right;",
                icon = icon("eraser")
              )
            ),
            tags$br(),
            tags$br()
          ),
          footer = bs_modal_closebutton(label = "Cancel")
        ),
        configure_button |>
          #tagAppendAttributes(class = r6$addGSEAInputClass()) |>
          bs_attach_modal(id_modal = ns("configure-GSEA"))
      )

    })

    r6_obj <- reactiveVal(NULL)

    #expose a reactive that always reads the current instance
    r6 <- reactive({
      req(r6_obj())
      r6_obj()
    })

    observeEvent(c(input$run), {

      validate(
        need(input$run > 0, ""),
        need(is_supported_study(), "")
      )

      inst <- getGSEAPathwayAnalysis(
        app_config = app_config,
        study = study(),
        summary_data = summary_data()
      )

      r6_obj(inst)

      insertTab(
        session = parent,
        inputId = "AnalytePlotBox",
        tabPanel(
          title = "GSEA Hallmarks",
          plots_GSEA_analysis$ui(ns("hallmarks"))
        ),
        target = NULL,
        select = TRUE
      )

      insertTab(
        session = parent,
        inputId = "AnalytePlotBox",
        tab = tabPanel(
          title = "GSEA Enrichment",
          plots_GSEA_analysis_enrichment$ui(ns("enrichment"))
        ),
        target = NULL,
        select = FALSE
      )

      click("configure")

    }, ignoreInit = TRUE)

    observeEvent(c(input$clear), {

      validate(
        need(input$clear > 0, ""),
        need(is_supported_study(), "")
      )

      removeUI(
        session = parent,
        selector = paste0("#", parent$ns("GSEA-Content"))
      )

      removeTab(
        session = parent,
        inputId = "AnalytePlotBox",
        target = "GSEA Hallmarks"
      )

      removeTab(
        session = parent,
        inputId = "AnalytePlotBox",
        target = "GSEA Enrichment"
      )

      toggle_GSEA_volcano_plot_trace(
        session = session,
        namespace = parent$ns(""),
        plot_name = "plot",
        expected_trace_count = 3,
        analytes = NULL,
        trace_name = NULL,
        action = "clear"
      )

      click("configure")

    }, ignoreInit = TRUE)

    gsea_data <- reactive({
      show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = "Calculating GSEA Data..."
      )
      on.exit(remove_modal_spinner(), add = TRUE)
      r6()$get_gsea_data()
    }) |>
      bindEvent(input$run)

    gsea_plot_output <- plots_GSEA_analysis$server(
      id = "hallmarks",
      r6 = r6,
      gsea_data = gsea_data,
      target_plot_name = "plot",
      parent = parent
    )

    observeEvent(gsea_plot_output$plot_click_data(), {
      # sub-plot had click:
      if (nrow(gsea_plot_output$plot_click_data()) > 0) {
        updateTabsetPanel(
          session = parent,
          inputId = "AnalytePlotBox",
          selected = "GSEA Enrichment"
        )
      } else {
        updateTabsetPanel(
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

  })

}
