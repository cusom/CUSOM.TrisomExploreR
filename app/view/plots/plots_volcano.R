box::use(
  shiny[NS, tagList, tags, moduleServer, reactive, validate, need, bindEvent,
    isolate, reactiveVal, observeEvent, req],
  shinydashboardPlus[box],
  shinycustomloader[withLoader],
  plotly[plotlyOutput, renderPlotly, event_data, toWebGL],
  shinybusy[show_modal_spinner, remove_modal_spinner],
)


box::use(
  app/logic/feature_analysis/summary/FeatureAnalysisSummary[getFeatureAnalysisSummary],
  app/logic/shared/plot_utils[set_plot_source, object_is_rendered],
  app/view/inputs/inputs_volcano_plot_analyte,
  app/view/inputs/inputs_GSEA_analysis,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = tags$div(
        class = "volcano-top-input-panel",
        inputs_volcano_plot_analyte$ui(ns("volcano-analyte")),
        inputs_GSEA_analysis$ui(ns("gsea"))
      ),
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      withLoader(
        plotlyOutput(
          ns("plot"),
          height = "600px",
          width = "99%"
        ),
        type = "html",
        loader = "dnaspin"
      ),
      tags$div(
        id = ns("volcanoMultiSelectTextPlaceholder")
      )
    )
  )
}

#' @export
server <- function(id, analysis_config, app_config, feature, study, study_data, study_plan = NULL, stat_test,
  covariates, adjustment_method, comparison = NULL, ...) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    r6 <- reactive({
      req(study_data())

      getFeatureAnalysisSummary(
        analysis_config = analysis_config$get_analysis_config(feature()),
        app_config = app_config,
        study = study(),
        study_data = study_data(),
        study_plan = if (is.null(study_plan)) NULL else study_plan(),
        stat_test = stat_test(),
        covariates = covariates(),
        adjustment_method = adjustment_method()
      )
    })

    summary_data <- reactive({
      validate(
        need(!is.null(study_data()), "")
      )

      show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = "Calculating Statistics for Volcano Plot..."
        )
      on.exit(remove_modal_spinner(), add = TRUE)

      selected_comparison <- if (is.null(comparison)) NULL else isolate(comparison())

      if (is.null(selected_comparison) || !nzchar(trimws(selected_comparison))) {
        r6()$get_summary_data(study_data())
      } else {
        r6()$get_summary_data(
          study_data(),
          comparison = selected_comparison
        )
      }

    })

    if (is.null(comparison)) {
      summary_data <- summary_data |>
        bindEvent(study_data(), ignoreInit = FALSE)
    } else {
      summary_data <- summary_data |>
        bindEvent(study_data(), comparison(), ignoreInit = FALSE)
    }

    output$plot <- renderPlotly({

      validate(
        need(!is.null(summary_data()), "")
      )

      isolate({

        show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = "Rendering Volcano Plot..."
        )
        on.exit(remove_modal_spinner(), add = TRUE)

        summary_data() |>
          r6()$get_summary_plot() |>
          set_plot_source(ns("plot")) |>
          toWebGL()

      })

    })

    plot_click_data <- reactive({
      validate(
        need(!is.null(summary_data()), ""),
        need(object_is_rendered(session, ns("plot")), "")
      )
      event_data(
        "plotly_click",
        priority = "event",
        source = ns("plot")
      )
    })

    plot_selected_data <- reactive({
      validate(
        need(!is.null(summary_data()), ""),
        need(object_is_rendered(session, ns("plot")), "")
      )
      event_data(
        "plotly_selected",
        priority = "event",
        source = ns("plot")
      )
    })

    analyte <- inputs_volcano_plot_analyte$server(
      id = "volcano-analyte",
      r6 = r6,
      summary_data = summary_data,
      plot_click_data = plot_click_data,
      plot_selected_data = plot_selected_data,
      summary_plot_name = ns("plot"),
      parent = session
    )

    inputs_GSEA_analysis$server(
      id = "gsea",
      summary_data = summary_data,
      study = study,
      ...
    )

    table_data <- reactive({
      r6()$get_table_data()
    })

    return(
      list(
        summary_data = summary_data,
        table_data = table_data,
        fold_change_var = reactive({r6()$fold_change_var}),
        stat_test = reactive({r6()$stat_test}),
        analyte = analyte$analyte,
        analyte_input_name = analyte$analyte_input_name,
        analyte_session = analyte$analyte_session
      )
    )

  })
}
