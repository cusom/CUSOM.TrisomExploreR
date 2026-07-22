box::use(
  shiny[NS, fluidRow, column, tagList, tags, moduleServer, reactive, validate, need, bindEvent,
    isolate, reactiveVal, observeEvent, req],
  shinydashboardPlus[box],
  shinycustomloader[withLoader],
  plotly[plotlyOutput, renderPlotly, event_data, toWebGL, plotly_empty, layout],
  shinybusy[show_modal_spinner, remove_modal_spinner],
)


box::use(
  app/logic/feature_analysis/summary/FeatureAnalysisSummary[getFeatureAnalysisSummary],
  app/logic/shared/plot_utils[set_plot_source, object_is_rendered],
  app/view/tables/table_volcano,
  app/view/inputs/inputs_volcano_plot_analyte,
  app/view/inputs/inputs_GSEA_analysis,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "align-items-center justify-content-center",
      tags$h4("Summary Data and Volcano Plot"),
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
                    inputs_volcano_plot_analyte$ui(ns("volcano-analyte")),
                  ),
                  tags$li(
                    table_volcano$ui(ns("summary-data"))
                  ),
                  tags$li(
                    inputs_GSEA_analysis$ui(ns("gsea"))
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
                  ns("plot"),
                  height = "600px",
                  width = "99%"
                ),
                type = "html",
                loader = "dnaspin"
              ),
              tags$br(),
              tags$div(
                id = ns("volcanoMultiSelectTextPlaceholder")
              )
            )
          )
        )
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

    empty_plot <- function() {
      plotly_empty() |>
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = "Loading new study...",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          ))
        )
      }

    rendered_plot <- reactiveVal(empty_plot())

    observeEvent(study(), {
      rendered_plot(empty_plot())
    }, ignoreInit = TRUE, priority = 100)

    observeEvent(summary_data(), {
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

        rendered_plot(
          summary_data() |>
            r6()$get_summary_plot() |>
            set_plot_source(ns("plot")) |>
            toWebGL()
        )
      })
    }, ignoreInit = FALSE)

    output$plot <- renderPlotly({
      rendered_plot()
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
      req(summary_data())
      r6()$get_table_data()
    }) |>
    bindEvent(summary_data(), ignoreInit = FALSE)

    adjusted <- reactive({
      adjustment_method() != "none"
    })

    table_volcano$server(
      id = "summary-data",
      summary_data = table_data,
      adjusted = adjusted,
      stat_test = stat_test,
      study = study,
    )

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
