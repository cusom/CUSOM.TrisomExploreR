box::use(
  shiny[NS, tagList, fluidRow, column, tabPanel, moduleServer, tags],
  shinydashboard[tabBox]
)

box::use(
  app/view/inputs/inputs_feature_analysis,
  app/view/inputs/inputs_volcano_plot_analyte,
  app/view/plots/plots_volcano,
  app/view/tables/table_volcano,
  app/view/plots/plots_feature_analysis_analyte,
  app/view/tables/table_analyte,
)

#' @export
ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        class = "col-lg-2 col-slim",
        inputs_feature_analysis$ui(ns("inputs"))
      ),
      column(
        width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
        tabBox(
          id = ns("VolcanoPlotBox"),
          title = "",
          height = "auto",
          width = NULL,
          tabPanel(
            title = "Volcano Plot",
            tags$div(
              id = ns("VolcanoContent"),
              plots_volcano$ui(ns("volcano"))
            )
          ),
          tabPanel(
            title = "Volcano Plot Summary Data",
            table_volcano$ui(ns("summary-data"))
          )
        )
      ),
      column(
        width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
        tabBox(
          id = ns("AnalytePlotBox"),
          title = "",
          height = "auto",
          width = NULL,
          tabPanel(
            title = "Analyte Plot",
            value = "AnalytePlot",
            plots_feature_analysis_analyte$ui(ns("analyte"))
          ),
          tabPanel(
            title = "Analyte Sample Level Data",
            value = "AnalyteTable",
            table_analyte$ui(ns("analyte-data"))
          )
        )
      )
    ),
    tags$div(
      id = ns("GSEA-Placeholder")
    )
  )
}

#' @export
server <- function(id, app_config, analysis_config, input_config) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    #base inputs
    inputs <- inputs_feature_analysis$server(
      id = "inputs",
      app_config = app_config,
      analysis_config = analysis_config
    )

    # volcano plot
    analyte <- plots_volcano$server(
      id = "volcano",
      analysis_config = analysis_config,
      app_config = app_config,
      feature = inputs$feature,
      study = inputs$study,
      study_data = inputs$study_data,
      stat_test = inputs$stat_test,
      covariates = inputs$covariates,
      adjustment_method = inputs$adjustment_method,
      parent = session
    )

    table_volcano$server(
      id = "summary-data",
      summary_data = analyte$table_data,
      fold_change_variable = inputs$fold_change_variable,
      adjusted = inputs$adjusted,
      stat_test = inputs$stat_test,
      study = inputs$study,
    )

    # analyte plot
    analyte_data <- plots_feature_analysis_analyte$server(
      id = "analyte",
      analysis_config = analysis_config,
      analyte = analyte$analyte,
      app_config = app_config,
      feature = inputs$feature,
      study = inputs$study,
      study_data = inputs$study_data,
      summary_data = analyte$summary_data,
      analyte_input_name = analyte$analyte_input_name,
      analyte_session = analyte$analyte_session
    )

    table_analyte$server(
      id = "analyte-data",
      analyte = analyte$analyte,
      table_data = analyte_data$table_data
    )

  })

}
