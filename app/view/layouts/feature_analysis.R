box::use(
  shiny[NS, tagList, fluidRow, column, tabPanel, moduleServer, tags],
  shinydashboard[tabBox]
)

box::use(
  app/view/inputs/inputs_feature_analysis,
  app/view/inputs/inputs_volcano_plot_analyte,
  app/view/plots/plots_volcano,
  app/view/plots/plots_feature_analysis_analyte,
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
        width = 12, class = "col-lg-5 col-slim",
        tags$div(
          id = ns("SummaryDataContent"),
          tags$div(
            id = ns("VolcanoPlotContent"),
            plots_volcano$ui(ns("volcano"))
          )
        )
      ),
      column(
        width = 12, class = "col-lg-5 col-slim",
        tags$div(
          id = ns("AnalyteDataContent"),
          tags$div(
            id = ns("AnalytePlotContent"),
            plots_feature_analysis_analyte$ui(ns("analyte"))
          )
        )
      )
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
      study_plan = inputs$study_plan,
      stat_test = inputs$stat_test,
      covariates = inputs$covariates,
      adjustment_method = inputs$adjustment_method,
      adjusted = inputs$adjusted,
      fold_change_variable = inputs$fold_change_variable,
      parent = session
    )

    # analyte plot
    analyte_data <- plots_feature_analysis_analyte$server(
      id = "analyte",
      analysis_config = analysis_config,
      analyte = analyte$analyte,
      app_config = app_config,
      feature = inputs$feature,
      study = inputs$study,
      karyotype = inputs$karyotype,
      study_data = inputs$study_data,
      study_plan = inputs$study_plan,
      summary_data = analyte$summary_data,
      analyte_input_name = analyte$analyte_input_name,
      analyte_session = analyte$analyte_session
    )

  })

}
