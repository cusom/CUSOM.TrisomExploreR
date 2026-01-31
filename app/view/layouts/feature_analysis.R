box::use(
  shiny[tags]
)


box::use(
  app/logic/inputs/inputs_manager_factory[get_inputs_manager],
  app/logic/summary_plots/SummaryDataManagerFactory[getFeatureAnalysisSummaryDataManager],
  app/logic/analyte_plots/AnalyteDataManagerFactory[getFeatureAnalysisAnalyteDataManager],
  app/view/inputs/inputs_feature_analysis,
  app/view/inputs/inputs_volcano_plot_analyte,
  app/view/plots/plots_volcano,
  app/view/tables/table_volcano,
  app/view/plots/plots_feature_analysis_analyte,
  app/view/tables/table_analyte,
)

#' @export
ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        class = "col-lg-2 col-slim",
        inputs_feature_analysis$ui(ns("inputs"))
      ),
      shiny::column(
        width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
        shinydashboard::tabBox(
          id = ns("VolcanoPlotBox"),
          title = "",
          height = "auto",
          width = NULL,
          shiny::tabPanel(
            title = "Volcano Plot",
            shiny::tags$div(
              id = ns("VolcanoContent"),
              plots_volcano$ui(ns("volcano"))
            )
          ),
          shiny::tabPanel(
            title = "Volcano Plot Summary Data",
            #tags$p("holder")
            table_volcano$ui(ns("summary-data"))
          )
        )
      ),
      shiny::column(
        width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
        shinydashboard::tabBox(
          id = ns("AnalytePlotBox"),
          title = "",
          height = "auto",
          width = NULL,
          shiny::tabPanel(
            title = "Analyte Plot",
            value = "AnalytePlot",
            plots_feature_analysis_analyte$ui(ns("analyte"))
          ),
          shiny::tabPanel(
            title = "Analyte Sample Level Data",
            value = "AnalyteTable",
            table_analyte$ui(ns("analyte-data"))
          )
        )
      )
    ),
    shiny::tags$div(
      id = ns("GSEA-Placeholder")
    )
  )
}

#' @export
server <- function(id, app_config, analysis_config, input_config) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    gargoyle::init(
      "get_volcano_data",
      "update_volcano_analytes",
      "show_analyte_plot",
      "sync_analyte_choice",
      "validate_GSEA",
      "run_GSEA",
      "get_GSEA_path_data",
      session = session
    )

    #base inputs
    inputs <- inputs_feature_analysis$server(
      id = "inputs",
      r6 = get_inputs_manager(
        app_config = app_config,
        analysis_config = analysis_config,
        input_config = input_config
      )
    )

    # volcano plot
    analyte <- plots_volcano$server(
      id = "volcano",
      r6 = getFeatureAnalysisSummaryDataManager(
        analysis_config = analysis_config,
        StatTest = inputs$StatTest,
        Covariates = inputs$Covariates,
        AdjustmentMethod = inputs$AdjustmentMethod
      ),
      Study = inputs$Study,
      StudyData = inputs$StudyData,
      parent = session
    )

    table_volcano$server(
      id = "summary-data",
      summary_data = analyte$table_data,
      fold_change_variable = analyte$fold_change_var,
      adjusted = analyte$adjusted,
      stat_test = inputs$StatTest,
      study = inputs$Study,
    )

    # analyte plot
    analyte_data <- plots_feature_analysis_analyte$server(
      id = "analyte",
      r6 = getFeatureAnalysisAnalyteDataManager(
        app_config = app_config,
        analysis_config = analysis_config,
        study = inputs$Study,
        study_data = inputs$StudyData,
        analyte = analyte$analyte,
        summary_data = analyte$SummaryData
      ),
      analyte = analyte$analyte,
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
