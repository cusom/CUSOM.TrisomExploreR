box::use(
  shiny[tags]
)


box::use(
  #app/logic/feature_analysis/InputsManager[FeatureAnalysisInputsManager],
  app/logic/inputs/inputs_feature_analysis[FeatureAnalysisInputsManager],
  app/logic/summary_plots/SummaryDataManagerFactory[getFeatureAnalysisSummaryDataManager],
  app/logic/analyte_plots/AnalyteDataManagerFactory[getFeatureAnalysisAnalyteDataManager],
  #app/logic/analyte_plots/AnalyteDataManager[FeatureAnalysisAnalyteDataManager],
  app/view/inputs/inputs_feature_analysis,
  app/view/inputs/inputs_volcano_plot_analyte,
  app/view/plots/plots_volcano,
  #app/logic/table_volcano_datatable[volcano_data_table_ui, volcano_data_table_server],
  app/view/plots/plots_feature_analysis_analyte,
  #app/logic/table_feature_analysis_analyte[feature_analysis_analyte_summary_data_ui,feature_analysis_analyte_summary_data_server]
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
            tags$p("holder")
            #volcano_data_table_ui(ns("volcano-summary"))
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
            tags$p("Test")
            #feature_analysis_analyte_summary_data_ui(ns("analyte-summary-data"))
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
      r6 = FeatureAnalysisInputsManager$new(
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

    # volcano_data_table_server(id = "volcano-summary", r6 = r6)

    # analyte plot
    plots_feature_analysis_analyte$server(
      id = "analyte",
      r6 = getFeatureAnalysisAnalyteDataManager(#FeatureAnalysisAnalyteDataManager$new(
        analysis_config = analysis_config,
        study_data = inputs$StudyData,
        analyte = analyte$analyte,
        summary_data = analyte$SummaryData
      ),
      analyte = analyte$analyte,
      analyte_input_name = analyte$fanalyte_input_name,
      analyte_session = analyte$analyte_session
    )

    # feature_analysis_analyte_summary_data_server(id = "analyte-summary-data", r6 = r6)

  })

}
