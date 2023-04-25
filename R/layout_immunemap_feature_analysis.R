#' @export
immunemap_feature_analysis_ui <- function(id, ...) {
  ns <- NS(id)
  shiny::tagList(
    fluidPage(
      fluidRow(
        column(
          width = 12, class = "col-lg-2",
          div(
            id = ns("Dataset-Options"), class="sidebar-text",
            TrisomExploreR::immunemap_feature_analysis_inputs_ui(ns("Inputs"), ...)
          )
        ),
        column(
          width = 12, class = "col-lg-5",
          shinydashboard::tabBox(
            id = ns("VolcanoPlotBox"),
            title = "",
            height = "auto",
            width = NULL,
            shiny::tabPanel(
              title = "Volcano Plot",
              div(
                id = ns("VolcanoContent"),
                TrisomExploreR::volcano_plot_ui(ns("volcano-plot")),
                TrisomExploreR::feature_analysis_GSEA_summary_data_ui(ns("GSEA-summary-data"))
              )
            ),
            shiny::tabPanel(
              title = 'Volcano Plot Summary Data',
              TrisomExploreR::volcano_data_table_ui(ns("volcano-summary"))
            )
          )
        ),
        column(
          width = 12, class = "col-lg-5",
          shinydashboard::tabBox(
            id = ns("AnalytePlotBox"),
            title = "",
            height = "auto",
            width = NULL,
            shiny::tabPanel(
              title = 'Analyte Plot',
              TrisomExploreR::feature_analysis_analyte_plot_ui(ns("Analyte-Plot"))
            ),
            shiny::tabPanel(
              title = 'Analyte Sample Level Data',
              TrisomExploreR::feature_analysis_analyte_summary_data_ui(ns("Analyte-Summary-Data"))
            ),
            shiny::tabPanel(
              title = 'GSEA Plot',
              TrisomExploreR::feature_analysis_GSEA_plot_ui(ns("GSEA"))
            )
          )
        )
      )
    )
  )
}

#' @export
immunemap_feature_analysis_server <- function(id, r6, ...) {

  moduleServer(id, function(input, output, session) {

    gargoyle::init(
      "get_volcano_data", 
      "show_analyte_plot", 
      "sync_analyte_choice", 
      "disable_GSEA", 
      "enable_GSEA", 
      "run_GSEA", 
      "get_GSEA_path_data",  
      session = session
    )

    TrisomExploreR::immunemap_feature_analysis_inputs_server(id = "Inputs", r6 = r6, ...)

    TrisomExploreR::volcano_plot_server(id = "volcano-plot", r6 = r6)

    TrisomExploreR::volcano_data_table_server(id = "volcano-summary", r6 = r6)

    TrisomExploreR::feature_analysis_analyte_plot_server(id = "Analyte-Plot", r6 = r6)

    TrisomExploreR::feature_analysis_analyte_summary_data_server(id = "Analyte-Summary-Data", r6 = r6)

    TrisomExploreR::feature_analysis_GSEA_plot_server(id = "GSEA", r6 = r6)

    TrisomExploreR::feature_analysis_GSEA_summary_data_server(id = "GSEA-summary-data", r6 = r6)

    observeEvent(c(gargoyle::watch("disable_GSEA", session = session)),{
      hideTab(
        session = session,
        inputId = "AnalytePlotBox",
        target = "GSEA Plot"
      )
    }, ignoreInit = FALSE)

    observeEvent(c(gargoyle::watch("enable_GSEA", session = session)),{
      showTab(
        session = session,
        inputId = "AnalytePlotBox",
        target = "GSEA Plot"
      )
    }, ignoreInit = TRUE)

    observeEvent(c(gargoyle::watch("run_GSEA", session = session)),{
      updateTabItems(
        session = session,
        inputId = "AnalytePlotBox",
        selected = "GSEA Plot"
      )
    }, ignoreInit = TRUE)

  })

}
