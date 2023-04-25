#' @export
feature_analysis_ui <- function(id, ...) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      column(
        width = 12,
        class = "col-lg-2 col-slim",
        TrisomExploreR::feature_analysis_inputs_ui(ns("inputs"), ...)
      ),
      column(
        width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
        shinydashboard::tabBox(
          id = ns("VolcanoPlotBox"),
          title = "",
          height = "auto",
          width = NULL,
          shiny::tabPanel(
            title = "Volcano Plot",
            div(
              id = ns("VolcanoContent"),
              TrisomExploreR::volcano_plot_ui(ns("volcano-plot"))
            )
          ),
          shiny::tabPanel(
            title = "Volcano Plot Summary Data",
            TrisomExploreR::volcano_data_table_ui(ns("volcano-summary"))
          )
        )
      ),
      column(
        width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
        shinydashboard::tabBox(
          id = ns("AnalytePlotBox"),
          title = "",
          height = "auto",
          width = NULL,
          shiny::tabPanel(
            title = "Analyte Plot",
            value = "AnalytePlot",
            TrisomExploreR::feature_analysis_analyte_plot_ui(ns("analyte-plot"))
          ),
          shiny::tabPanel(
            title = "Analyte Sample Level Data",
            value = "AnalyteTable",
            TrisomExploreR::feature_analysis_analyte_summary_data_ui(ns("analyte-summary-data"))
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
feature_analysis_server <- function(id, r6, ...) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    gargoyle::init(
      "get_volcano_data",
      "show_analyte_plot",
      "sync_analyte_choice",
      "validate_GSEA",
      "run_GSEA",
      "get_GSEA_path_data",
      session = session
    )

    TrisomExploreR::feature_analysis_inputs_server(id = "inputs", r6 = r6, ...)

    TrisomExploreR::volcano_plot_server(id = "volcano-plot", r6 = r6, parent = session)

    TrisomExploreR::volcano_data_table_server(id = "volcano-summary", r6 = r6)

    TrisomExploreR::feature_analysis_analyte_plot_server(id = "analyte-plot", r6 = r6)

    TrisomExploreR::feature_analysis_analyte_summary_data_server(id = "analyte-summary-data", r6 = r6)

  })

}
