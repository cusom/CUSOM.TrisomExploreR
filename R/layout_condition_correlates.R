#' @export
condition_correlates_ui <- function(id, ...) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 12, class = "col-lg-2",
          div(
            id = ns("Dataset-Options"), class="sidebar-text",
            condition_correlates_inputs_ui(ns("correlates-inputs"), ...)
          )
        ),
        column(
          width = 12, class = "col-lg-5",
          shinydashboard::tabBox(
            id = ns("VolcanoPlotBox"),
            title = "",
            height = "auto",
            width = NULL,
            tabPanel(
              title = "Volcano Plot",
              div(
                id = ns("VolcanoContent"),
                volcano_plot_ui(ns("volcano-plot"))
              )
            ),
            tabPanel(
              title = 'Volcano Plot Summary Data',
              volcano_data_table_ui(ns("volcano-summary"))

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
            tabPanel(
              title = 'Heatmap',
              div(
                id = ns("AnalyteContent"),
                condition_correlates_heatmap_plot_ui(ns("heatmap-plot"))
              )
            ),
            tabPanel(
              title = "Heatmap Summary Data",
              feature_analysis_analyte_summary_data_ui(ns("heatmap-summary"))
              #heatmap_data_table_ui(ns("heatmap-summary"))
            )
          )
        )
      )
    )

  )
}

#' @export
condition_correlates_server <- function(id, r6, ...) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    gargoyle::init(
      "get_volcano_data",
      "sync_analyte_choice",
      #"show_correlates_boxplot",
      "show_analyte_plot",
      "show_download_modal",
      "validate_GSEA",
      "run_GSEA",
      "get_GSEA_path_data",
      session = session
    )

    condition_correlates_inputs_server(id = "correlates-inputs", r6 = r6, ...)

    volcano_plot_server(id = "volcano-plot", r6 = r6)

    volcano_data_table_server(id = "volcano-summary", r6 = r6)

    feature_analysis_analyte_summary_data_server(id = "heatmap-summary", r6 = r6)

    condition_correlates_heatmap_plot_server(id = "heatmap-plot", r6 = r6)

    condition_correlates_boxplot_server(id = "boxplot", r6 = r6)

  })

}
