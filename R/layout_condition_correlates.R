#' Create layout / skeleton / objects / submodules for TrisomExploreR condition correlates explorer
#' @param id - string - id for this module namespace
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom shinydashboard tabBox
#' @export
condition_correlates_ui <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        class = "col-lg-2 col-slim",
        condition_correlates_inputs_ui(ns("correlates-inputs"), ...)
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
              volcano_plot_ui(ns("volcano-plot"))
            )
          ),
          shiny::tabPanel(
            title = "Volcano Plot Summary Data",
            volcano_data_table_ui(ns("volcano-summary"))
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
            title = "Heatmap",
            shiny::tags$div(
              id = ns("AnalyteContent"),
              condition_correlates_heatmap_plot_ui(ns("heatmap-plot"))
            )
          ),
          shiny::tabPanel(
            title = "Heatmap Summary Data",
            feature_analysis_analyte_summary_data_ui(ns("heatmap-summary"))
          )
        )
      )
    ),
    shiny::tags$div(
      id = ns("GSEA-Placeholder")
    )
  )
}

#' Server-side logic / processing for TrisomExploreR condition correlates explorer
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom gargoyle init
#' @export
condition_correlates_server <- function(id, r6, ...) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    gargoyle::init(
      "get_volcano_data",
      "sync_analyte_choice",
      "update_volcano_analytes",
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
