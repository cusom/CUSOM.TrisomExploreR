#' Create standard layout / skeleton / sub-modules for TrisomExploreR cell type analysis 
#' @param id - string - id for this module namespace
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @return ui module
#' @export
cell_type_analysis_ui <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12, class = "col-lg-2 col-slim",
        shiny::tags$div(
          id = ns("Dataset-Options"),
          class = "sidebar-text",
          TrisomExploreR::cell_type_inputs_ui(ns("inputs"), ...)
        )
      ),
      shiny::column(
        width = 12, class = "col-lg-10 col-slim",
        TrisomExploreR::cell_type_plot_ui(ns("plot"))
      )
    )
  )
}

#' Server-side logic / processing for TrisomExploreR cell type analysis 
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @importFrom gargoyle init
#' @export
cell_type_analysis_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## Add Watcher
    gargoyle::init("render_analyte_plot", session = session)

    # inputs
    TrisomExploreR::cell_type_inputs_server(id = "inputs", r6 = r6)

    # plot
    TrisomExploreR::cell_type_plot_server(id = "plot", r6 = r6)

  })

}
