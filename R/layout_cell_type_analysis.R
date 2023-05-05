#' @export
cell_type_analysis_ui <- function(id, ...) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      column(
        width = 12, class = "col-lg-2 col-slim",
        div(
          id = ns("Dataset-Options"),
          class = "sidebar-text",
          TrisomExploreR::cell_type_inputs_ui(ns("inputs"), ...)
        )
      ),
      column(
        width = 12, class = "col-lg-10 col-slim",
        TrisomExploreR::cell_type_plot_ui(ns("plot"))
      )
    )
  )
}

#' @export
cell_type_analysis_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## Add Watcher
    gargoyle::init("render_analyte_plot", session = session)

    # inputs
    TrisomExploreR::cell_type_inputs_server(id = "inputs", r6 = r6)

    # plot
    TrisomExploreR::cell_type_server(id = "plot", r6 = r6)

  })

}
