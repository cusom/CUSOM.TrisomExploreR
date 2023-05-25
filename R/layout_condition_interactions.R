#' Create layout / skeleton / objects for TrisomExploreR condition interactions
#' @param id - string - id for this module namespace
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @export
condition_interactions_ui <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12, class = "col-lg-2",
        shiny::div(
          id = ns("Dataset-Options"),
          class = "sidebar-text",
          TrisomExploreR::condition_interactions_inputs_ui(ns("inputs"), ...)
        )
      ),
      shiny::column(
        width = 12, class = "col-lg-10",
        TrisomExploreR::condition_interactions_plot_ui(ns("plot"))
      )
    )

  )
}

#' Server-side logic / processing for TrisomExploreR condition interactions
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom gargoyle init
#' @export
condition_interactions_server <- function(id, r6, ...) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    gargoyle::init("get_interactions_plot", session = session)

    TrisomExploreR::condition_interactions_inputs_server(id = "inputs", r6 = r6, ...)

    TrisomExploreR::condition_interactions_plot_server(id = "plot", r6 = r6)

  })

}
