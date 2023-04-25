#' @export
condition_interactions_ui <- function(id, ...) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12, class = "col-lg-2",
        div(
          id = ns("Dataset-Options"),
          class = "sidebar-text",
          TrisomExploreR::condition_interactions_inputs_ui(ns("inputs"), ...)
        )
      ),
      column(
        width = 12, class = "col-lg-10",
        TrisomExploreR::condition_interactions_plot_ui(ns("plot"))
      )
    )

  )
}

#' @export
condition_interactions_server <- function(id, r6, input_config) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    #r6 <- TrisomExplorerConditionInteractionsR6$new("interactions")

    gargoyle::init("get_interactions_plot", session = session)

    TrisomExploreR::condition_interactions_inputs_server(id = "inputs", r6 = r6, input_config = input_config)

    TrisomExploreR::condition_interactions_plot_server(id = "plot", r6 = r6)

  })

}
