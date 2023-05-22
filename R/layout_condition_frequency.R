#' Create layout / skeleton / objects for TrisomExploreR condition frequency analysis
#' @param id - string - id for this module namespace
#' @export
condition_frequency_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 12, class = "col-lg-6",
          condition_frequency_class_table_ui(ns("class_table"))
        ),
        column(
          width = 12, class = "col-lg-6",
          condition_frequency_condition_table_ui(ns("condition_table"))
        )
      ),
      tags$hr(style="margin-top:5px;margin-bottom:10px;"),
      fluidRow(
        column(
          width = 12, class = "col-lg-6",
          condition_frequency_class_sex_plot_ui(ns("class_sex_plot"))
        ),
        column(
          width = 12, class = "col-lg-6",
          condition_frequency_condition_sex_plot_ui(ns("condition_sex_plot"))
        )
      )
    )
  )

}

#' Server-side logic / processing for TrisomExploreR condition frequency analysis
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @importFrom gargoyle init
#' @importFrom glue glue
#' @export
condition_frequency_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session

    gargoyle::init("get_child_conditions","get_child_conditions_summary","get_condition_plot", session = session)

    submodules <- list("class_table","condition_table","class_sex_plot","condition_sex_plot")

    sapply(submodules, function(submodule) {
      module_name <- glue::glue("condition_frequency_{submodule}_server")
      do.call(module_name, list(submodule, r6))
    })


  })

}
