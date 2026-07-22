box::use(
  shiny[tags, NS, tagList, fluidRow, column, moduleServer],
  shinydashboardPlus[box],
  htmltools[HTML]
)

#' @export
ui <- function(id) {

  ns <- NS(id)

  tagList(
    tags$h2("TOFA Overview")
  )

}

#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}
