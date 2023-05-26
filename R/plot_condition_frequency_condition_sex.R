#' Create condition sex plot for TrisomExploreR condition frequency analysis
#' @param id - string - id for this module namespace
#' @importFrom plotly plotlyOutput
#' @export
condition_frequency_condition_sex_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(
      ns("SelectedChildConditionSexesChart")
    )
  )
}

#' Server logic for TrisomExploreR condition frequency condition sex plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @importFrom gargoyle watch
#' @importFrom plotly renderPlotly 
#' @export
condition_frequency_condition_sex_plot_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    conditionSexData <- shiny::eventReactive(
      c(gargoyle::watch("get_condition_plot")), {
      r6$updateConditionSexCounts()
      r6$ConditionSexCounts
    }, ignoreInit = TRUE)

    # Child Conditions Sexes Chart ####
    output$SelectedChildConditionSexesChart <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(conditionSexData()), "")
      )
      conditionSexData() |>
        r6$getGetConditionStackedSexPlot(type = "Conditions")
    })

  })

}
