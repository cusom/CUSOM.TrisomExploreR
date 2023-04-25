#' @export
condition_frequency_condition_sex_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(
      ns("SelectedChildConditionSexesChart")
    )
  )
}

#' @export
condition_frequency_condition_sex_plot_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    conditionSexData <- eventReactive(c(gargoyle::watch("get_condition_plot")),{
      r6$updateConditionSexCounts()
      r6$ConditionSexCounts
    }, ignoreInit = TRUE)

    # Child Conditions Sexes Chart ####
    output$SelectedChildConditionSexesChart <- plotly::renderPlotly({

      validate(
        need(!is.null(conditionSexData()),"")
      )

      conditionSexData() |>
        r6$getGetConditionStackedSexPlot(type = "Conditions")

    })

  })

}
