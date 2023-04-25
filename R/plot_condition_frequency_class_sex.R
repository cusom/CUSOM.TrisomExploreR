#' @export
condition_frequency_class_sex_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(
      ns("SelectedConditionClassesSexesChart")
    )
  )
}

#' @export
condition_frequency_class_sex_plot_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

   ns <- session$ns

   conditionClassSexData <- eventReactive(c(gargoyle::watch("get_child_conditions")),{
     r6$updateConditionClassSexCounts()
     r6$ConditionClassSexCounts
   }, ignoreInit = TRUE)

   # Condition Class Sexes Chart ####
   output$SelectedConditionClassesSexesChart <- plotly::renderPlotly({

     validate(
       need(!is.null(conditionClassSexData()), "")
     )

     conditionClassSexData() |>
       r6$getGetConditionStackedSexPlot(type = "Classes")

   })

  })

}
