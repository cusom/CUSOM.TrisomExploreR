#' Create condition class sex plot for TrisomExploreR condition frequency analysis
#' @param id - string - id for this module namespace
#' @importFrom plotly plotlyOutput
#' @export
condition_frequency_class_sex_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(
      ns("SelectedConditionClassesSexesChart")
    )
  )
}

#' Server logic for TrisomExploreR condition frequency condition class sex plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @importFrom gargoyle watch
#' @importFrom plotly renderPlotly 
#' @export
condition_frequency_class_sex_plot_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

   ns <- session$ns

   conditionClassSexData <- shiny::eventReactive(
    c(gargoyle::watch("get_child_conditions")), {
     r6$updateConditionClassSexCounts()
     r6$ConditionClassSexCounts
   }, ignoreInit = TRUE)

   # Condition Class Sexes Chart ####
   output$SelectedConditionClassesSexesChart <- plotly::renderPlotly({
     shiny::validate(
       shiny::need(!is.null(conditionClassSexData()), "")
     )
     conditionClassSexData() |>
       r6$getGetConditionStackedSexPlot(type = "Classes")
   })

  })

}
