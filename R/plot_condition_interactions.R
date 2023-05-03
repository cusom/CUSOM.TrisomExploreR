#' @export
condition_interactions_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("UpsetPlotContent"),
      shinydashboardPlus::box(
        title = htmlOutput(
          ns("UpsetPlotTitle")
        ),
        height = "auto",
        width = NULL,
        closable = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        shinycustomloader::withLoader(
          plotOutput(
            ns("ConditionUpsetPlot"),
            height = "610px"
          ),
          type = "html",
          loader = "dnaspin"
        )
      )
    )
  )
}

#' @export
condition_interactions_plot_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

   ns <- session$ns

   output$UpsetPlotTitle <- renderText({

     if(length(r6$SelectedConditions)>=2) {

       plotTitle <- '
          <h3>Conditions Upset Plot
            <span data-toggle="tooltip"
            data-placement="auto right"
            title=""
            class="fas fa-info-circle gtooltip info-tooltip"
            data-original-title="
            In this upset plot, each participant appears only once. The Set Size, on the left hand side, represents the total number of
            participants by each Condition Class, or Specific Condition, that is selected. The bar graph displays the breakdown of each
            condition class to show overlap of co-occurring conditions.
            Note that a maximum of six Condition Classes, or Specific Conditions, can be selected at a time.">
            </span>
          </h3>
          '

     } else {

       plotTitle <- '
          <h3>Conditions Upset Plot <i class="fa fa-info-circle info-tooltip" data-toggle="collapse" data-target="#information"></i></h3>
            <div id="information" class="collapse in">
              Please choose at least 2 conditions in the dataset filters to render the plot
          </div>'
     }

     HTML(plotTitle)

   })

   upsetData <- eventReactive(c(gargoyle::watch("get_interactions_plot")),{
     r6$upsetPlotData
   }, ignoreInit = TRUE)


   output$ConditionUpsetPlot <- renderPlot({

     if(is.null(upsetData())) {
       NULL
     }
     else {

      UpSetR::upset(
         upsetData(),
         nintersects = 10,
         nsets = 6,
         number.angles = 2,
         point.size = 10,
         line.size = 2,
         mainbar.y.label = "Number of Participants",
         #show.numbers = TRUE,
         sets.x.label = "Set Size",
         sets.bar.color = "#3E99CD",
         order.by = "freq",
         decreasing = T,
         empty.intersections = NULL,
         # intersection size title, intersection sizetick labels,
         # set size title, set size tick labels, set names, numbers above bars
         text.scale = c(2, 2, 2, 2, 2, 2)
       )

     }

   })

  })


}
