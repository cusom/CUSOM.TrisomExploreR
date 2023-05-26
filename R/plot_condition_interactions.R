#' Create upset plot for TrisomExploreR condition interactions analysis
#' @param id - string - id for this module namespace
#' @importFrom plotly plotlyOutput
#' @export
condition_interactions_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("UpsetPlotContent"),
      shinydashboardPlus::box(
        title = shiny::htmlOutput(
          ns("UpsetPlotTitle")
        ),
        height = "auto",
        width = NULL,
        closable = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        shinycustomloader::withLoader(
          shiny::plotOutput(
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

#' Server logic for TrisomExploreR condition interactions upset plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @importFrom gargoyle watch
#' @importFrom UpSetR upset
#' @export
condition_interactions_plot_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

   ns <- session$ns

   output$UpsetPlotTitle <- shiny::renderText({

     if (length(r6$SelectedConditions) >= 2) {
       plotTitle <- "
       <h3>Conditions Upset Plot
        <span 
        data-toggle=\"tooltip\"
        data-placement=\"auto right\"
        title=\"\"
        class=\"fas fa-info-circle gtooltip info-tooltip\"
        data-original-title=\"
        In this upset plot, each participant appears only once. 
        The Set Size, on the left hand side, represents the total number of
        participants by each Condition Class, or Specific Condition, that is selected. 
        The bar graph displays the breakdown of each
        condition class to show overlap of co-occurring conditions.
        Note that a maximum of six Condition Classes, or Specific Conditions, can be selected at a time.\">
        </span>
      </h3>"
     } 
     else {

       plotTitle <- "
        <h3>Conditions Upset Plot 
          <i class=\"fa fa-info-circle info-tooltip\" 
            data-toggle=\"collapse\" 
            data-target=\"#information\">
          </i>
        </h3>
          <div id=\"information\" class=\"collapse in\">
            Please choose at least 2 conditions in the dataset filters to render the plot
        </div>"
     }

     shiny::HTML(plotTitle)

   })

   upsetData <- shiny::eventReactive(
    c(gargoyle::watch("get_interactions_plot")), {
     r6$upsetPlotData
   }, ignoreInit = TRUE)

   output$ConditionUpsetPlot <- shiny::renderPlot({

     if (is.null(upsetData())) {
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
         decreasing = TRUE,
         empty.intersections = NULL,
         # intersection size title, intersection sizetick labels,
         # set size title, set size tick labels, set names, numbers above bars
         text.scale = c(2, 2, 2, 2, 2, 2)
       )

     }

   })

  })


}
