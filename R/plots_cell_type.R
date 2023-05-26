#' Create celltype boxplots for TrisomExploreR cell type analysis
#' @param id - string - id for this module namespace
#' @export
cell_type_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      id = ns("AnalyteContent"),
      title = "",
      height = "auto",
      width = 12,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      sidebar = shinydashboardPlus::boxSidebar(
        id = ns("sidebarLinks"),
        width = 30,
        icon = shiny::icon("highlighter"),
        shiny::tags$h4("Plot tools:"),
        shiny::tags$hr(),
        CUSOMShinyHelpers::createInputControl(
          controlType = "pickerInput",
          inputId = ns("GroupA"),
          label = "Highlight Record:",
          choices = NULL,
          selected = NULL,
          options = list(
            `actions-box` = TRUE,
            `live-search`=TRUE
          ),
          multiple = TRUE
        ),
        shiny::tags$hr(),
        shiny::actionButton(
          ns("ClearHighlights"),
          "Clear Highlighted Points",
          class = "refresh-btn"
        )
      ),
      shinycustomloader::withLoader(
        plotly::plotlyOutput(
          ns("AnalyteBoxPlot"),
          height = "700px"
        ),
        type = "html",
        loader = "dnaspin"
      )
    )
  )
}

#' Server logic for TrisomExploreR celltype boxplots
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @importFrom gargoyle watch
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyProxy
#' @importFrom plotly plotlyProxyInvoke
#' @importFrom shinyjs runjs
#' @export
cell_type_plot_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    AnalyteData <- shiny::eventReactive(
      c(gargoyle::watch("render_analyte_plot", session = session)), {

      shiny::validate(
        shiny::need(r6$Platform != "", ""),
        shiny::need(r6$Analyte != "", ""),
        shiny::need(r6$CellType != "", "")
      )

      r6$getAnalyteData()

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "GroupA",
        choices = list("D21" = r6$cids, "T21" = r6$tids)
      )

      r6$AnalyteData

    }, ignoreInit = TRUE)

    output$AnalyteBoxPlot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(AnalyteData()), "")
      )
      AnalyteData() |>
        r6$getPlot(ns)
    })

    shiny::observeEvent(c(input$GroupA), {

      data_update <- list(
        "selectedpoints" = NA
      )

      plotName <- ns("AnalyteBoxPlot")

      plotly::plotlyProxy(plotName, session) |>
        plotly::plotlyProxyInvoke("update", data_update)

      js <- paste0('

          var ControlCustomData = document.getElementById("',plotName,'").data[0].key
          var TrisomeCustomData = document.getElementById("',plotName,'").data[1].key
          var vals = [',paste(shQuote(input$GroupA), collapse = ", "),'];

          var Controlindexes = [], i = -1;
          for (j=0;j<vals.length;j++) {
              val = vals[j];
              while ((i = ControlCustomData.indexOf(val, i+1)) != -1){
                  Controlindexes.push(i);
              }
          }

          var Trisomeindexes = [], i = -1;
          for (j=0;j<vals.length;j++) {
              val = vals[j];
              while ((i = TrisomeCustomData.indexOf(val, i+1)) != -1){
                  Trisomeindexes.push(i);
              }
          }

          var indicies = [Controlindexes,Trisomeindexes];
          Plotly.update("CellType-plot-AnalyteBoxPlot", {selectedpoints: indicies});
          Plotly.restyle("CellType-plot-AnalyteBoxPlot", { selected : { marker: { size:12, opacity:1.0, color:"#FFA500" } } }, [0] );
          Plotly.restyle("CellType-plot-AnalyteBoxPlot", { unselected : { marker: { size:6, opacity:0.4, color:"#BBBDC0" } } }, [0] );
          Plotly.restyle("CellType-plot-AnalyteBoxPlot", { selected : {marker: { size:12, opacity:1.0, color:"#ff0000"} } }, [1]);
          Plotly.restyle("CellType-plot-AnalyteBoxPlot", { unselected : {marker: { size:6, opacity:0.4, color:"#287BA5"} } }, [1]);
          ')

      shinyjs::runjs(js)

    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$ClearHighlights), {

      data_update <- list(
        "selectedpoints" = NA
      )

      plotly::plotlyProxy(ns("AnalyteBoxPlot"), session) |>
        plotly::plotlyProxyInvoke("update", data_update)

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "GroupA",
        selected = ""
      )

    }, ignoreInit = TRUE)

  })


}
