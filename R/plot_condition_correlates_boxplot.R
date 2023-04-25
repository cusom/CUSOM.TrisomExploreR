#' @export
condition_correlates_boxplot_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' @export
condition_correlates_boxplot_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(c(gargoyle::watch("show_analyte_plot", session = session)),{

      showModal(
        modalDialog(
          tabsetPanel(
            tabPanel(
              title = glue::glue("Sina Plot"),
              plotly::plotlyOutput(
                ns("AnalytePlot"),
                height = "600px",
                width = "100%"
              )
            ),
            tabPanel(
              title = glue::glue("Sample Level Data"),
              shinycustomloader::withLoader(
                DT::dataTableOutput(
                  ns("AnalyteDataTable")
                ),
                type = "html",
                loader = "dnaspin"
              )

            )
          ),
          easyClose = FALSE,
          size = "l",
          footer = actionButton(ns("closeAnalyteModal"), label = "Close")
        )
      )


    }, ignoreInit = TRUE)

    observeEvent(input$closeAnalyteModal,{
      removeModal()
    })

    AnalyteData <- eventReactive(c(gargoyle::watch("show_analyte_plot", session = session)),{
      r6$getBoxPlotData()
      r6$BoxplotData
    })

    AnalyteDataTableDownload <- eventReactive(c(gargoyle::watch("show_analyte_plot", session = session)),{
      r6$BoxplotData |>
        r6$getFormattedBoxplotData()
    })

    output$AnalytePlot <- plotly::renderPlotly({
      AnalyteData() |>
        r6$getBoxPlot(ns)
    })

    output$AnalyteDataTable <- DT::renderDataTable({

      DT::datatable(
        data = AnalyteDataTableDownload(),
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          ifelse(length(r6$Analyte)>1,"Selected Analytes Data",glue::glue("{r6$Analyte} Data"))
        ),
        filter = 'top',
        extensions = c('Buttons','ColReorder','Responsive','Scroller'),
        selection = 'none',
        rownames = FALSE,
        style = 'bootstrap',
        escape = FALSE,
        options = list(
          dom = 'Brftip',
          colReorder = TRUE,
          autowidth = FALSE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          buttons = list(
            'colvis',
            list(
              extend = "collection",
              text = "Download Data",
              action = DT::JS(
                paste0(
                  "function ( e, dt, node, config ) {
                   Shiny.setInputValue('", ns("DataDownload"), "', true, {priority: 'event'});
                  }"
                )
              )
            )
          )
        )
      )

    }, server=FALSE)

    observeEvent(c(input$DataDownload), {
      CUSOMShinyHelpers::downloadFile(
        id = ns("download"),
        fileName = glue::glue('{r6$Condition}_Sample_Level_Data_{format(Sys.time(),\"%Y%m%d_%H%M%S\")}'),
        dataForDownload = AnalyteDataTableDownload()
      )
    })

  })

}
