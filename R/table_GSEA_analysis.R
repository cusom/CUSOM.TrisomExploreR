#' Create GSEA pathway table output for TrisomExploreR pathway analysis
#' @param id - string - id for this module namespace
#' @importFrom shinydashboardPlus box
#' @importFrom shinycustomloader withLoader
#' @importFrom DT dataTableOutput
#' @export
feature_analysis_GSEA_summary_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("GSEAPathwayTableBox"),
      shinydashboardPlus::box(
        title = "GSEA Pathway Data",
        height = "auto",
        width = NULL,
        closable = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        shinycustomloader::withLoader(
          DT::dataTableOutput(
            ns("GSEADataTable"),
            width = "99%",
            height = "400px"
          ),
          type = "html",
          loader = "dnaspin"
        )
      )
    )
  )
}

#' Server logic for GSEA pathway table output for TrisomExploreR pathway analysis
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @param parent - shiny session object - parent session
#' @import DT
#' @import glue
#' @importFrom gargoyle watch
#' @importFrom CUSOMShinyHelpers downloadFile
#' @export
feature_analysis_GSEA_summary_data_server <- function(id, r6, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    GSEAPathwayData <- shiny::eventReactive(
      c(gargoyle::watch("get_GSEA_path_data", session = session)), {

      r6$GSEAPathwayData

    }, ignoreInit = TRUE)

    output$GSEADataTable <- DT::renderDataTable({

      if (nrow(GSEAPathwayData()) > 0) {

        DT::datatable(
          data = GSEAPathwayData(),
          caption = htmltools::tags$caption(
            style = "caption-side: top; text-align: left;",
            shiny::HTML(glue::glue('<a href="https://www.gsea-msigdb.org/gsea/msigdb/cards/{r6$GSEAGenesetName}" target="_blank" title="Click here to learn more about this gene set">{r6$GSEAGenesetName} GENE SET</a>')),
            htmltools::em("")
          ),
          extensions = c("Buttons", "Responsive", "Scroller"),
          selection = "none",
          rownames = FALSE,
          style = "bootstrap",
          escape = FALSE,
          options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20),
            dom = "Bfti",
            searchHighlight = TRUE,
            initComplete = DT::JS("function(settings) {
              var table=settings.oInstance.api();
              table.$('td:first-child').each(function(){
                this.setAttribute('title', 'Click here to search genecards.org for '+ this.textContent);
              })
            }"
            ),
            autowidth = FALSE,
            deferRender = TRUE,
            scrollY = 200,
            scroller = TRUE,
            buttons = list(
              "colvis",
              list(
                extend = "collection",
                text = "Download Data",
                action = DT::JS(
                  paste0(
                    "function ( e, dt, node, config ) {
                   Shiny.setInputValue('",ns('DataDownload'),"', true, {priority: 'event'});
                  }"
                  )
                )
              )
            )
          )
        )
      }

    }, server = FALSE)

    shiny::observeEvent(c(input$DataDownload), {
      CUSOMShinyHelpers::downloadFile(
        id = ns("download"),
        fileName = glue::glue('{self$applicationName} - GSEA {r6$GSEAGenesetName} {format(Sys.time(),\"%Y%m%d_%H%M%S\")}'),
        dataForDownload = GSEAPathwayData()
      )
    })

  })
}
