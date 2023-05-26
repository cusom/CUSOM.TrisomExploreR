#' @export
feature_analysis_analyte_summary_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      title = "",
      id = ns("AnalyteDataTablePanelBox"),
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinycustomloader::withLoader(
        DT::dataTableOutput(
          ns("AnalyteDataTable"),
          height = "650px",
          width = "99%"
        ),
        type = "html",
        loader = "dnaspin"
      )
    )
  )
}

#' @export
feature_analysis_analyte_summary_data_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    AnalyteDataTableDownload <- shiny::eventReactive(
      c(gargoyle::watch("show_analyte_plot", session = session)), {

      shiny::validate(
        shiny::need(!is.null(r6$Analyte), ""),
        shiny::need(r6$Analyte != "", ""),
        shiny::need(!is.null(r6$VolcanoSummaryData), "")
      )

      r6$getFormattedAnalyteSummaryData()

    }, ignoreInit = FALSE)


    output$AnalyteDataTable <- DT::renderDataTable({
        shiny::validate(
          shiny::need(r6$Analyte != "", ""),
          shiny::need(!is.null(AnalyteDataTableDownload()), "")
        )

        DT::datatable(
          data = AnalyteDataTableDownload(),
          caption = htmltools::tags$caption(
            style = "caption-side: bottom; text-align: center;",
            ifelse(length(r6$Analyte) > 1, "Selected Analytes Data", glue::glue("{r6$Analyte} Data"))
          ),
          filter = "top",
          extensions = list(
            "Buttons" = NULL,
            "ColReorder" = NULL,
            #"Responsive" = NULL,
            "Scroller" = NULL
          ),
          selection = "none",
          rownames = FALSE,
          style = "bootstrap",
          escape = FALSE,
          options = list(
            dom = "Brftip",
            colReorder = TRUE,
            autowidth = FALSE,
            deferRender = TRUE,
            scrollY = 400,
            scrollX = TRUE,
            scroller = TRUE,
            buttons = list(
              "colvis",
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
      },
      server = FALSE
    )

    shiny::observeEvent(c(input$DataDownload), {
      CUSOMShinyHelpers::downloadFile(
        id = ns("download"),
        fileName = glue::glue('{r6$Analyte}_Sample_Level_Data_{format(Sys.time(),\"%Y%m%d_%H%M%S\")}'),
        dataForDownload = AnalyteDataTableDownload()
      )
    })
  })
}
