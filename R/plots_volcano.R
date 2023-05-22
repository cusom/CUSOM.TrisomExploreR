#' Volcano Plot Module
#' @param id namespace for this module instance
#' @importFrom shinydashboardPlus box
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom shinycustomloader withLoader
#' @importFrom plotly renderPlotly
#' @export
volcano_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shinydashboardPlus::box(
      title = shiny::tags$div(
        class = "volcano-top-input-panel",
        shiny::tags$span(
          id = ns("AnalyteInput"),
          shiny::selectizeInput(
            inputId = ns("Analyte"),
            label = "",
            choices = NULL,
            multiple = TRUE,
            options = list(
              labelField = "name",
              searchField = "name",
              valueField = "name",
              placeholder = "Select analyte below",
              onInitialize = I('function() { this.setValue(""); }'),
              closeAfterSelect = TRUE,
              selectOnTab = TRUE,
              persist = FALSE,
              `live-search` = TRUE,
              dropupAuto = FALSE,
              onType = I(paste0("
                function (str) {
                  if(this.currentResults.total == 0) {
                    Shiny.setInputValue(
                      '", ns("analyteSearchResults"), "',
                      {
                        query: this.currentResults.query,
                        total: this.currentResults.total
                      },
                      { priority: 'event' }
                    );
                  };
                }"))
            )
          ) |>
          bsplus::bs_embed_tooltip(
            title = "Select from this dropdown",
            placement = "left",
            html = TRUE
          ),
          htmlOutput(ns("AnalyteSearchError"))
        ),
        TrisomExploreR::GSEA_analysis_inputs_ui(ns("GSEA"))
      ),
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinycustomloader::withLoader(
        plotly::plotlyOutput(
          ns("VolcanoPlot"),
          height = "600px",
          width = "99%"
        ),
        type = "html",
        loader = "dnaspin"
      ),
      htmlOutput(ns("volcanoMultiSelectText"))
    )
  )
}

#' server side logic / processing for volcano plot ui module
#' @param id namespace for this module instance
#' @param r6 r6 class for data management
#' @param ... dots - additional arguments passed to other submodules
#' @import plotly
#' @import glue
#' @import shinyjs
#' @importFrom gargoyle watch
#' @importFrom gargoyle trigger
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy remove_modal_spinner
#' @export
volcano_plot_server <- function(id, r6, ...) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c("Analyte"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    FoldChangeData <- eventReactive(c(gargoyle::watch("get_volcano_data", session = session)), {

      validate(
        need(!is.null(r6$Study),"")
      )

      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = "Calculating data for Volcano Plot..."
      )

      shiny::isolate({

        r6$getVolcanoSummaryData()

        analytes <- r6$VolcanoSummaryData |>
          dplyr::select(Analyte) |>
          dplyr::distinct() |>
          dplyr::arrange(Analyte) |>
          data.table::as.data.table()

        updateSelectizeInput(
          session = session,
          inputId = "Analyte",
          choices = analytes,
          selected = r6$Analyte
        )

        gargoyle::trigger("validate_GSEA", session = session)

        r6$VolcanoSummaryData

      })

    }, ignoreInit = TRUE)

    output$VolcanoPlot <- plotly::renderPlotly({

      validate(
        need(!is.null(FoldChangeData()), "")
      )

      shiny::isolate({

        shinybusy::show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = "Rendering Volcano Plot..."
        )

        p <- FoldChangeData() |>
          r6$getVolcanoPlot(ns) |>
          plotly::toWebGL()

        shinybusy::remove_modal_spinner()

        p

      })

    })

    observeEvent(plotly::event_data("plotly_click", priority = "event", source = ns("VolcanoPlot"), session = session), {

      e <- plotly::event_data("plotly_click", source = ns("VolcanoPlot"), session = session)

      updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = e$key
      )

    }, domain = session)

    observeEvent(plotly::event_data("plotly_selected", priority = "event", source = ns("VolcanoPlot"), session = session), {

      e <- plotly::event_data("plotly_selected", source = ns("VolcanoPlot"), session = session)

      updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = e$key
      )
    }, domain = session)

    volcanoMultiSelectText <- eventReactive(c(input$Analyte), {
      r6$volcanoMultiSelectText
    }, domain = session)

    output$volcanoMultiSelectText <- renderText({
      volcanoMultiSelectText()
    })

    observeEvent(c(gargoyle::watch("sync_analyte_choice", session = session)), {
      updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = r6$Analyte
      )
    }, ignoreInit = TRUE,  domain = session)

    observeEvent(c(input$Analyte),{

      plotName <- ns("VolcanoPlot")

      if (all(input$Analyte != "")) {
        if (length(input$Analyte) == 1) {
          keys <- glue::glue_collapse(input$Analyte, sep = "|")
          shinyjs::runjs(glue::glue('annotatePointByKey("{plotName}","{keys}",5);'))
        } else {
          keys <- ""
          shinyjs::runjs(glue::glue('annotatePointByKey("{plotName}","{keys}",5);'))
          keys <- glue::glue_collapse(input$Analyte, sep = "|")
          shinyjs::runjs(glue::glue('updateSelectedKeys("{plotName}","{keys}");'))
        }

        shinyjs::runjs(
          paste0("
            Shiny.setInputValue(
              '", ns("analyteSearchResults"), "',
              {
                query: '", input$Analyte, "',
                total: ", length(input$Analyte), "
              },
              { priority: 'event' }
            );"
          )
        )

      } else {
        keys <- ""
        shinyjs::runjs(glue::glue('annotatePointByKey("{plotName}","{keys}",5);'))
      }

      gargoyle::trigger("show_analyte_plot", session = session)

    }, ignoreInit = TRUE, domain = session)

    AnalyteSearchErrorText <- eventReactive(c(input$analyteSearchResults, input$Analyte), {
      searchResultData <- input$analyteSearchResults
      req(searchResultData)

      if (length(input$Analyte) > 0) {
        HTML("")
      }
      else if(searchResultData$total == 0) {
        HTML(paste0('<span style="color:black;font-size:smaller;padding-left:10px;"><b>"', searchResultData$query, '"</b> not found. Please try another value</span>'))
      }
      else {
        HTML("")
      }
    }, domain = session)

    output$AnalyteSearchError <- renderUI({
      AnalyteSearchErrorText()
    })

    TrisomExploreR::GSEA_analysis_inputs_server(id = "GSEA", r6 = r6, ...)

  })
}
