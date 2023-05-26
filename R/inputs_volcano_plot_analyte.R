#' Create analyte inputs for volcano plot
#' @param id namespace for this module instance
#' @importFrom bsplus bs_embed_tooltip
#' @export
volcano_plot_analyte_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
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
      shiny::htmlOutput(ns("AnalyteSearchError"))
    )
  )
}

#' Server side processing / logic for analyte input for volcano plot
#' @param id namespace for this module instance
#' @param r6 r6 class for data management
#' @param parent shiny session - parent session
#' @importFrom gargoyle watch
#' @importFrom gargoyle trigger
#' @import dplyr
#' @importFrom data.table as.data.table
#' @importFrom plotly event_data
#' @import glue
#' @importFrom shinyjs runjs
#' @export
volcano_plot_analyte_input_server <- function(id, r6, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c("Analyte"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    shiny::insertUI(
      session = parent,
      selector = paste0("#", parent$ns("volcanoMultiSelectTextPlaceholder")),
      immediate = TRUE,
      where = "afterEnd",
      ui = shiny::htmlOutput(ns("volcanoMultiSelectText"))
    )

    shiny::observeEvent(
      c(gargoyle::watch("update_volcano_analytes", session = session)), {
      shiny::validate(
        shiny::need(!is.null(r6$VolcanoSummaryData), "")
      )
      analytes <- r6$VolcanoSummaryData |>
        dplyr::select(Analyte) |>
        dplyr::distinct() |>
        dplyr::arrange(Analyte) |>
        data.table::as.data.table()

      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        choices = analytes,
        selected = r6$Analyte
      )
    }, ignoreInit = TRUE, domain = session)

    shiny::observeEvent(
      plotly::event_data(
        "plotly_click",
        priority = "event",
        source = parent$ns("VolcanoPlot"),
        session = parent
      ), {

      e <- plotly::event_data(
        "plotly_click",
        source = parent$ns("VolcanoPlot"),
        session = parent
      )

      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = e$key
      )

    }, domain = session)

    shiny::observeEvent(
      plotly::event_data(
        "plotly_selected",
        priority = "event",
        source = parent$ns("VolcanoPlot"),
        session = parent
      ), {

      e <- plotly::event_data(
        "plotly_selected",
        source = parent$ns("VolcanoPlot"),
        session = parent
      )

      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = e$key
      )
    }, domain = session)

    volcanoMultiSelectText <- shiny::eventReactive(c(input$Analyte), {
      r6$volcanoMultiSelectText
    }, domain = session)

    output$volcanoMultiSelectText <- shiny::renderText({
      shiny::HTML(volcanoMultiSelectText())
    })

    shiny::observeEvent(
      c(gargoyle::watch("sync_analyte_choice", session = session)), {
      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = r6$Analyte
      )
    }, ignoreInit = TRUE,  domain = session)

    shiny::observeEvent(c(input$Analyte), {

      plotName <- parent$ns("VolcanoPlot")

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

    AnalyteSearchErrorText <- shiny::eventReactive(
      c(input$analyteSearchResults, input$Analyte), {
      searchResultData <- input$analyteSearchResults
      shiny::req(searchResultData)

      if (length(input$Analyte) > 0) {
        shiny::HTML("")
      }
      else if(searchResultData$total == 0) {
        shiny::HTML(
          paste0(
            '<span style="color:black;font-size:smaller;padding-left:10px;"><b>"',
            searchResultData$query,
            '"</b> not found. Please try another value</span>'
          )
        )
      }
      else {
        shiny::HTML("")
      }
    }, domain = session)

    output$AnalyteSearchError <- shiny::renderUI({
      AnalyteSearchErrorText()
    })


  })
}
