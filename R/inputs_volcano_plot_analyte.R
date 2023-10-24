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

  Analyte <- NULL

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
        dplyr::pull()

      if (!any(r6$Analyte %in% analytes)) {
        r6$Analyte <- ""
        gargoyle::trigger("show_analyte_plot", session = session)
      }

      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        choices = analytes,
        selected = r6$Analyte,
        options = list(
          maxOptions = length(analytes)
        ),
        server = TRUE
      )

    }, domain = session)

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

      r6$volcanoEventData <- e

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

      r6$volcanoEventData <- e

      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = e$key
      )
    }, domain = session)

    volcano_multi_select_text <- shiny::eventReactive(c(input$Analyte), {

      r6$getVolcanoMultiSelectText()

    }, domain = session, ignoreInit = TRUE)

    output$volcanoMultiSelectText <- shiny::renderText({
      shiny::HTML(volcano_multi_select_text())
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

      r6$annotate_volcano_point(
        "VolcanoPlot",
        parent$ns
      )

      gargoyle::trigger("show_analyte_plot", session = session)

    }, ignoreInit = TRUE, domain = session)

    analyte_search_error_text <- shiny::eventReactive(
      c(input$analyteSearchResults, input$Analyte), {
      search_result_data <- input$analyteSearchResults
      shiny::req(search_result_data)

      if (length(input$Analyte) > 0) {
        shiny::HTML("")
      } else if (search_result_data$total == 0) {
        shiny::HTML(
          paste0(
            '<span style="color:black;font-size:smaller;padding-left:10px;"><b>"',
            search_result_data$query,
            '"</b> not found. Please try another value</span>'
          )
        )
      } else {
        shiny::HTML("")
      }
    }, domain = session)

    output$AnalyteSearchError <- shiny::renderUI({
      analyte_search_error_text()
    })


  })
}
