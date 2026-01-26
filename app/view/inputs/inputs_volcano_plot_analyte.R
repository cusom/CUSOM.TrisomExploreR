box::use(
  app/logic/shared/server_utils
)

#' Create analyte inputs for volcano plot
#' @param id namespace for this module instance
#' @importFrom bsplus bs_embed_tooltip
#' @export
ui <- function(id) {
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
server <- function(id, r6, VolcanoSummaryData, plot_click_data, plot_selected_data, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    server_utils$bind_events(
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

    shiny::observeEvent(c(VolcanoSummaryData()), {

      shiny::validate(
        shiny::need(!is.null(VolcanoSummaryData()), "")
      )

      shiny::isolate({

        analytes <- VolcanoSummaryData() |>
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

      })

    }, domain = session)

    shiny::observeEvent(
      c(plot_click_data()), {
    
      r6$volcanoEventData <- plot_click_data()

      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = plot_click_data()$key
      )

    }, domain = session)

    shiny::observeEvent(
      c(plot_selected_data()), {
  
      r6$volcanoEventData <- plot_selected_data()

      shiny::updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = plot_selected_data()$key
      )

    }, domain = session)

    volcano_multi_select_text <- shiny::reactive({

      r6$volcanoMultiSelectText

    }) |>
      shiny::bindEvent(input$Analyte, ignoreInit = TRUE)

    output$volcanoMultiSelectText <- shiny::renderText({
      shiny::HTML(volcano_multi_select_text())
    })

    shiny::observeEvent(c(input$Analyte), {

      r6$annotate_volcano_point(
        "VolcanoPlot",
        parent$ns
      )

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


    Analyte <- shiny::eventReactive(c(input$Analyte), {
      input$Analyte
    })

    return(
      list(
        analyte = Analyte,
        analyte_input_name ="Analyte",
        analyte_session = session
      )
    )

  })
}
