box::use(
  shiny[NS, tagList, tags, moduleServer, reactive, validate, need, bindEvent,
    isolate, selectizeInput, updateSelectizeInput, htmlOutput, insertUI, 
    observeEvent, renderText],
  htmltools[HTML],
  bsplus[bs_embed_tooltip],
  dplyr[select, distinct, arrange, pull],

)

box::use(
  app/logic/shared/server_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$span(
      id = ns("AnalyteInput"),
      selectizeInput(
        inputId = ns("analyte"),
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
        bs_embed_tooltip(
          title = "Select from this dropdown",
          placement = "left",
          html = TRUE
        ),
      htmlOutput(ns("AnalyteSearchError"))
    )
  )
}

#' @export
server <- function(id, r6, summary_data, plot_click_data, plot_selected_data, summary_plot_name, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    insertUI(
      session = parent,
      selector = paste0("#", parent$ns("volcanoMultiSelectTextPlaceholder")),
      immediate = TRUE,
      where = "afterEnd",
      ui = htmlOutput(ns("volcanoMultiSelectText"))
    )

    observeEvent(c(summary_data()), {
      validate(
        need(!is.null(summary_data()), "")
      )
      isolate({
        analytes <- summary_data() |>
          select(Analyte) |>
          distinct() |>
          arrange(Analyte) |>
          pull()

        updateSelectizeInput(
          session = session,
          inputId = "analyte",
          choices = analytes,
          selected = NULL,
          options = list(
            maxOptions = length(analytes)
          ),
          server = TRUE
        )
      })

    }, domain = session)

    observeEvent(
      c(plot_click_data()), {
      plot_click_data() |>
        r6$set_plot_event_data()

      updateSelectizeInput(
        session = session,
        inputId = "analyte",
        selected = plot_click_data()$key
      )

    }, domain = session)

    observeEvent(
      c(plot_selected_data()), {
        plot_selected_data() |>
          r6$set_plot_event_data()

        updateSelectizeInput(
          session = session,
          inputId = "analyte",
          selected = plot_selected_data()$key
        )

    }, domain = session)

    volcano_multi_select_text <- reactive({
        r6$volcanoMultiSelectText
    }) |>
      bindEvent(input$analyte, ignoreInit = TRUE)

    output$volcanoMultiSelectText <- renderText({
      HTML(volcano_multi_select_text())
    })

    observeEvent(c(input$analyte), {
      r6$set_analyte(input$analyte, annotate = TRUE, plot_name = summary_plot_name)
    }, ignoreInit = TRUE, domain = session)

    return(
      list(
        analyte = reactive({input$analyte}),
        analyte_input_name = "analyte",
        analyte_session = session
      )
    )

  })
}
