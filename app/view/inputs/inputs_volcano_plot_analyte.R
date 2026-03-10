box::use(
  shiny[NS, tagList, tags, moduleServer, reactive, validate, need, bindEvent,
    isolate, selectizeInput, updateSelectizeInput, htmlOutput, insertUI,
    observe, observeEvent, renderText, actionButton, icon],
  htmltools[HTML],
  bsplus[bs_modal, bs_modal_closebutton, bs_attach_modal],
  shinyjs[disabled, toggleState, runjs],
  dplyr[select, distinct, arrange, pull],

)

box::use(
  app/logic/shared/server_utils,
  app/logic/shared/plot_utils[annotate_volcano_from_events, get_volcano_multi_select_text]
)

#' @export
ui <- function(
  id,
  button_label = "Select Analyte",
  button_icon = "vial",
  button_class = "",
  ...
) {
  ns <- NS(id)
  tagList(
    bs_modal(
      id = ns("analyte-picker-modal"),
      title = tags$h4("Select Analyte"),
      size = "large",
      body = list(
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
          ),
          htmlOutput(ns("AnalyteSearchError"))
        )
      ),
      footer = bs_modal_closebutton(label = "Close")
    ),
    disabled(
      actionButton(
        inputId = ns("open"),
        label = button_label,
        class = button_class,
        icon = icon(button_icon)
      )
    ) |>
      bs_attach_modal(id_modal = ns("analyte-picker-modal"))
  )
}

#' @export
server <- function(id, r6, summary_data, plot_click_data, plot_selected_data, summary_plot_name, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observe({
      toggleState(id = "open", condition = !is.null(summary_data()))
    })

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
      updateSelectizeInput(
        session = session,
        inputId = "analyte",
        selected = plot_click_data()$key
      )

    }, domain = session)

    observeEvent(
      c(plot_selected_data()), {
        updateSelectizeInput(
          session = session,
          inputId = "analyte",
          selected = plot_selected_data()$key
        )

    }, domain = session)

    observeEvent(input$analyte, {
      if (!is.null(input$analyte) && length(input$analyte) > 0) {
        runjs(paste0("$('#", ns("analyte-picker-modal"), "').modal('hide');"))
      }
    }, ignoreInit = TRUE)

    volcano_multi_select_text <- reactive({
      get_volcano_multi_select_text(
        plot_data = summary_data(),
        analyte = input$analyte
      )
    }) |>
      bindEvent(input$analyte, ignoreInit = TRUE)

    output$volcanoMultiSelectText <- renderText({
      HTML(volcano_multi_select_text())
    })

    observeEvent(c(input$analyte, plot_click_data(), plot_selected_data()), {
      annotate_volcano_from_events(
        plot_name = summary_plot_name,
        analyte = input$analyte,
        plot_click_data = plot_click_data(),
        plot_selected_data = plot_selected_data(),
        marker_size = 5
      )
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
