#' @export
volcano_data_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # shinydashboardPlus::box(
    #   title = "",
    #   height = "auto",
    #   width = NULL,
    #   closable = FALSE,
    #   solidHeader = FALSE,
    #   collapsible = FALSE,
    #   headerBorder = FALSE,
      fluidRow(
        column(
          width = 12, class = "col-md-5",
          uiOutput(ns("FoldChangeTableFilter"))
        ),
        column(
          offset = 1,
          width = 12, class = "col-md-5",
          uiOutput(ns("SignificanceLevelTableFilter"))
        )
      ),
      tags$hr(),
      fluidRow(
        column(
          width = 12,
          shinycustomloader::withLoader(
            DT::dataTableOutput(
              ns("FoldChangeDataTable")
            ),
            type = "html",
            loader = "dnaspin"
          )
        )
      )
    # )
  )
}

#' @export
volcano_data_table_server <- function(id, r6) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$FoldChangeTableFilter <- renderUI({

      lim <- ceiling(r6$VolcanoSummaryMaxFoldChange)

      shiny::sliderInput(
        inputId = ns("FoldChangeTableFilter"),
        label = HTML(glue::glue("Filter by {r6$VolcanoSummaryDataXAxisLabel}")),
        min = -lim,
        max = lim,
        step = round(1 / (lim * 2), 1),
        value = c(-lim, lim)
      )
    })

    output$SignificanceLevelTableFilter <- renderUI({
      label <- "Filter by p-value significance level"
      choices <- c("all", " * p &le; 0.05", " ** p &le; 0.01", " *** p &le; 0.001")

      if (r6$AdjustmentMethod != "none") {
        label <- stringr::str_replace(label, "[/p+-]", "q")
        choices <- stringr::str_replace(choices, "[/p+/]", "q")
      }

      shinyWidgets::prettyRadioButtons(
        inputId = ns("SignificanceLevelTableFilter"),
        label = label,
        choiceNames = lapply(choices, HTML),
        choiceValues = choices,
        status = "primary"
      )
    })

    FoldChangeDataTableData <- shiny::eventReactive(
      c(gargoyle::watch("get_volcano_data", session = session),
        input$SignificanceLevelTableFilter, input$FoldChangeTableFilter
      ), {

      validate(
        need(!is.null(r6$VolcanoSummaryData), ""),
        need(!is.null(input$SignificanceLevelTableFilter), ""),
        need(!is.null(input$FoldChangeTableFilter), "")
      )

      r6$VolcanoSummaryData |>
        dplyr::mutate(
          pvalueCutoff = dplyr::case_when(
            input$SignificanceLevelTableFilter == "all" ~ 1,
            grepl("&le; 0.05", input$SignificanceLevelTableFilter) ~ 0.05,
            grepl("&le; 0.01", input$SignificanceLevelTableFilter) ~ 0.01,
            grepl("&le; 0.001", input$SignificanceLevelTableFilter) ~ 0.001
          ),
          "Statistical Test" = r6$StatTest
        ) |>
        dplyr::filter(
          !!rlang::sym(r6$FoldChangeVar) >= min(input$FoldChangeTableFilter),
          !!rlang::sym(r6$FoldChangeVar) <= max(input$FoldChangeTableFilter),
          p.value <= pvalueCutoff
        ) |>
        r6$getFormattedVolcanoSummaryData()
    })

    output$FoldChangeDataTable <- DT::renderDataTable(
      {
        validate(
          need(!is.null(FoldChangeDataTableData()), "")
        )

        DT::datatable(
          data = FoldChangeDataTableData(),
          caption = htmltools::tags$caption(
            style = "caption-side: bottom; text-align: center;",
            "Fold Change Data: ", htmltools::em("Fold Change Data Used for Volcano Plot")
          ),
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

    observeEvent(c(input$DataDownload), {
      CUSOMShinyHelpers::downloadFile(
        id = ns("download"),
        fileName = glue::glue('{r6$Study}_Summary_Data_{format(Sys.time(),\"%Y%m%d_%H%M%S\")}'),
        dataForDownload = FoldChangeDataTableData()
      )
    })
  })
}
