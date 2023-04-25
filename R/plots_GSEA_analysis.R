#' @export
feature_analysis_GSEA_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      title = "",
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinyjs::hidden(
        selectizeInput(
          inputId = ns("GSEASelectedAnalytes"),
          label = "",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      ),
      plotly::plotlyOutput(
        ns("GSEAPlot"),
        height = "650px",
        width = "99%"
      )
    )
  )
}

#' @export
feature_analysis_GSEA_plot_server <- function(id, r6, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    GSEAData <- eventReactive(c(gargoyle::watch("run_GSEA", session = session)),{
      r6$GSEAData
    })

    output$GSEAPlot <- plotly::renderPlotly({

      validate(
        need(!is.null(GSEAData()), "")
      )

      GSEAData() |>
        r6$getGSEAPlot(ns)

    })

    observeEvent(plotly::event_data("plotly_click", priority = "event", source = ns("GSEAPlot"), session = session),{

      e <- plotly::event_data("plotly_click", priority = "event", source = ns("GSEAPlot"), session = session)

      keys <- tibble::tibble("Gene" = e$customdata) |>
        tidyr::separate_rows(Gene, sep = ",") |>
        dplyr::inner_join(
          r6$VolcanoSummaryData |>
            dplyr::select(Analyte) |>
            dplyr::rowwise() |>
            dplyr::mutate("Gene" = CUSOMShinyHelpers::parseDelimitedString(Analyte, 1)),
          by = "Gene"
        ) |>
        dplyr::select(Analyte) |>
        dplyr::summarise(text = toString(Analyte)) |>
        dplyr::mutate(text = gsub(", ", "|", text)) |>
        dplyr::pull()

      r6$Analyte <- stringr::str_split(keys, "\\|", simplify = TRUE)

      r6$GSEATraceName <- e$y

      updateSelectizeInput(
        session = session,
        inputId = "GSEASelectedAnalytes",
        choices = e$customdata,
        selected = e$customdata
      )

      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = glue::glue("Fetching {r6$GSEATraceName} data...")
      )

      r6$getGSEAPathwayData(r6$GSEATraceName)

      gargoyle::trigger("get_GSEA_path_data", session = session)

      toggle_GSEA_volcano_plot_trace(
        session = session,
        ns = ns,
        plot_name = "VolcanoPlot",
        r6 = r6,
        action = "add"
      )

      shinybusy::remove_modal_spinner()

    }, ignoreNULL = TRUE, ignoreInit = TRUE, domain = session)

  })
}
