#' Create GSEA plot for TrisomExploreR GSEA pathway analysis
#' @param id - string - id for this module namespace
#' @importFrom shinydashboardPlus box
#' @importFrom shinyjs hidden
#' @importFrom plotly plotlyOutput
#' @export
feature_analysis_GSEA_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      title = "",
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinyjs::hidden(
        shiny::selectizeInput(
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

#' Server logic / processing for TrisomExploreR GSEA plot
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @param parent shiny session - parent shiny session
#' @importFrom gargoyle watch
#' @importFrom gargoyle trigger
#' @importFrom plotly renderPlotly
#' @importFrom plotly event_data
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom CUSOMShinyHelpers parseDelimitedString
#' @importFrom stringr str_split
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy remove_modal_spinner
#' @export
feature_analysis_GSEA_plot_server <- function(id, r6, parent) {

  Gene <- Analyte <- text <- NULL

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    GSEAData <- shiny::eventReactive(
      c(gargoyle::watch("run_GSEA", session = session)), {
      r6$GSEAData
    })

    output$GSEAPlot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(GSEAData()), "")
      )
      GSEAData() |>
        r6$getGSEAPlot(ns)
    })

    shiny::observeEvent(
      plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("GSEAPlot"),
        session = session
        ), {

      e <- plotly::event_data(
        "plotly_click",
        priority = "event",
        source = ns("GSEAPlot"),
        session = session
      )

      keys <- tibble::tibble("Gene" = e$customdata) |>
        tidyr::separate_rows(Gene, sep = ",") |>
        dplyr::inner_join(
          r6$VolcanoSummaryData |>
            dplyr::select(Analyte) |>
            dplyr::rowwise() |>
            dplyr::mutate(
              "Gene" = CUSOMShinyHelpers::parseDelimitedString(Analyte, 1)
            ),
          by = "Gene"
        ) |>
        dplyr::select(Analyte) |>
        dplyr::summarise(text = toString(Analyte)) |>
        dplyr::mutate(text = gsub(", ", "|", text)) |>
        dplyr::pull()

      r6$Analyte <- stringr::str_split(keys, "\\|", simplify = TRUE)

      r6$GSEATraceName <- e$y

      shiny::updateSelectizeInput(
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
