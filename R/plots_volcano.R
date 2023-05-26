#' Create Volcano Plot
#' @param id namespace for this module instance
#' @importFrom shinydashboardPlus box
#' @importFrom shinycustomloader withLoader
#' @importFrom plotly renderPlotly
#' @export
volcano_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      title = shiny::tags$div(
        class = "volcano-top-input-panel",
        TrisomExploreR::volcano_plot_analyte_input_ui(ns("volcano-analyte")),
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
      shiny::tags$div(
        id = ns("volcanoMultiSelectTextPlaceholder")
      )
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

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    FoldChangeData <- shiny::eventReactive(
      c(gargoyle::watch("get_volcano_data", session = session)), {

      shiny::validate(
        shiny::need(!is.null(r6$Study), "")
      )

      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = "Calculating data for Volcano Plot..."
      )

      shiny::isolate({

        r6$getVolcanoSummaryData()

        gargoyle::trigger("update_volcano_analytes", session = session)

        gargoyle::trigger("validate_GSEA", session = session)

        r6$VolcanoSummaryData

      })

    }, ignoreInit = TRUE)

    output$VolcanoPlot <- plotly::renderPlotly({

      shiny::validate(
        shiny::need(!is.null(FoldChangeData()), "")
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

    TrisomExploreR::volcano_plot_analyte_input_server(
      id = "volcano-analyte",
      r6 = r6,
      parent = session
    )

    TrisomExploreR::GSEA_analysis_inputs_server(
      id = "GSEA",
      r6 = r6,
      ...
    )

  })
}
