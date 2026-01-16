box::use(
  app/view/inputs/inputs_volcano_plot_analyte,
  app/view/inputs/inputs_GSEA_analysis
)

#' Create Volcano Plot
#' @param id namespace for this module instance
#' @importFrom shinydashboardPlus box
#' @importFrom shinycustomloader withLoader
#' @importFrom plotly renderPlotly
#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      title = shiny::tags$div(
        class = "volcano-top-input-panel",
        inputs_volcano_plot_analyte$ui(ns("volcano-analyte")),
        inputs_GSEA_analysis$ui(ns("gsea"))
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
server <- function(id, r6, Study, StudyData, ...) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    VolcanoSummaryData <- shiny::reactive({
      shiny::validate(
        shiny::need(!is.null(StudyData()), "")
      )

      shinybusy::show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = "Calculating Statistics for Volcano Plot..."
        )

      SummaryData <- StudyData() |>
        r6$setStudyData() |>
        r6$getVolcanoSummaryData()

      shinybusy::remove_modal_spinner()

      return(SummaryData)

    })

    output$VolcanoPlot <- plotly::renderPlotly({

      shiny::validate(
        shiny::need(!is.null(VolcanoSummaryData()), "")
      )

      shiny::isolate({

        shinybusy::show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = "Rendering Volcano Plot..."
        )

        p <- VolcanoSummaryData() |>
          r6$getVolcanoPlot(ns) |>
          plotly::toWebGL()

        shinybusy::remove_modal_spinner()

        p

      })

    })

    feature <- inputs_volcano_plot_analyte$server(
      id = "volcano-analyte",
      r6 = r6,
      VolcanoSummaryData = VolcanoSummaryData,
      parent = session
    )

    inputs_GSEA_analysis$server(
      id = "gsea",
      VolcanoSummaryData = VolcanoSummaryData,
      Study = Study,
      ...
    )

    return(
      list(
        SummaryData = VolcanoSummaryData,
        Feature = feature$feature,
        feature_input_name = feature$feature_input_name,
        feature_session = feature$feature_session
      )
    )

  })
}
