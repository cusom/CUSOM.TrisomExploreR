box::use(
    shiny[NS, moduleServer, tagList, tags, fluidRow, column],
    shinydashboardPlus[box],
    shinyjs[hidden],
    shinyWidgets[prettyRadioButtons, awesomeCheckboxGroup, numericRangeInput],
    shinycustomloader[withLoader]
)

box::use(
    app/logic/inputs/inputs_cell_type_analysis[CellTypesInputsManager],
    app/view/inputs/inputs_cell_type_analysis,
    app/view/plots/plots_cell_type_analysis
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 12, class = "col-lg-2 col-slim",
                tags$div(
                    id = ns("Dataset-Options"),
                    class = "sidebar-text",
                    inputs_cell_type_analysis$ui(ns("inputs"))
                )
            ),
            column(
                width = 12, class = "col-lg-10 col-slim",
                plots_cell_type_analysis$ui(ns("plot"))
            )
        )
    )
}

#' @export
server <- function(id, app_config, analysis_config, input_config) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        r6 <- CellTypesInputsManager$new(
                app_config = app_config,
                analysis_config = analysis_config,
                input_config = input_config
            )

        inputs <- inputs_cell_type_analysis$server(
            "inputs",
            r6 = r6
        )

        plots_cell_type_analysis$server(
            "plot",
            cell_type_data = inputs$cell_type_data,
            r6 = r6
        )

    })

}