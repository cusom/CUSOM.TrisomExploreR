box::use(
    shiny[NS, tagList, moduleServer, tags, fluidRow, column, tabPanel, icon],
    shinydashboard[tabBox]
)

box::use(
    app/logic/app_resources/app_configs[TOFAAppManager],
    app/view/inputs/inputs_tofa_analysis,
    app/view/plots/plots_tofa_timeseries
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 12, class = "col-lg-2 col-slim",
                inputs_tofa_analysis$ui(ns("inputs"))
            ),
            column(
                width = 12, class = "col-lg-10 col-slim",
                plots_tofa_timeseries$ui(ns("timeseries"))
            )
        )
    )
}


#' @export
server <- function(id, app_config) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        analysis_config <- TOFAAppManager$new(app_config)

        inputs <- inputs_tofa_analysis$server(
            "inputs",
            analysis_config = analysis_config
        )

        timeseries <- plots_tofa_timeseries$server(
            "timeseries",
            scores = analysis_config$datasets,
            cohort = inputs$data,
            feature = inputs$feature,
            plot_type = inputs$plot_type
        )

    })
}