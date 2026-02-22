box::use(
    shiny[NS, tagList, fluidRow, column, tabPanel, moduleServer, tags],
    shinydashboard[tabBox]
)

box::use(
    app/view/inputs/inputs_correlates,
    app/view/plots/plots_volcano,
    app/view/plots/plots_feature_analysis_analyte,
)

#' @export
ui <- function(id) {

    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 12,
                class = "col-lg-2 col-slim",
                inputs_correlates$ui(ns("inputs"))
            ),
            column(
                width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
                tabBox(
                    id = ns("VolcanoPlotBox"),
                    title = "",
                    height = "auto",
                    width = NULL,
                    tabPanel(
                        title = "Volcano Plot",
                        tags$div(
                            id = ns("VolcanoContent"),
                            plots_volcano$ui(ns("volcano"))
                        )
                    ),
                    tabPanel(
                        title = "Volcano Plot Summary Data",
                        tags$p("holder")
                        #risomExploreR::volcano_data_table_ui(ns("volcano-summary"))
                    )
                )
            ),
            column(
                width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
                tabBox(
                    id = ns("AnalytePlotBox"),
                    title = "",
                    height = "auto",
                    width = NULL,
                    tabPanel(
                        title = "Correlation Plot",
                        value = "Correlation Plot",
                        plots_feature_analysis_analyte$ui(ns("analyte"))
                    ),
                    tabPanel(
                        title = "Correlation Sample Level Data",
                        value = "Correlation Sample Level Data",
                        tags$p("holder")
                    )
                )
            )
        ),
        tags$div(
            id = ns("GSEA-Placeholder")
        )
    )

}
#' @export
server <- function(id, app_config, analysis_config, input_config) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        inputs <- inputs_correlates$server(
            id = "inputs",
            app_config = app_config,
            analysis_config = analysis_config
        )

        # volcano plot
        analyte <- plots_volcano$server(
            id = "volcano",
            analysis_config = analysis_config,
            app_config = app_config,
            feature = inputs$feature,
            study = inputs$study,
            study_data = inputs$study_data,
            stat_test = inputs$stat_test,
            covariates = inputs$covariates,
            adjustment_method = inputs$adjustment_method,
            parent = session
        )

        # analyte plot
        plots_feature_analysis_analyte$server(
            id = "analyte",
            analysis_config = analysis_config,
            analyte = analyte$analyte,
            feature = inputs$feature,
            app_config = app_config,
            study = inputs$study,
            study_data = inputs$study_data,
            summary_data = analyte$summary_data,
            analyte_input_name = analyte$analyte_input_name,
            analyte_session = analyte$analyte_session
        )

    })

}
