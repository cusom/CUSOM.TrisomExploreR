box::use(
    shiny[NS, tagList, moduleServer, tags, fluidRow, column, tabPanel, icon],
    shinydashboard[tabBox]
)

box::use(
    app/logic/app_resources/app_configs[TOFAAppManager],
    app/view/inputs/inputs_tofa_analysis,
    app/view/plots/plots_volcano,
    app/view/plots/plots_feature_analysis_analyte
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
                width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
                plots_volcano$ui(ns("volcano"))
            ),
            column(
                width = 12, class = "col-lg-5 col-slim",
                plots_feature_analysis_analyte$ui(ns("timeseries"))
            )
        )
    )
}


#' @export
server <- function(id, app_config) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        # override with TOFA config
        app_config <- TOFAAppManager$new(app_config)

        inputs <- inputs_tofa_analysis$server(
            "inputs",
            app_config = app_config,
            analysis_config = app_config
        )

        # volcano plot
        analyte <- plots_volcano$server(
            id = "volcano",
            analysis_config = app_config,
            app_config = app_config,
            feature = inputs$feature,
            study = inputs$study,
            study_data = inputs$study_data,
            study_plan = inputs$study_plan,
            stat_test = inputs$stat_test,
            covariates = inputs$covariates,
            adjustment_method = inputs$adjustment_method,
            comparison = inputs$comparison,
            parent = session
        )

        analyte_data <- plots_feature_analysis_analyte$server(
            id = "timeseries",
            analysis_config = app_config,
            app_config = app_config,
            analyte = analyte$analyte,
            feature = inputs$feature,
            study = inputs$study,
            study_data = inputs$study_data,
            study_plan = inputs$study_plan,
            summary_data = analyte$summary_data,
            analyte_input_name = analyte$analyte_input_name,
            analyte_session = analyte$analyte_session,
            comparison = inputs$comparison
        )

    })
}