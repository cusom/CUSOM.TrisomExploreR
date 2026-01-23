box::use(
    shiny[tags]
)

box::use(
    app/logic/feature_analysis/InputsCorrelatesManager[CorrelatesAnalysisInputsManager],
    app/view/inputs/inputs_correlates,
    app/logic/feature_analysis/CorrelatesSummaryDataManager[CorrelatesSummaryDataManager],
    app/view/inputs/inputs_volcano_plot_analyte,
    app/view/plots/plots_volcano,
    app/view/plots/plots_feature_analysis_analyte,
)

#' @export
ui <- function(id) {

    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                width = 12,
                class = "col-lg-2 col-slim",
                inputs_correlates$ui(ns("inputs"))
            ),
            shiny::column(
                width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
                shinydashboard::tabBox(
                    id = ns("VolcanoPlotBox"),
                    title = "",
                    height = "auto",
                    width = NULL,
                    shiny::tabPanel(
                        title = "Volcano Plot",
                        shiny::tags$div(
                            id = ns("VolcanoContent"),
                            plots_volcano$ui(ns("volcano-plot"))
                        )
                    ),
                    shiny::tabPanel(
                        title = "Volcano Plot Summary Data",
                        tags$p("holder")
                        #risomExploreR::volcano_data_table_ui(ns("volcano-summary"))
                    )
                )
            ),
            shiny::column(
                width = 12, class = "col-lg-5 col-slim", style = "width:40%;",
                shinydashboard::tabBox(
                    id = ns("AnalytePlotBox"),
                    title = "",
                    height = "auto",
                    width = NULL,
                    shiny::tabPanel(
                        title = "Correlation Plot",
                        value = "Correlation Plot",
                        #plots_feature_analysis_analyte$ui(ns("analyte-plot"))
                    ),
                    shiny::tabPanel(
                        title = "Correlation Sample Level Data",
                        value = "Correlation Sample Level Data",
                        tags$p("holder")
                    )
                )
            )
        ),
        shiny::tags$div(
            id = ns("GSEA-Placeholder")
        )
    )

}
#' @export
server <- function(id, analysis_config, input_config) {

    shiny::moduleServer(id, function(input, output, session) {

        ns <- session$ns

        inputs <- inputs_correlates$server(
            id = "inputs",
            r6 = CorrelatesAnalysisInputsManager$new(
                analysis_config = analysis_config,
                input_config = input_config
            )
        )

        # volcano plot
        feature <- plots_volcano$server(
            id = "volcano-plot",
            r6 = CorrelatesSummaryDataManager$new(
                analysis_config = analysis_config
            ),
            Study = inputs$Study,
            StudyData = inputs$StudyData,
            parent = session
        )

    })

}
