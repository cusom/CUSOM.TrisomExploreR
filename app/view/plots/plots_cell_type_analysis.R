box::use(
    shiny[NS, moduleServer, tagList, validate, need, uiOutput, renderUI, tags],
    shinydashboardPlus[box],
    shinycustomloader[withLoader],
    plotly[plotlyOutput, renderPlotly]
)

box::use(
    app/logic/celltype_analysis/CellTypeAnalyteDataManager[get_cell_type_plot]
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            id = ns("AnalyteContent"),
            title = "",
            height = "auto",
            width = NULL,
            closable = FALSE,
            solidHeader = FALSE,
            collapsible = FALSE,
            headerBorder = FALSE,
            withLoader(
                plotlyOutput(
                    ns("plot"),
                    height = "700px"
                ),
                type = "html",
                loader = "dnaspin"
            ),
            uiOutput(ns("stat_warning"))
        )
    )
}

#' @export
server <- function(id, cell_type_data, r6) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        output$plot <- renderPlotly({
            validate(
                need(!is.null(cell_type_data()), "")
            )

            get_cell_type_plot(
                cell_type_data(),
                intersect(r6()$CellTypes, r6()$CellType),
                r6()$GeneLabel,
                r6()$StatTest,
                r6()$Covariates,
                r6()$AdjustmentMethod,
                r6()$applicationName,
                ns
            )
        })

        output$stat_warning <- renderUI({
            data <- cell_type_data()

            validate(
                need(!is.null(data), "")
            )

            show_warning <- identical(r6()$StatTest, "Wilcoxon test") &&
                "p.value.text" %in% names(data) &&
                any(grepl("^Unable to compute", data$p.value.text))

            if (!show_warning) {
                return(NULL)
            }

            tags$div(
                style = "margin: 8px 10px 0 10px; font-size: 12px; color: #8a6d3b;",
                "Note: Wilcoxon test could not be computed for one or more cell type groups due to insufficient variation or sample size."
            )
        })
    })

}