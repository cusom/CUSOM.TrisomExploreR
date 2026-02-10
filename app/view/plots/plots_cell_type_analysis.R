box::use(
    shiny[NS, moduleServer, tagList, validate, need],
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
            )
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
                r6$CellTypes,
                r6$Analyte,
                r6$StatTest,
                r6$Covariates,
                r6$AdjustmentMethod,
                r6$applicationName,
                ns
            )
        })
    })

}