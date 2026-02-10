box::use(
    app/logic/shared/file_utils[download_file]
)

box::use(
    shiny[NS, moduleServer, tagList, validate, need, observeEvent],
    shinycustomloader[withLoader],
    DT[dataTableOutput, renderDataTable, datatable],
    glue[glue]
)


#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinydashboardPlus::box(
            title = "",
            id = ns("AnalyteDataTablePanelBox"),
            height = "auto",
            width = NULL,
            closable = FALSE,
            solidHeader = FALSE,
            collapsible = FALSE,
            headerBorder = FALSE,
            withLoader(
                dataTableOutput(
                    ns("table"),
                    height = "650px",
                    width = "99%"
                ),
                type = "html",
                loader = "dnaspin"
            )
        )
    )
}

#' @export
server <- function(id, analyte, table_data) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        output$table <- renderDataTable({
            validate(
                need(!is.null(table_data()), "")
            )

            datatable(
                data = table_data(),
                caption = htmltools::tags$caption(
                    style = "caption-side: bottom; text-align: center;",
                    ifelse(
                        length(analyte()) > 1,
                        "Selected Analytes Data",
                        glue("{analyte()} Data")
                    )
                ),
                filter = "top",
                extensions = list(
                    "Buttons" = NULL,
                    "ColReorder" = NULL,
                    "Scroller" = NULL
                ),
                selection = "none",
                rownames = FALSE,
                style = "bootstrap",
                escape = FALSE,
                options = list(
                    dom = "Brftip",
                    colReorder = TRUE,
                    autowidth = FALSE,
                    deferRender = TRUE,
                    scrollY = 400,
                    scrollX = TRUE,
                    scroller = TRUE,
                    buttons = list(
                        "colvis",
                        list(
                            extend = "collection",
                            text = "Download Data",
                            action = DT::JS(
                                paste0(
                                    "function ( e, dt, node, config ) {
                                        Shiny.setInputValue('", ns("data_download"), "', true, {priority: 'event'});
                                    }"
                                )
                            )
                        )
                    )
                )
            )
        }, server = FALSE)

        observeEvent(c(input$data_download), {
            download_file(
                id = ns("download"),
                file_name = glue('{analyte()}_Sample_Level_Data_{format(Sys.time(),\"%Y%m%d_%H%M%S\")}'),
                download_data = table_data()
            )
        })

    })

}