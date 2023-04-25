#' #' @export
#' heatmap_data_table_ui <- function(id) {
#'   ns <- shiny::NS(id)
#'   shiny::tagList(
#'     shinydashboard::box(
#'       title = "",
#'       height = "auto",
#'       width = NULL,
#'       closable = FALSE,
#'       solidHeader = FALSE,
#'       collapsible = FALSE,
#'       headerBorder = FALSE,
#'       fluidRow(
#'         column(
#'           width = 12,
#'           shinycustomloader::withLoader(
#'             DT::dataTableOutput(
#'               ns("HeatmapDataTable")
#'             ),
#'             type = "html",
#'             loader = "dnaspin"
#'           )
#'         )
#'       )
#'     )
#'   )
#' }
#'
#' #' @export
#' heatmap_data_table_server <- function(id, r6) {
#'   shiny::moduleServer(id, function(input, output, session) {
#'
#'     ns <- session$ns
#'
#'     HeatmapDataTableData <- shiny::eventReactive(c(gargoyle::watch("get_volcano_data", session = session)), {
#'
#'       r6$VolcanoSummaryData |>
#'         r6$getFormattedHeatmapData()
#'
#'     })
#'
#'     output$HeatmapDataTable <- DT::renderDataTable(
#'       {
#'         validate(
#'           need(!is.null(HeatmapDataTableData()), "")
#'         )
#'
#'         DT::datatable(
#'           data = HeatmapDataTableData(),
#'           caption = htmltools::tags$caption(
#'             style = "caption-side: bottom; text-align: center;",
#'             "Heatmap Data: ",
#'           ),
#'           extensions = list(
#'             "Buttons" = NULL,
#'             "ColReorder" = NULL,
#'             "Responsive" = NULL,
#'             "Scroller" = NULL
#'           ),
#'           selection = "none",
#'           rownames = FALSE,
#'           style = "bootstrap",
#'           escape = FALSE,
#'           options = list(
#'             dom = "Brftip",
#'             colReorder = TRUE,
#'             autowidth = FALSE,
#'             deferRender = TRUE,
#'             scrollY = 400,
#'             scroller = TRUE,
#'             buttons = list(
#'               "colvis",
#'               list(
#'                 extend = "collection",
#'                 text = "Download Data",
#'                 action = DT::JS(
#'                   paste0(
#'                     "function ( e, dt, node, config ) {
#'                      Shiny.setInputValue('", ns("DataDownload"), "', true, {priority: 'event'});
#'                     }"
#'                   )
#'                 )
#'               )
#'             )
#'           )
#'         )
#'       },
#'       server = FALSE
#'     )
#'
#'     observeEvent(c(input$DataDownload), {
#'       CUSOMShinyHelpers::downloadFile(
#'         id = ns("download"),
#'         fileName = glue::glue('{r6$Study}_Summary_Data_{format(Sys.time(),\"%Y%m%d_%H%M%S\")}'),
#'         dataForDownload = HeatmapDataTableData()
#'       )
#'     })
#'   })
#' }
