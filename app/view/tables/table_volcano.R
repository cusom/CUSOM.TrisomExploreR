box::use(
    app/logic/shared/table_utils[format_summary_data],
    app/logic/shared/file_utils[download_file]
)

#' @export
ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
        shiny::column(
            width = 12, class = "col-md-5",
            shiny::uiOutput(ns("fold_change"))
        ),
        shiny::column(
            offset = 1,
            width = 12, class = "col-md-5",
            shiny::uiOutput(ns("significance_level"))
        )
        ),
        shiny::tags$hr(),
        shiny::fluidRow(
            shiny::column(
                width = 12,
                shinycustomloader::withLoader(
                    DT::dataTableOutput(
                        ns("table")
                    ),
                    type = "html",
                    loader = "dnaspin"
                )
            )
        )
    )
}

#' @export
server <- function(id, summary_data, fold_change_variable, adjusted, stat_test, study, ...) {

    shiny::moduleServer(id, function(input, output, session) {

        ns <- session$ns

        output$fold_change <- shiny::renderUI({

            lim <- ceiling(max(summary_data()$FoldChange))

            shiny::sliderInput(
                inputId = ns("fold_change"),
                label = "", #shiny::HTML(glue::glue("Filter by {r6$VolcanoSummaryDataXAxisLabel}")),
                min = -lim,
                max = lim,
                step = round(1 / (lim * 2), 1),
                value = c(-lim, lim)
            )
        })

        output$significance_level <- shiny::renderUI({
            label <- "Filter by p-value significance level"
            choices <- c("all", " * p &le; 0.05", " ** p &le; 0.01", " *** p &le; 0.001")

            if (adjusted()) {
                label <- stringr::str_replace(label, "[/p+-]", "q")
                choices <- stringr::str_replace(choices, "[/p+/]", "q")
            }

            shinyWidgets::prettyRadioButtons(
                inputId = ns("significance_level"),
                label = label,
                choiceNames = lapply(choices, shiny::HTML),
                choiceValues = choices,
                status = "primary"
            )
        })

        table_data <- shiny::reactive({
            shiny::validate(
                shiny::need(!is.null(summary_data()), "")
            )
            summary_data() |>
                dplyr::mutate(
                    pvalueCutoff = dplyr::case_when(
                        input$significance_level == "all" ~ 1,
                        grepl("&le; 0.05", input$significance_level) ~ 0.05,
                        grepl("&le; 0.01", input$significance_level) ~ 0.01,
                        grepl("&le; 0.001", input$significance_level) ~ 0.001
                    ),
                    "Statistical Test" = stat_test()
                ) |>
                dplyr::filter(
                    !!rlang::sym(fold_change_variable()) >= min(input$fold_change),
                    !!rlang::sym(fold_change_variable()) <= max(input$fold_change),
                    p.value <= pvalueCutoff
                ) |>
                format_summary_data(adjusted(), cols_to_drop = c("pvalueCutoff", "formattedPValue", "text", "ivs"))
        }) |>
            shiny::bindEvent(c(summary_data(), input$significance_level, input$fold_change),
                ignoreInit = TRUE,
                ignoreNULL = TRUE
            )

        output$table <- DT::renderDataTable({
            shiny::validate(
                shiny::need(!is.null(table_data()), "")
            )

            DT::datatable(
                data = table_data(),
                caption = htmltools::tags$caption(
                    style = "caption-side: bottom; text-align: center;",
                    "Fold Change Data: ", htmltools::em("Fold Change Data Used for Volcano Plot")
                ),
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
            },
            server = FALSE
        )

        shiny::observeEvent(c(input$data_download), {
            download_file(
                id = ns("download"),
                file_name = glue::glue('{study()}_Summary_Data_{format(Sys.time(),\"%Y%m%d_%H%M%S\")}'),
                download_data = table_data()
            )
        })
    })

}
