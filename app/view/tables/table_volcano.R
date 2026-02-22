
box::use(
    shiny[NS, tagList, tags, moduleServer, reactive, renderUI, validate, need, observeEvent,
        fluidRow, column, uiOutput, bindEvent, sliderInput],
    shinyWidgets[prettyRadioButtons],
    shinycustomloader[withLoader],
    DT[datatable, dataTableOutput, renderDataTable, JS],
    htmltools[HTML, em],
    glue[glue],
    dplyr[select, filter, mutate, case_when, sym, summarise],
    stringr[str_replace]
)

box::use(
    app/logic/shared/file_utils[download_file]
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
        column(
            width = 12, class = "col-md-5",
            uiOutput(ns("fold_change"))
        ),
        column(
            offset = 1,
            width = 12, class = "col-md-5",
            uiOutput(ns("significance_level"))
        )
        ),
        tags$hr(),
        fluidRow(
            column(
                width = 12,
                withLoader(
                    dataTableOutput(
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

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        output$fold_change <- renderUI({
            validate(
                need(!is.null(summary_data()), "")
            )

            lim <- summary_data() |>
                select(`Fold Change`) |>
                filter(`Fold Change` != Inf) |>
                summarise(m = max(`Fold Change`)) |>
                ceiling() |>
                as.integer()

            sliderInput(
                inputId = ns("fold_change"),
                label = "Fold Change",
                min = -lim,
                max = lim,
                step = round(1 / (lim * 2), 1),
                value = c(-lim, lim)
            )
        })

        output$significance_level <- renderUI({
            validate(
                need(!is.null(summary_data()), "")
            )

            label <- "Filter by p-value significance level"
            choices <- c("all", " * p &le; 0.05", " ** p &le; 0.01", " *** p &le; 0.001")

            if (adjusted()) {
                label <- str_replace(label, "[/p+-]", "q")
                choices <- str_replace(choices, "[/p+/]", "q")
            }

            shinyWidgets::prettyRadioButtons(
                inputId = ns("significance_level"),
                label = label,
                choiceNames = lapply(choices, HTML),
                choiceValues = choices,
                status = "primary"
            )
        })

        table_data <- reactive({
            validate(
                need(!is.null(summary_data()), "")
            )

            sig_col <- ifelse(adjusted(), "q-value", "p.value")

            summary_data() |>
                mutate(
                    p_cut = case_when(
                        input$significance_level == "all" ~ 1,
                        grepl("&le; 0.05", input$significance_level) ~ 0.05,
                        grepl("&le; 0.01", input$significance_level) ~ 0.01,
                        grepl("&le; 0.001", input$significance_level) ~ 0.001
                    )
                ) |>
                filter(
                    `Fold Change` >= min(input$fold_change),
                    `Fold Change` <= max(input$fold_change),
                    !!rlang::sym(sig_col) <= p_cut
                ) |>
                select(-c("p_cut"))

        }) |>
            bindEvent(c(summary_data(), input$significance_level, input$fold_change),
                ignoreInit = TRUE,
                ignoreNULL = TRUE
            )

        output$table <- renderDataTable({
            validate(
                need(!is.null(table_data()), "")
            )

            datatable(
                data = table_data(),
                caption = tags$caption(
                    style = "caption-side: bottom; text-align: center;",
                    "Fold Change Data: ", em("Fold Change Data Used for Volcano Plot")
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
                        action = JS(
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

        observeEvent(c(input$data_download), {
            download_file(
                id = ns("download"),
                file_name = glue('{study()}_Summary_Data_{format(Sys.time(),"%Y%m%d_%H%M%S")}'),
                download_data = table_data()
            )
        })
    })

}
