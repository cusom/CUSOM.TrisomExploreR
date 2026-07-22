
box::use(
    shiny[NS, tagList, tags, moduleServer, reactive, renderUI, validate, need, req, observe, observeEvent,
        fluidRow, column, uiOutput, bindEvent, sliderInput, actionButton, icon],
    shinyWidgets[prettyRadioButtons],
    shinycustomloader[withLoader],
    DT[datatable, dataTableOutput, renderDataTable, JS, formatSignif],
    htmltools[HTML, em],
    glue[glue],
    stringr[str_replace],
    bsplus[bs_attach_modal, bs_modal],
    shinyjs[disabled, toggleState],
)

box::use(
    app/logic/shared/file_utils[download_file],
    app/logic/shared/table_volcano_utils[get_fold_change_column_label, 
    get_fold_change_slider_settings, prepare_volcano_table_data]
)

#' @export
ui <- function(
    id,
    button_label = "Summary Data",
    button_icon = "database",
    button_class = "",
    tooltip_text = "",
    ...
    ) {
    ns <- NS(id)
    tagList(
        bs_modal(
            id = ns("volcano-data-modal"),
            title = tags$h3("Volcano Plot Data"),
            size = "large",
            body = list(
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
            )
        ),
        disabled(
            actionButton(
                ns("data"),
                label = button_label,
                class = button_class,
                icon = icon(button_icon)
            )
        ) |>
            bs_attach_modal(id_modal = ns("volcano-data-modal"))
    )
}

#' @export
server <- function(id, summary_data, adjusted, stat_test, study, ...) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        observe({
            toggleState(id = "data", condition = !is.null(summary_data()))
        })

        fold_change_variable <- reactive({
            req(summary_data())
            get_fold_change_column_label(summary_data())
        })

        output$fold_change <- renderUI({
            validate(
                need(!is.null(summary_data()), "")
            )

            slider_settings <- get_fold_change_slider_settings(
                summary_df = summary_data(),
                fold_change_label = fold_change_variable()
            )

            validate(
                need(is.null(slider_settings$error), slider_settings$error)
            )

            sliderInput(
                inputId = ns("fold_change"),
                label = glue("Filter by {fold_change_variable()}"),
                min = slider_settings$min,
                max = slider_settings$max,
                step = slider_settings$step,
                value = slider_settings$value
            )
        })

        output$significance_level <- renderUI({
            validate(
                need(!is.null(summary_data()), "")
            )

            label <- glue("Filter by {ifelse(adjusted(), 'q-value', 'p-value')} significance level")
            choices <- c("all", " * p &le; 0.05", " ** p &le; 0.01", " *** p &le; 0.001")

            if (adjusted()) {
                choices <- str_replace(choices, "[/p+/]", "q")
            }

            prettyRadioButtons(
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

            table_result <- prepare_volcano_table_data(
                summary_df = summary_data(),
                fold_change_label = fold_change_variable(),
                significance_level = input$significance_level,
                fold_change_range = input$fold_change,
                adjusted = adjusted()
            )

            validate(
                need(is.null(table_result$error), table_result$error)
            )

            table_result$data

        }) |>
            bindEvent(c(summary_data(), input$significance_level, input$fold_change),
                ignoreInit = TRUE,
                ignoreNULL = TRUE
            )

        output$table <- renderDataTable({
            validate(
                need(!is.null(table_data()), "")
            )

            table_df <- table_data()
            numeric_cols <- names(table_df)[vapply(table_df, is.numeric, logical(1))]

            datatable(
                data = table_df,
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
            ) |>
                formatSignif(columns = numeric_cols, digits = 4)

        }, server = FALSE)

        observeEvent(c(input$data_download), {
            download_file(
                id = ns("download"),
                file_name = glue('{study()}_Summary_Data_{format(Sys.time(),"%Y%m%d_%H%M%S")}'),
                download_data = table_data()
            )
        })
    })

}
