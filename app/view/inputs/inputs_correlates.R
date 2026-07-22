box::use(
    shiny[NS, moduleServer, tags, tagList, bindEvent, actionButton, icon, uiOutput,
        selectizeInput, renderUI, reactive, reactiveVal, updateSelectizeInput, observeEvent,
        validate, need, req],
    shinydashboardPlus[box],
    shinyWidgets[virtualSelectInput, updateVirtualSelect, prepare_choices],
    shinyjs[disable, enable, hidden, click],
    bsplus[bs_embed_tooltip, bs_accordion, bs_set_opts, bs_append],
    shinycustomloader[withLoader],
    shinybusy[show_modal_spinner, remove_modal_spinner],
    glue[glue],
)

box::use(
    app/logic/shared/input_locking_utils,
    app/logic/shared/plot_utils[purge_plot],
    app/view/custom_ui/input_widgets[prettyRadioButtonsFieldSet],
    app/logic/correlates_analysis/inputs/CorrelatesInputs[getCorrelatesAnalysisInputs],
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h3("Inputs"),
        bs_accordion(id = ns("AccordionInputs")) |>
            bs_set_opts(panel_type = "default", use_heading_link = TRUE) |>
            bs_append(
                title = "1) Select Query Dataset",
                content = list(
                    withLoader(
                        uiOutput(ns("QueryExperiment")),
                        type = "html",
                        loader = "loader6",
                        proxy.height = "20px"
                    )
                )
            ) |>
            bs_append(
                title = "2) Choose Comparison Dataset",
                content = list(
                    withLoader(
                        uiOutput(ns("CompareExperiment")),
                        type = "html",
                        loader = "loader6",
                        proxy.height = "20px"
                    )
                )
            ) |>
            bs_append(
                title = "3) Choose Query Analyte",
                content = list(
                    virtualSelectInput(
                        inputId = ns("QueryAnalyte"),
                        label = NULL,
                        choices = NULL,
                        position = "auto",
                        search = TRUE,
                        maxOptions = 1,
                        zIndex = 9999,
                        dropboxWrapper = "body"
                    )
                )
            ),
            tags$div(
                hidden(
                    tags$div(
                        id = "internals",
                        selectizeInput(
                            inputId = ns("stat_test"),
                            label = "",
                            choices = "spearman",
                            selected = "spearman"
                        ),
                        selectizeInput(
                            inputId = ns("covariates"),
                            label = "",
                            choices = c(0),
                            selected = c(0)
                        ),
                        selectizeInput(
                            inputId = ns("adjustment_method"),
                            label = "",
                            choices = "BH",
                            selected = "BH"
                        )
                    )
                )
            ),
            tags$div(
                actionButton(
                    ns("getData"),
                    label = "Analyze & Plot",
                    class = "refresh-btn",
                    icon = icon("play")
                )
            )
        )
}

#' @export
server <- function(id, app_config, analysis_config) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        #expose a reactive that always reads the current instance
        r6 <- reactive({
            getCorrelatesAnalysisInputs(
                app_config = app_config,
                analysis_config = app_config$get_analysis_config("correlates"),
                input_config = app_config$get_input_config("correlates")
            )
        })

        output$QueryExperiment <- renderUI({

            choices <- r6()$getQueryExperiments()

            selected <- ifelse(nrow(choices) == 1, choices, character(0))

            prettyRadioButtonsFieldSet(
                input_id = ns("QueryExperiment"),
                label = NULL,
                field_set_data = choices,
                selected = selected
            ) |>
                bs_embed_tooltip(
                    title = "Select a study below",
                    placement = "top",
                    html = TRUE
                )

        })

        observeEvent(input$QueryExperiment, {
            req(input$QueryExperiment)
            click(glue("AccordionInputs-1-heading"), asis = FALSE)
        }, ignoreInit = FALSE, priority = 999)

        ComparisonExperiments <- reactive({
            req(input$QueryExperiment)
            show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = glue("Getting Comparison Experiments...")
            )
            on.exit(remove_modal_spinner(), add = TRUE)

            r6()$getComparisonExperiments(input$QueryExperiment)

        }) |>
            bindEvent(input$QueryExperiment, ignoreInit = FALSE, ignoreNULL = TRUE)

        output$CompareExperiment <- renderUI({

            if (!is.null(ComparisonExperiments())) {
                tagList(
                    prettyRadioButtonsFieldSet(
                        input_id = ns("CompareExperiment"),
                        label = NULL,
                        field_set_data =  ComparisonExperiments(),
                        selected =  ComparisonExperiments(),
                    ) |>
                        bs_embed_tooltip(
                            title = "Select a study below",
                            placement = "top",
                            html = TRUE
                        )
                )
            } else {
                tagList()
            }

        })

        observeEvent(input$CompareExperiment, {

            click(glue("AccordionInputs-2-heading"), asis = FALSE)

            show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = glue("Getting Query Analytes...")
            )
            on.exit(remove_modal_spinner(), add = TRUE)

            choice_data <- r6()$getQueryAnalytes(
                input$QueryExperiment,
                input$CompareExperiment
            )

            updateVirtualSelect(
                inputId = "QueryAnalyte",
                label = "Query Analyte",
                choices = prepare_choices(
                    choice_data,
                    label = QueryAnalyte,
                    value = QueryAnalyteKey
                ),
                selected = NULL,
                session = session
            )

        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        observeEvent(input$QueryAnalyte, {

            module_namespace <- sub("-$", "", session$ns(""))

            if (input$QueryAnalyte == "") {

                disable(id = "CompareExperiment")

                purge_plot(session, module_namespace, "plot", r6())
                purge_plot(session, module_namespace, "AnalytePlot", r6())

                updateSelectizeInput(
                    session = session,
                    inputId = "ComparisonAnalyte",
                    selected = ""
                )

            } else {

                enable(id = "CompareExperiment")

            }

        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        last_request_signature <- reactiveVal(NULL)
        last_correlation_data <- reactiveVal(NULL)

        input_locking_utils$bind_action_button_state(
            session = session,
            button_id = "getData",
            is_ready_fn = function() {
                length(input$QueryExperiment) == 1
            },
            can_enable_fn = function() {
                length(input$QueryExperiment) == 1 &&
                    !is.null(input$QueryAnalyte) &&
                    input$QueryAnalyte != "" &&
                    !is.null(input$CompareExperiment) &&
                    length(input$CompareExperiment) > 0 &&
                    input$CompareExperiment != ""
            }
        )

        correlation_data <- reactive({
            validate(
                need(input$getData > 0, ""),
                need(input$QueryExperiment != "", ""),
                need(input$QueryAnalyte != "", ""),
                need(input$CompareExperiment != "", "")
            )

            request_signature <- paste(
                input$QueryExperiment,
                input$CompareExperiment,
                input$QueryAnalyte,
                sep = "::"
            )

            if (identical(last_request_signature(), request_signature) &&
                !is.null(last_correlation_data())) {
                return(last_correlation_data())
            }

            show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = "Getting Correlation Data..."
            )
            on.exit(remove_modal_spinner(), add = TRUE)

            data <- r6()$get_correlation_data(
                input$QueryExperiment,
                input$CompareExperiment,
                input$QueryAnalyte
            )

            last_request_signature(request_signature)
            last_correlation_data(data)

            data

        }) |>
            bindEvent(input$getData, ignoreInit = TRUE)

        return(
            list(
                feature = reactive({"correlates"}),
                study = reactive({input$QueryExperiment}),
                study_data = correlation_data,
                stat_test = reactive({input$stat_test}),
                covariates = reactive({input$covariates}),
                adjustment_method = reactive({input$adjustment_method}),
                fold_change_variable = reactive({r6()$fold_change_variable}),
                adjusted = reactive({TRUE})
            )
        )

    })

}
