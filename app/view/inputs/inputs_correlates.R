box::use(
    shiny[NS, moduleServer, tags, tagList, bindEvent, actionButton, icon, uiOutput,
        selectizeInput, renderUI, reactive, updateSelectizeInput, observeEvent,
        validate, need, showNotification],
    shinydashboardPlus[box],
    htmltools[HTML],
    shinyWidgets[virtualSelectInput, updateVirtualSelect, prepare_choices],
    shinyjs[disabled, disable, enable, removeClass, addClass, hidden],
    bsplus[bs_embed_tooltip],
    shinycustomloader[withLoader],
    shinybusy[show_modal_spinner, remove_modal_spinner],
    glue[glue],
)

box::use(
    app/logic/shared/input_locking_utils,
    app/logic/shared/server_utils,
    app/view/custom_ui/input_widgets[prettyRadioButtonsFieldSet],
    app/logic/correlates_analysis/inputs/CorrelatesInputs[getCorrelatesAnalysisInputs],
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = HTML(
                "<div class=\"dataset-options-title\">Dataset Options
                    <span
                        data-toggle=\"tooltip\"
                        data-placement=\"auto right\"
                        title = \"\"
                        class = \"fas fa-filter\"
                        data-original-title=\"Set options below to generate volcano plot\">
                    </span>
                </div>"
            ),
            height = "auto",
            width = NULL,
            closable = FALSE,
            solidHeader = FALSE,
            collapsible = FALSE,
            headerBorder = FALSE,
            disabled(
                actionButton(
                    ns("PrimaryTutorial"),
                    label = "Take Tutorial",
                    class = "tutorial-btn",
                    icon = icon("question-circle")
                ) |>
                bs_embed_tooltip(
                    title = "Click here to learn about setting dataset options
                        to generate the volcano plot",
                    placement = "top",
                    html = TRUE
                )
            ),
            tags$div(
                id = ns("scrollableOptions"),
                style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                tags$b("1) Select Query Dataset"),
                tags$div(
                    id = ns("QueryStudies"),
                    withLoader(
                        uiOutput(ns("QueryExperiment")),
                        type = "html",
                        loader = "loader6",
                        proxy.height = "20px"
                    )
                ),
                tags$hr(),
                tags$div(
                    id = ns("CompareExperiments"),
                    withLoader(
                        uiOutput(ns("CompareExperiment")),
                        type = "html",
                        loader = "loader6",
                        proxy.height = "20px"
                    )
                ),
                tags$hr(),
                tags$b("3) Select Query Analyte"),
                tags$div(
                    id = ns("QueryAnalyteInput"),
                    virtualSelectInput(
                        inputId = ns("QueryAnalyte"),
                        label = NULL,
                        choices = NULL,
                        position = "auto",
                        search = TRUE,
                        maxOptions = 1
                    )
                ),
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
            footer = tagList(
                actionButton(
                    ns("getData"),
                    label = "Analyze & Plot",
                    class = "refresh-btn",
                    icon = icon("play")
                )
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

        ComparisonExperiments <- reactive({

            show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = glue("Getting Comparison Experiments...")
            )
            on.exit(remove_modal_spinner(), add = TRUE)

            r6()$getComparisonExperiments(input$QueryExperiment)

        }) |>
            bindEvent(c(input$QueryExperiment), ignoreInit = TRUE, ignoreNULL = TRUE)

        output$CompareExperiment <- renderUI({

            if (!is.null(ComparisonExperiments())) {
                tagList(
                    tags$b("2) Select Comparison Dataset"),
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

        observeEvent(c(input$CompareExperiment), {

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
                label = "",
                choices = prepare_choices(
                    choice_data,
                    label = QueryAnalyte,
                    value = QueryAnalyteKey
                ),
                selected = NULL,
                session = session
            )

        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        observeEvent(c(input$QueryAnalyte), {

            if (input$QueryAnalyte == "") {

                disable(id = "CompareExperiment")

                purge_plot(session, ns, "VolcanoPlot", r6)
                purge_plot(session, ns, "AnalytePlot", r6)

                updateSelectizeInput(
                    session = session,
                    inputId = "ComparisonAnalyte",
                    selected = ""
                )

            } else {

                enable(id = "CompareExperiment")

            }

        }, ignoreInit = TRUE, ignoreNULL = TRUE)

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

            show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = "Getting Correlation Data..."
            )
            on.exit(remove_modal_spinner(), add = TRUE)

            r6()$get_correlation_data(
                input$QueryExperiment,
                input$CompareExperiment,
                input$QueryAnalyte
            )

        }) |>
            bindEvent(c(input$getData), ignoreInit = TRUE)

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
