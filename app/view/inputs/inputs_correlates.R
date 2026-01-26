box::use(
    shiny[tags, tagList, bindEvent]
)

box::use(
    app/logic/shared/server_utils,
    app/view/custom_ui/input_widgets[prettyRadioButtonsFieldSet],
    app/view/inputs/inputs_conditions_feature_analysis
)

#' @export
ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shinydashboardPlus::box(
            title = shiny::HTML(
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
            shinyjs::disabled(
                shiny::actionButton(
                    ns("PrimaryTutorial"),
                    label = "Take Tutorial",
                    class = "tutorial-btn",
                    icon = shiny::icon("question-circle")
                ) |>
                bsplus::bs_embed_tooltip(
                    title = "Click here to learn about setting dataset options
                      to generate the volcano plot",
                    placement = "top",
                    html = TRUE
                )
            ),
            shiny::tags$div(
                id = ns("scrollableOptions"),
                style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
                shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                tags$b("1) Select Query Dataset"),
                shiny::tags$div(
                    id = ns("QueryStudies"),
                    shinycustomloader::withLoader(
                        shiny::uiOutput(ns("QueryExperiment")),
                        type = "html",
                        loader = "loader6",
                        proxy.height = "20px"
                    )
                ),
                shiny::tags$hr(),
                shiny::tags$div(
                    id = ns("CompareExperiments"),
                    shinycustomloader::withLoader(
                        shiny::uiOutput(ns("CompareExperiment")),
                        type = "html",
                        loader = "loader6",
                        proxy.height = "20px"
                    )
                ),
                shiny::tags$hr(),
                tags$b("3) Select Query Analyte"),
                shiny::tags$div(
                    id = ns("QueryAnalyteInput"),
                    shiny::selectizeInput(
                        inputId = ns("QueryAnalyte"),
                        label = "",
                        choices = NULL,
                        options = list(
                            placeholder = "Please select below",
                            onInitialize = I('function() { this.setValue(""); }'),
                            closeAfterSelect = TRUE,
                            selectOnTab = TRUE,
                            persist = FALSE,
                            `live-search` = TRUE,
                            maxoptions = 1
                        )
                    )
                )
            ),
            footer = shiny::tagList(
                shiny::actionButton(
                    ns("getData"),
                    label = "Analyze & Plot",
                    class = "refresh-btn",
                    icon = shiny::icon("play")
                )
            )
        )
    )

}

#' @export
server <- function(id, r6) {

    shiny::moduleServer(id, function(input, output, session) {

        ns <- session$ns

        server_utils$bind_events(
            ids = c("QueryExperiment", "CompareExperiment", "QueryAnalyte"),
            r6 = r6,
            session = session,
            parent_input = input
        )

        output$QueryExperiment <- shiny::renderUI({

            choices <- r6$getQueryExperiments()

            selected <- ifelse(nrow(choices) == 1, choices, character(0))
        
            prettyRadioButtonsFieldSet(
                input_id = ns("QueryExperiment"),
                label = NULL,
                field_set_data = choices,
                selected = selected
            ) |>
                bsplus::bs_embed_tooltip(
                    title = "Select a study below",
                    placement = "top",
                    html = TRUE
                )

        })

        ComparisonExperiments <- shiny::reactive({

            shinybusy::show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = glue::glue("Getting Comparison Experiments...")
            )

            comparison_experiments <- r6$getComparisonExperiments()

            shinybusy::remove_modal_spinner()

            comparison_experiments

            }) |>
                shiny::bindEvent(c(input$QueryExperiment), ignoreInit = TRUE, ignoreNULL = TRUE)

        output$CompareExperiment <- shiny::renderUI({

            if (!is.null(ComparisonExperiments())) {
                shiny::tagList(
                    tags$b("2) Select Comparison Dataset"),
                    prettyRadioButtonsFieldSet(
                        input_id = ns("CompareExperiment"),
                        label = NULL,
                        field_set_data =  ComparisonExperiments(),
                        selected =  ComparisonExperiments(),
                    ) |>
                        bsplus::bs_embed_tooltip(
                            title = "Select a study below",
                            placement = "top",
                            html = TRUE
                        )
                )
            } else {
                shiny::tagList()
            }

        })

        shiny::observeEvent(c(input$CompareExperiment), {

            shinybusy::show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = glue::glue("Getting Query Analytes...")
            )

            analyte_choices <- r6$getQueryAnalytes()

            shiny::updateSelectizeInput(
                session = session,
                inputId = "QueryAnalyte",
                label = "",
                choices = analyte_choices,
                options = list(
                placeholder = "Choose Query Analyte",
                onInitialize = I('function() { this.setValue(""); }'),
                closeAfterSelect = TRUE,
                selectOnTab = TRUE,
                persist = FALSE,
                `live-search` = TRUE,
                maxoptions = 1
                )
            )

            shinybusy::remove_modal_spinner()

        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        shiny::observeEvent(c(input$QueryAnalyte), {

            if (input$QueryAnalyte == "") {

                shinyjs::disable(id = "CompareExperiment")

                purge_plot(session, ns, "VolcanoPlot", r6)
                purge_plot(session, ns, "AnalytePlot", r6)

                shiny::updateSelectizeInput(
                    session = session,
                    inputId = "ComparisonAnalyte",
                    selected = ""
                )

            } else {

                shinyjs::enable(id = "CompareExperiment")

            }

        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        shiny::observeEvent(c(input$QueryExperiment, input$QueryAnalyte, input$CompareExperiment), {

            if (any(length(input$QueryExperiment) != 1 |  input$QueryAnalyte == "" | is.null(input$CompareExperiment))) {
                shinyjs::disable("getData")
                shinyjs::removeClass(id = "getData", class = "refresh-ready-btn")
                shinyjs::addClass(id = "getData", class = "refresh-btn")
            } else {
                shinyjs::enable("getData")
                shinyjs::removeClass(id = "getData", class = "refresh-btn")
                shinyjs::addClass(id = "getData", class = "refresh-ready-btn")
            }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        # shiny::observeEvent(c(input$getData), {

        correlation_data <- shiny::reactive({

            shiny::validate(
                shiny::need(input$getData > 0, ""),
                shiny::need(input$QueryExperiment != "", ""),
                shiny::need(input$QueryAnalyte != "", ""),
                shiny::need(input$CompareExperiment != "", "")
            )

            shinybusy::show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = "Getting Correlation Data..."
            )

            data <- r6$get_correlation_data()

            shinybusy::remove_modal_spinner()

            data

        }) |>
            shiny::bindEvent(c(input$getData), ignoreInit = TRUE)

        return(
            list(
                Study = shiny::reactive(input$QueryExperiment),
                StudyData = correlation_data
            )
        )

    })

}
