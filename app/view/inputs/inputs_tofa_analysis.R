box::use(
    shiny[
        NS,
        actionButton,
        bindEvent,
        HTML,
        icon,
        moduleServer,
        need,
        observe,
        observeEvent,
        reactive,
        reactiveVal,
        renderUI,
        req,
        selectizeInput,
        tagList,
        uiOutput,
        validate,
        tags
    ],
    shinydashboardPlus[box],
    shinyjs[addClass, disable, disabled, enable, removeClass],
    bsplus[bs_embed_tooltip],
    shinycustomloader[withLoader],
    shinyWidgets[awesomeCheckboxGroup, numericRangeInput, prettyRadioButtons, virtualSelectInput,
        prepare_choices, updatePrettyRadioButtons],
    shinybusy[remove_modal_spinner, show_modal_spinner],
    glue[glue],
    dplyr[arrange, filter, pull],
    rlang[set_names]
)


box::use(
    app/logic/tofa_analysis/inputs/AnalysisInputs[getAnalysisInputs],
    app/view/custom_ui/input_widgets[prettyRadioButtonsFieldSet],
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Set Analysis Inputs",
            height = "70vh",
            width = NULL,
            closable = FALSE,
            solidHeader = FALSE,
            collapsible = FALSE,
            headerBorder = FALSE,
            tags$div(
                id = ns("scrollableOptions"),
                style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                withLoader(
                    uiOutput(ns("dataset")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                tags$b("Karyotype"),
                withLoader(
                    uiOutput(ns("karyotype")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                tags$b("Sex"),
                withLoader(
                    uiOutput(ns("sexes")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                withLoader(
                    uiOutput(ns("age_group")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                withLoader(
                    uiOutput(ns("feature")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                withLoader(
                    uiOutput(ns("plot_type")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                )
            ),
            footer = tagList(
                actionButton(
                    ns("run_analysis"),
                    label = "Analyze & Plot",
                    class = "refresh-btn",
                    icon = icon("play")
                )
            )
        )
    )

}

#' @export
server <- function(id, analysis_config) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        output$dataset <- renderUI({

            choices <- analysis_config$datasets

            selected <- ifelse(nrow(choices) == 1, choices, character(0))

            prettyRadioButtonsFieldSet(
                input_id = ns("dataset"),
                label = NULL,
                field_set_data = choices,
                selected = selected
            ) |>
                bs_embed_tooltip(
                    title = "Select a dataset below",
                    placement = "top",
                    html = TRUE
                )

        })

        r6_obj <- reactiveVal(NULL)

        # Recreate the R6 instance when Dataset changes
        observeEvent(input$dataset, ignoreInit = TRUE, {
            req(input$dataset)
            inst <- getAnalysisInputs(
                analysis_config = analysis_config,
                dataset = input$dataset
            )
            r6_obj(inst)
        })

        #expose a reactive that always reads the current instance
        r6 <- reactive({
            req(r6_obj())
            r6_obj()
        })

        output$sexes <- renderUI({
            disabled(
                awesomeCheckboxGroup(
                    inputId = ns("sexes"),
                    label = "",
                    choices = r6()$sexes,
                    selected = r6()$sexes,
                    inline = TRUE,
                    width = "90%"
                )
            )
        })

        output$karyotype <- renderUI({
            disabled(
                prettyRadioButtons(
                    inputId = ns("karyotype"),
                    label = "",
                    choiceNames = r6()$karyotype,
                    choiceValues = r6()$karyotype,
                    inline = TRUE,
                    width = "90%"
                )
            )
        })

        output$age_group <- renderUI({
            disabled(
                awesomeCheckboxGroup(
                    inputId = ns("age_group"),
                    label = "Age Groups",
                    choices = r6()$age_groups,
                    selected = r6()$age_groups,
                    inline = FALSE,
                    width = "90%"
                )
            )
        })


        output$comparisons <- renderUI({
            virtualSelectInput(
                inputId = ns("comparisons"),
                label = "Comparisons Available",
                choices = prepare_choices(
                    r6()$event_comparisons,
                    label = analysis,
                    value = events
                ),
                selected = character(0),
                multiple = FALSE,
                search = TRUE
            )
        })

        output$feature <- renderUI({
            selectizeInput(
                inputId = ns("feature"),
                label = "Choose Score / Endpoint",
                choices = r6()$features,
                selected = r6()$features[1],
                multiple = FALSE,
                options = list(
                    placeholder = "Select below",
                    onInitialize = I('function() { this.setValue(""); }'),
                    closeAfterSelect = TRUE,
                    selectOnTab = TRUE,
                    persist = FALSE,
                    dropupAuto = FALSE
                )
            )
        })

        output$plot_type <- renderUI({
            prettyRadioButtons(
                inputId = ns("plot_type"),
                label = "Show Trial Data as:",
                choiceNames = c("All Events", "Differences from Baseline"),
                choiceValues = c("Base", "Difference"),
                inline = FALSE,
                width = "90%"
            )
        })

        observeEvent(input$feature, {
            updatePrettyRadioButtons(
                session = session,
                inputId = "plot_type",
                label = glue("Show {input$feature} as:")
            )
        }, ignoreInit = TRUE)

        is_ready_to_analyze <- reactive({
            feature_ready <- !is.null(input$feature) && nzchar(trimws(input$feature))
            plot_type_ready <- !is.null(input$plot_type) && nzchar(trimws(input$plot_type))
            feature_ready && plot_type_ready
        })

        observe({
            if (is_ready_to_analyze()) {
                enable("run_analysis")
                removeClass("run_analysis", "refresh-btn")
                addClass("run_analysis", "refresh-ready-btn")
            } else {
                disable("run_analysis")
                removeClass("run_analysis", "refresh-ready-btn")
                addClass("run_analysis", "refresh-btn")
            }
        })

        cohort <- reactive({
            r6()$get_data(
                input$sexes,
                input$races,
                input$ethnicities,
                input$karyotype,
                input$age,
                input$age_group,
                input$conditions
            )
        }) |>
            bindEvent(input$run_analysis, ignoreInit = TRUE)

        return(
            list(
                cohort = cohort,
                feature = reactive({input$feature}),
                plot_type = reactive({input$plot_type})
            )
        )

    })
}
