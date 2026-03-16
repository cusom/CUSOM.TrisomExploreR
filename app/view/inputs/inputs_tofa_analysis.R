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
                style = "height:70vh;padding-left:2px;max-height:70vh;overflow-y:auto;overflow-x:hidden;",
                sapply(
                    list("sexes",
                        #"races", "ethnicities",
                        "down_syndrome_status",
                        #"age",
                        "age_group",
                        #"conditions",
                        "feature",
                        "plot_type"),
                    function(x) {
                        return(
                            tagList(
                                tags$div(
                                    withLoader(
                                        uiOutput(ns(x)),
                                        type = "html",
                                        loader = "loader6",
                                        proxy.height = "20px"
                                    )
                                ),
                                tags$hr(style = "margin-top:15px;margin-bottom:15px;")
                            )
                        )
                    }
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

        r6 <- getAnalysisInputs(
            analysis_config$participant_data,
            analysis_config$encounter_data,
            analysis_config$datasets
        )

        output$sexes <- renderUI({
            awesomeCheckboxGroup(
                inputId = ns("sexes"),
                label = "Sex",
                choices = r6$sexes,
                selected = r6$sexes,
                inline = TRUE,
                width = "90%"
            )
        })

        output$races <- renderUI({
            awesomeCheckboxGroup(
                inputId = ns("races"),
                label = "Race",
                choices = r6$races,
                selected = r6$races,
                inline = FALSE,
                width = "90%"
            )
        })

        output$ethnicities <- renderUI({
            awesomeCheckboxGroup(
                inputId = ns("ethnicities"),
                label = "Ethnicity",
                choices = r6$ethnicities,
                selected = r6$ethnicities,
                inline = FALSE,
                width = "90%"
            )
        })

        output$down_syndrome_status <- renderUI({
            disabled(
                prettyRadioButtons(
                    inputId = ns("down_syndrome_status"),
                    label = "Down syndrome status",
                    choiceNames = r6$down_syndrome_status,
                    choiceValues = r6$down_syndrome_status,
                    inline = TRUE,
                    width = "90%"
                )
            )
        })

        output$age <- renderUI({
            numericRangeInput(
                inputId = ns("age"),
                label = "Age at visit (in days)",
                value = c(min(r6$age_at_visit, na.rm = TRUE), max(r6$age_at_visit, na.rm = TRUE)),
                min = min(r6$age_at_visit, na.rm = TRUE),
                max = max(r6$age_at_visit, na.rm = TRUE),
                step = 1,
                width = "90%"
            )
        })

        output$age_group <- renderUI({
            awesomeCheckboxGroup(
                inputId = ns("age_group"),
                label = "Age Groups",
                choices = r6$age_groups,
                selected = r6$age_groups,
                inline = FALSE,
                width = "90%"
            )
        })

        output$conditions <- renderUI({
            selectizeInput(
                inputId = ns("conditions"),
                label = "Qualifying conditions",
                choices = r6$conditions,
                selected = NULL,
                multiple = TRUE,
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

        output$comparisons <- renderUI({
            virtualSelectInput(
                inputId = ns("comparisons"),
                label = "Comparisons Available",
                choices = prepare_choices(
                    r6$event_comparisons,
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
                label = "Feature to Analyze",
                choices = r6$features,
                selected = r6$features[1],
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
                label = "Show as:",
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

        data <- reactive({
            r6$get_data(
                input$sexes,
                input$races,
                input$ethnicities,
                input$down_syndrome_status,
                input$age,
                input$age_group,
                input$conditions
            )
        }) |>
            bindEvent(input$run_analysis, ignoreInit = TRUE)

        return(
            list(
                data = data,
                feature = reactive({input$feature}),
                plot_type = reactive({input$plot_type})
            )
        )

    })
}
