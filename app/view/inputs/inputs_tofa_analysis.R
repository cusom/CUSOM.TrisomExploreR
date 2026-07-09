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
    app/logic/feature_analysis/inputs/FeatureAnalysisInputs[getFeatureAnalysisInputs],
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
                withLoader(
                    uiOutput(ns("karyotype")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
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
                    uiOutput(ns("comparison")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
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
server <- function(id, app_config, analysis_config) {

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

            inst <- getFeatureAnalysisInputs(
                app_config = app_config,
                analysis_config = app_config$get_analysis_config(input$dataset),
                input_config = app_config$get_input_config(input$dataset),
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
                    label = "Sex",
                    choices = r6()$Sexes,
                    selected = r6()$Sexes,
                    inline = TRUE,
                    width = "90%"
                )
            )
        })

        output$karyotype <- renderUI({
            disabled(
                prettyRadioButtons(
                    inputId = ns("karyotype"),
                    label = "Karyotype",
                    choiceNames = r6()$Karyotypes,
                    choiceValues = r6()$Karyotypes,
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
                    choices = r6()$Age_Groups,
                    selected = r6()$Age_Groups,
                    inline = FALSE,
                    width = "90%"
                )
            )
        })

        output$comparison <- renderUI({
            virtualSelectInput(
                inputId = ns("comparison"),
                label = "Comparisons Available",
                choices = prepare_choices(
                    r6()$baseline_comparisons,
                    label = analysis,
                    value = events
                ),
                selected = character(0),
                multiple = FALSE,
                search = FALSE
            )
        })

        is_ready_to_analyze <- reactive({
            comparison_ready <- !is.null(input$comparison) && nzchar(trimws(input$comparison))
            comparison_ready
            # feature_ready <- !is.null(input$feature) && nzchar(trimws(input$feature))
            # plot_type_ready <- !is.null(input$plot_type) && nzchar(trimws(input$plot_type))
            # feature_ready && plot_type_ready
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

        # cohort <- reactive({
        #     req(input$dataset)
        #     req(input$comparison)
        #     r6()$get_cohort(
        #         input$sexes,
        #         input$races,
        #         input$ethnicities,
        #         input$karyotype,
        #         input$age,
        #         input$age_group,
        #         input$conditions
        #     )
        # })

        study_data <- reactive({
            r6()$get_study_data(
                input$sexes,
                input$races,
                input$ethnicities,
                input$karyotype,
                input$age,
                input$age_group,
                input$conditions,
                input$comparison
            )
        }) |>
            bindEvent(input$run_analysis, ignoreInit = TRUE)

        return(
            list(
                feature = reactive({"Timepoint"}),
                study = reactive({input$dataset}),
                study_label = reactive({input$dataset}),
                study_data = study_data,
                stat_test = reactive({"Linear Model"}),
                covariates = reactive({c("Sex", "Age")}),
                adjustment_method = reactive({"BH"}),
                fold_change_variable = reactive({"Event_Name"}),
                adjusted = reactive(TRUE),
                comparison = reactive({input$comparison})
            )
        )

    })
}
