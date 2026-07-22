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

            selected <- if (nrow(choices) == 1) choices$Values[[1]] else character(0)

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

        # Recreate the R6 instance when Dataset changes (including initial selection)
        observeEvent(input$dataset, ignoreInit = FALSE, {
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

        run_snapshot <- reactiveVal(NULL)

        build_study_plan <- function(dataset_id) {
            context <- app_config$feature_association_planner$create_context(
                feature_id = "timepoint",
                dataset_id = dataset_id,
                statistic_id = "linear_model",
                filters = list(),
                covariates = character(0),
                visualization = list(),
                analysis_id = "tofa_feature_association"
            )

            app_config$feature_association_planner$plan(context)
        }

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
            source_ready <- !is.null(r6_obj())
            comparison_ready <- !is.null(input$comparison) && nzchar(trimws(input$comparison))
            source_ready && comparison_ready
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

        observeEvent(input$run_analysis, ignoreInit = TRUE, {
            req(is_ready_to_analyze())

            dataset_id <- input$dataset
            comparison_value <- input$comparison

            data <- r6()$get_study_data(
                study = input$dataset,
                input$sexes,
                input$races,
                input$ethnicities,
                input$karyotype,
                input$age,
                input$age_group,
                input$conditions,
                input$comparison,
                stat_test = "Linear Model",
                covariates = c("Sex", "Age"),
                adjustment_method = "BH"
            )

            plan <- build_study_plan(dataset_id)

            run_snapshot(
                list(
                    study_data = data,
                    study_plan = plan,
                    comparison = comparison_value,
                    study = dataset_id
                )
            )
        })

        study_data <- reactive({
            req(run_snapshot())
            run_snapshot()$study_data
        })

        study_plan <- reactive({
            req(run_snapshot())
            run_snapshot()$study_plan
        })

        selected_comparison <- reactive({
            req(run_snapshot())
            run_snapshot()$comparison
        })

        return(
            list(
                feature = reactive({"Timepoint"}),
                study = reactive({input$dataset}),
                study_label = reactive({input$dataset}),
                study_data = study_data,
                study_plan = study_plan,
                stat_test = reactive({"Linear Model"}),
                covariates = reactive({c("Sex", "Age")}),
                adjustment_method = reactive({"BH"}),
                fold_change_variable = reactive({"Event_Name"}),
                adjusted = reactive(TRUE),
                comparison = selected_comparison
            )
        )

    })
}
