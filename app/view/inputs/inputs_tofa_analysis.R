box::use(
    shiny[
        NS,
        actionButton,
        bindEvent,
        eventReactive,
        icon,
        moduleServer,
        observe,
        observeEvent,
        reactive,
        reactiveVal,
        renderUI,
        req,
        tagList,
        uiOutput,
        tags
    ],
    shinyjs[addClass, disable, disabled, enable, removeClass, click],
    bsplus[bs_embed_tooltip, bs_accordion, bs_set_opts, bs_append],
    shinycustomloader[withLoader],
    shinyWidgets[awesomeCheckboxGroup, prettyRadioButtons, virtualSelectInput, prepare_choices],
    glue[glue]
)

box::use(
    app/logic/feature_analysis/inputs/FeatureAnalysisInputs[getFeatureAnalysisInputs],
    app/view/custom_ui/input_widgets[prettyRadioButtonsFieldSet],
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h3("Inputs"),
        bs_accordion(id = ns("AccordionInputs")) |>
        bs_set_opts(panel_type = "default", use_heading_link = TRUE) |>
        bs_append(
            title = "1) Set TOFA Dataset",
            content = list(
                withLoader(
                    uiOutput(ns("dataset")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                )
            )
        ) |>
        bs_append(
            title = "2) Set Comparison",
            content = list(
                withLoader(
                    uiOutput(ns("comparison")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                )
            )
        ) |>
        bs_append(
            title = "3) Set Participant Attributes (optional)",
            content = list(
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
                )
            )
        ),
        tags$div(
            actionButton(
                ns("run_analysis"),
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

        ANALYSIS_CONST <- list(
            feature_id = "timepoint",
            analysis_id = "tofa_feature_association",
            statistic_id = "linear_model",
            feature_label = "Timepoint",
            stat_test_label = "Linear Model",
            covariates = c("Sex", "Age"),
            adjustment_method = "BH",
            fold_change_variable = "Event_Name"
        )

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
        study_plan_cache <- reactiveVal(list())
        accordion_opened <- reactiveVal(FALSE)

        build_study_plan <- function(dataset_id) {
            context <- app_config$feature_association_planner$create_context(
                feature_id = ANALYSIS_CONST$feature_id,
                dataset_id = dataset_id,
                statistic_id = ANALYSIS_CONST$statistic_id,
                filters = list(),
                covariates = character(0),
                visualization = list(),
                analysis_id = ANALYSIS_CONST$analysis_id
            )

            app_config$feature_association_planner$plan(context)
        }

        get_or_build_study_plan <- function(dataset_id) {
            req(dataset_id)

            cache <- study_plan_cache()
            if (!is.null(cache[[dataset_id]])) {
                return(cache[[dataset_id]])
            }

            plan <- build_study_plan(dataset_id)
            cache[[dataset_id]] <- plan
            study_plan_cache(cache)
            plan
        }

        # Recreate the R6 instance when Dataset changes (including initial selection).
        observeEvent(input$dataset, ignoreInit = FALSE, {
            req(input$dataset)

            if (!accordion_opened()) {
                click(glue("AccordionInputs-1-heading"), asis = FALSE)
                accordion_opened(TRUE)
            }

            inst <- getFeatureAnalysisInputs(
                app_config = app_config,
                analysis_config = app_config$get_analysis_config(input$dataset),
                input_config = app_config$get_input_config(input$dataset),
                dataset = input$dataset
            )
            r6_obj(inst)

            # Precompute once; subsequent runs read from cache.
            get_or_build_study_plan(input$dataset)
        })

        # Expose a reactive that always reads the current instance.
        r6 <- reactive({
            req(r6_obj())
            r6_obj()
        })

        output$sexes <- renderUI({
            source <- r6()
            disabled(
                awesomeCheckboxGroup(
                    inputId = ns("sexes"),
                    label = "Sex",
                    choices = source$Sexes,
                    selected = source$Sexes,
                    inline = TRUE,
                    width = "90%"
                )
            )
        }) |>
            bindEvent(r6_obj(), ignoreNULL = TRUE)

        output$karyotype <- renderUI({
            source <- r6()
            disabled(
                prettyRadioButtons(
                    inputId = ns("karyotype"),
                    label = "Karyotype",
                    choiceNames = source$Karyotypes,
                    choiceValues = source$Karyotypes,
                    inline = TRUE,
                    width = "90%"
                )
            )
        }) |>
            bindEvent(r6_obj(), ignoreNULL = TRUE)

        output$age_group <- renderUI({
            source <- r6()
            disabled(
                awesomeCheckboxGroup(
                    inputId = ns("age_group"),
                    label = "Age Groups",
                    choices = source$Age_Groups,
                    selected = source$Age_Groups,
                    inline = FALSE,
                    width = "90%"
                )
            )
        }) |>
            bindEvent(r6_obj(), ignoreNULL = TRUE)

        output$comparison <- renderUI({
            source <- r6()
            virtualSelectInput(
                inputId = ns("comparison"),
                label = "Comparisons Available",
                choices = prepare_choices(
                    source$baseline_comparisons,
                    label = analysis,
                    value = events
                ),
                selected = character(0),
                multiple = FALSE,
                search = FALSE
            )
        }) |>
            bindEvent(r6_obj(), ignoreNULL = TRUE)

        is_ready_to_analyze <- reactive({
            source_ready <- !is.null(r6_obj())
            comparison_ready <- !is.null(input$comparison) && nzchar(trimws(input$comparison))
            source_ready && comparison_ready
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

        analysis_run <- eventReactive(input$run_analysis, {
            req(is_ready_to_analyze())

            dataset_id <- input$dataset
            comparison_value <- input$comparison

            filters <- list(
                sexes = input$sexes,
                karyotype = input$karyotype,
                age_groups = input$age_group,
                comparison = comparison_value
            )

            data <- do.call(
                r6()$get_study_data,
                c(
                    list(
                        study = dataset_id,
                        stat_test = ANALYSIS_CONST$stat_test_label,
                        covariates = ANALYSIS_CONST$covariates,
                        adjustment_method = ANALYSIS_CONST$adjustment_method
                    ),
                    filters
                )
            )

            list(
                study_data = data,
                study_plan = get_or_build_study_plan(dataset_id),
                comparison = comparison_value,
                study = dataset_id
            )
        }, ignoreInit = TRUE)

        study_data <- reactive({
            req(analysis_run())
            analysis_run()$study_data
        })

        study_plan <- reactive({
            req(analysis_run())
            analysis_run()$study_plan
        })

        selected_comparison <- reactive({
            req(analysis_run())
            analysis_run()$comparison
        })

        return(
            list(
                feature = reactive({ANALYSIS_CONST$feature_label}),
                study = reactive({input$dataset}),
                study_label = reactive({input$dataset}),
                study_data = study_data,
                study_plan = study_plan,
                stat_test = reactive({ANALYSIS_CONST$stat_test_label}),
                covariates = reactive({ANALYSIS_CONST$covariates}),
                adjustment_method = reactive({ANALYSIS_CONST$adjustment_method}),
                fold_change_variable = reactive({ANALYSIS_CONST$fold_change_variable}),
                adjusted = reactive(TRUE),
                comparison = selected_comparison
            )
        )

    })
}
