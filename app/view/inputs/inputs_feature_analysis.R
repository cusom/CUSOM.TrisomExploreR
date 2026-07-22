box::use(
  shiny[
    NS,
    actionButton,
    bindEvent,
    HTML,
    icon,
    moduleServer,
    need,
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
  shinyjs[disabled, click],
  bsplus[bs_embed_tooltip, bs_accordion, bs_set_opts, bs_append],
  shinycustomloader[withLoader],
  shinyWidgets[awesomeCheckboxGroup, numericRangeInput, prettyRadioButtons],
  shinybusy[remove_modal_spinner, show_modal_spinner],
  glue[glue],
  dplyr[arrange, filter, pull, mutate, select],
  tibble[deframe]
)

box::use(
  app/logic/feature_analysis/inputs/FeatureAnalysisInputs[getFeatureAnalysisInputs],
  app/logic/shared/input_locking_utils,
  app/logic/shared/server_utils,
  app/view/custom_ui/input_widgets[prettyRadioButtonsFieldSet],
  app/view/inputs/inputs_conditions_feature_analysis
)

catalog_feature_to_legacy_value <- function(feature_id) {
  mapping <- c(
    karyotype = "Karyotype",
    age = "Age",
    sex = "Sex",
    bmi = "BMI",
    comorbidity = "HasAnyConditionFlag"
  )

  mapped <- mapping[[feature_id]]

  if (!is.null(mapped)) {
    return(mapped)
  }

  feature_id
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Inputs"),
    bs_accordion(id = ns("AccordionInputs")) |>
        bs_set_opts(panel_type = "default", use_heading_link = TRUE) |>
        bs_append(
          title = "1) Set Analysis",
          content = list(
            withLoader(
              uiOutput(ns("Feature")),
              type = "html",
              loader = "loader6",
              proxy.height = "20px"
            )
          )
        ) |>
        bs_append(
          title = "2) Choose Study",
          content = list(
            withLoader(
              uiOutput(ns("Study")),
              type = "html",
              loader = "loader6",
              proxy.height = "20px"
            )
          )
        ) |>
        bs_append(
          title = "3) Set Participant Attributes",
          content = list(
            tagList(
              withLoader(
                uiOutput(ns("Karyotype")),
                type = "html",
                loader = "loader6",
                proxy.height = "20px"
              ),
              tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
              uiOutput(ns("ConditionsInputs")),
              tags$b("Sex"),
              withLoader(
                uiOutput(ns("Sex")),
                type = "html",
                loader = "loader6",
                proxy.height = "20px"
              ),
              tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
              withLoader(
                uiOutput(ns("Age")),
                type = "html",
                loader = "loader6",
                proxy.height = "20px"
              )
            )
          )
        ) |>
        bs_append(
          title = "4) Set Statistics",
          content = list(
            tagList(
              withLoader(
                uiOutput(ns("StatTest")),
                type = "html",
                loader = "loader6",
                proxy.height = "20px"
              ),
              tags$br(),
              tags$b("Adjust for covariates"),
              withLoader(
                uiOutput(ns("Covariates")),
                type = "html",
                loader = "loader6",
                proxy.height = "20px"
              ),
              withLoader(
                uiOutput(ns("AdjustmentMethod")),
                type = "html",
                loader = "loader6",
                proxy.height = "20px"
              )
            )
          )
        ),
        tags$div(
          disabled(
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
    condition_feature_options <- c("Comorbidity", "HasAnyConditionFlag", "Co-Occuring Conditions", "comorbidity")

    output$Feature <- renderUI({
      choices <- tryCatch({
        analysis_def <- app_config$get_catalog_analysis_definition("feature_association")
        feature_ids <- names(analysis_def$features)

        if (length(feature_ids) == 0) {
          stop("No features found in feature_association catalog definition.")
        }

        labels <- vapply(feature_ids, function(feature_id) {
          feature_def <- app_config$get_catalog_feature_definition(feature_id)
          display_name <- feature_def$display_name

          if (is.null(display_name) || !nzchar(display_name)) {
            display_name <- feature_id
          }

          display_name
        }, FUN.VALUE = character(1))

        values <- vapply(feature_ids, catalog_feature_to_legacy_value, FUN.VALUE = character(1))

        stats::setNames(values, labels)
      }, error = function(e) {
        character(0)
      })

      selectizeInput(
        inputId = ns("Feature"),
        label = "Set Analysis Option:",
        choices = choices,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Select analysis below",
          onInitialize = I('function() { this.setValue(""); }'),
          closeAfterSelect = TRUE,
          selectOnTab = TRUE,
          persist = FALSE,
          dropupAuto = FALSE
        )
      )
    })

    r6_obj <- reactiveVal(NULL)
    study_plan_val <- reactiveVal(NULL)

    # Recreate the R6 instance when Feature changes
    observeEvent(input$Feature, ignoreInit = TRUE, {
      req(input$Feature)
      inst <- getFeatureAnalysisInputs(
        app_config = app_config,
        analysis_config = app_config$get_analysis_config(input$Feature),
        input_config = app_config$get_input_config(input$Feature)
      )
      r6_obj(inst)
    })

    #expose a reactive that always reads the current instance
    r6 <- reactive({
      req(r6_obj())
      r6_obj()
    })

    server_utils$bind_events(
      ids = c("Study"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    observeEvent(c(input$Feature), {
      req(input$Feature)
      click(glue("AccordionInputs-1-heading"), asis = FALSE)
    }, ignoreInit = TRUE)

    output$Study <- renderUI({
      validate(
        need(input$Feature != "", "")
      )

      choices <- r6()$Studies

      selected <- if (nrow(choices) == 1) choices$Values[[1]] else character(0)

      prettyRadioButtonsFieldSet(
        input_id = ns("Study"),
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

    study_label <- reactive({
      r6()$StudyLabel
    })

    observeEvent(c(input$Study), {
      req(input$Study)
      click(glue("AccordionInputs-2-heading"), asis = FALSE)
    }, ignoreInit = TRUE)

    karyotypes <- reactive({
      req(!is.null(input$Study), nzchar(input$Study))

      inst <- r6()

      studies <- inst$Studies
      req(!is.null(studies), nrow(studies) > 0)
      req(any(as.character(studies$Values) == as.character(input$Study)))

      # Keep datasource state aligned when a new manager instance is created
      # on feature changes but Study input value itself does not emit a change.
      inst$Study <- input$Study

      inst$Karyotypes
    }) |>
      bindEvent(c(input$Feature, input$Study), ignoreNULL = TRUE)

    output$Karyotype <- renderUI({

      karyotype_choices <- karyotypes()

      input <- prettyRadioButtons(
        inputId = ns("Karyotype"),
        label = NULL,
        choiceNames = lapply(karyotype_choices$choiceNames, HTML),
        choiceValues = karyotype_choices$choiceValues,
        inline = FALSE,
        width = "90%"
      )

      if (nrow(karyotype_choices) == 1) {
        disabled(
          input
        )
      } else {
        input
      }
    })

    sexes <- reactive({
      r6()$Sexes
    }) |>
      bindEvent(c(input$Study), ignoreNULL = TRUE)

    output$Sex <- renderUI({

      awesomeCheckboxGroup(
        inputId = ns("Sex"),
        label = NULL,
        choices = sexes(),
        selected = sexes(),
        inline = TRUE,
        width = "90%"
      )
    })

    ages <- reactive({
      r6()$Ages
    }) |>
      bindEvent(c(input$Study), ignoreNULL = TRUE)

    output$Age <- renderUI({
      numericRangeInput(
        inputId = ns("Age"),
        label = "Age range",
        value = ages(),
        width = "90%"
      )
    })

    # Only show conditions UI when Feature is Comorbidity
    output$ConditionsInputs <- renderUI({
      req(input$Feature %in% condition_feature_options)
      inputs_conditions_feature_analysis$ui(ns("conditions"))
    })

    conditions <- inputs_conditions_feature_analysis$server(
      id = "conditions",
      r6 = r6,
      parent = session
    )

    output$StatTest <- renderUI({
      validate(
        need(input$Study != "", "")
      )

      prettyRadioButtons(
        inputId = ns("StatTest"),
        label = "Statistical test",
        choices = NULL,
        selected = NULL,
        status = "primary",
        icon = NULL,
        inline = FALSE,
        width = "90%",
        choiceNames = r6()$StatTestNames,
        choiceValues = r6()$StatTestValues
      )

    })

    output$Covariates <- renderUI({
      validate(
        need(input$StatTest != "", "")
      )
      if (input$StatTest == "Linear Model") {

        choices <- r6()$CovariateChoices

        tagList(
          awesomeCheckboxGroup(
            inputId = ns("Covariates"),
            label = NULL,
            choices = choices,
            selected = choices,
            inline = TRUE
          )
        )
      } else {
        tagList(

        )
      }
    })

    output$AdjustmentMethod <- renderUI({

      prettyRadioButtons(
        inputId = ns("AdjustmentMethod"),
        label = "Multiple hypothesis correction",
        choices = NULL,
        selected = NULL,
        status = "primary",
        icon = NULL,
        inline = FALSE,
        width = "90%",
        choiceNames = r6()$AdjustmentMethodNames,
        choiceValues = r6()$AdjustmentMethodValues
      )

    })

    adjusted <- reactive({
      input$AdjustmentMethod != "none"
    })

    feature_locked_inputs <- list(
      Sex = c("Sex"),
      Age = c("Age")
    )

    input_locking_utils$bind_feature_locked_inputs(
      input = input,
      session = session,
      feature_locked_inputs = feature_locked_inputs,
      trigger_ids = c("Study", "Feature"),
      feature_input_id = "Feature"
    )

    input_locking_utils$bind_action_button_state(
      session = session,
      button_id = "getData",
      is_ready_fn = function() {
        !is.null(input$Study) && input$Study != ""
      },
      can_enable_fn = function() {
        has_study <- !is.null(input$Study) && input$Study != ""
        if (!has_study) {
          return(FALSE)
        }

        has_karyotype <- !is.null(input$Karyotype) && nzchar(input$Karyotype)
        has_sex <- !is.null(input$Sex) && length(input$Sex) > 0
        has_age <- !is.null(input$Age) && length(input$Age) == 2 && all(!is.na(input$Age))
        has_stat_test <- !is.null(input$StatTest) && nzchar(input$StatTest)
        has_adjustment <- !is.null(input$AdjustmentMethod) && nzchar(input$AdjustmentMethod)

        if (!(has_karyotype && has_sex && has_age && has_stat_test && has_adjustment)) {
          return(FALSE)
        }

        if (input$Feature != "Comorbidity") {
          return(TRUE)
        }

        selected_conditions <- conditions$selected_conditions()
        !is.null(selected_conditions) && length(selected_conditions) > 0
      }
    )

    StudyData <- reactive({
      validate(
        need(input$getData > 0, ""),
        need(input$Study != "", ""),
        need(input$Karyotype != "", ""),
        need(input$Sex != "", ""),
        need(input$Age[1] != "", ""),
        need(input$Age[2] != "", "")
      )

      show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = glue("Fetching {study_label()} data...")
        )
      on.exit(remove_modal_spinner(), add = TRUE)

      data <- r6()$get_study_data(
        study = input$Study,
        karyotypes = input$Karyotype,
        sexes = input$Sex,
        ages = input$Age,
        stat_test = input$StatTest,
        covariates = input$Covariates,
        adjustment_method = input$AdjustmentMethod,
        conditions = if (input$Feature %in% condition_feature_options) conditions$selected_conditions() else NULL
      )

      study_plan_val(r6()$StudyPlan)
      data

    }) |>
      bindEvent(input$getData, ignoreInit = TRUE)

    StudyPlan <- reactive({
      study_plan_val()
    })

    return(
      list(
        feature = reactive(input$Feature),
        study = reactive(input$Study),
        karyotype = reactive(input$Karyotype),
        study_label = study_label,
        study_data = StudyData,
        study_plan = StudyPlan,
        stat_test = reactive(input$StatTest),
        covariates = reactive(input$Covariates),
        adjustment_method = reactive(input$AdjustmentMethod),
        fold_change_variable = reactive({r6()$fold_change_variable}),
        adjusted = adjusted
      )
    )

  })
}
