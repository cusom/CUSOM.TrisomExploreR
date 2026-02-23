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
  shinyWidgets[awesomeCheckboxGroup, numericRangeInput, prettyRadioButtons],
  shinybusy[remove_modal_spinner, show_modal_spinner],
  glue[glue]
)

box::use(
  app/logic/feature_analysis/inputs/FeatureAnalysisInputs[getFeatureAnalysisInputs],
  app/logic/shared/input_locking_utils,
  app/logic/shared/server_utils,
  app/view/custom_ui/input_widgets[prettyRadioButtonsFieldSet],
  app/view/inputs/inputs_conditions_feature_analysis
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
          title = "Click here to learn about setting dataset options to generate the volcano plot",
          placement = "top",
          html = TRUE
        )
      ),
      tags$div(
        id = ns("scrollableOptions"),
        style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$div(
          id = ns("Features"),
          withLoader(
            uiOutput(ns("Feature")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$div(
          id = ns("Studies"),
          withLoader(
            uiOutput(ns("Study")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$b("Karyotype"),
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
        ),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
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
        ),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;")
      ),
      footer = tagList(
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
  )

}

#' @export
server <- function(id, app_config, analysis_config) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$Feature <- renderUI({
      selectizeInput(
        inputId = ns("Feature"),
        label = "Set Analysis Option:",
        choices = c("Karyotype", "Age", "Sex", "Comorbidity", "BMI"),
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

    output$Study <- renderUI({
      validate(
        need(input$Feature != "", "")
      )

      choices <- r6()$Studies

      selected <- ifelse(nrow(choices) == 1, choices, character(0))

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

    karyotypes <- reactive({
      r6()$Karyotypes
    }) |>
      bindEvent(c(input$Study), ignoreNULL = TRUE)

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
      req(input$Feature == "Comorbidity")
      inputs_conditions_feature_analysis$ui(ns("conditions"))
    })

    conditions <- inputs_conditions_feature_analysis$server(
      id = "conditions",
      r6 = r6,
      parent = session
    )

    output$StatTest <- renderUI({

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

      r6()$get_study_data(
        study = input$Study,
        karyotypes = input$Karyotype,
        sexes = input$Sex,
        ages = input$Age,
        conditions = if (input$Feature == "Comorbidity") conditions$selected_conditions() else NULL
      )

    }) |>
      bindEvent(input$getData, ignoreInit = TRUE)

    return(
      list(
        feature = reactive(input$Feature),
        study = reactive(input$Study),
        study_label = study_label,
        study_data = StudyData,
        stat_test = reactive(input$StatTest),
        covariates = reactive(input$Covariates),
        adjustment_method = reactive(input$AdjustmentMethod),
        fold_change_variable = reactive({r6()$fold_change_variable}),
        adjusted = adjusted
      )
    )

  })
}
