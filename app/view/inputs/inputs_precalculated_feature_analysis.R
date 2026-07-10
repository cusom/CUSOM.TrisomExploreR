box::use(
  shiny[NS, tagList, HTML, actionButton, icon, uiOutput, moduleServer, renderUI,
    observeEvent, validate, need, reactive, bindEvent, tagAppendAttributes,
    tags, reactiveVal, req, selectizeInput],
  shinydashboardPlus[box],
  shinyjs[disabled, removeClass, addClass, disable, enable, hidden],
  bsplus[bs_embed_tooltip],
  shinycustomloader[withLoader],
  shinyWidgets[prettyRadioButtons, awesomeCheckboxGroup, numericRangeInput],
  shinybusy[show_modal_spinner, remove_modal_spinner],
  glue[glue, glue_collapse],
  dplyr[arrange, filter, pull],
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
            disabled(
              uiOutput(ns("Study"))
            ),
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
          disabled(
            uiOutput(ns("Sex"))
          ),
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
          disabled(
            uiOutput(ns("StatTest"))
          ),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$b("Adjust for covariates"),
        withLoader(
          uiOutput(ns("Covariates")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        withLoader(
          disabled(
            uiOutput(ns("AdjustmentMethod"))
          ),
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
      choices <- tryCatch({
        analysis_def <- analysis_config$get_catalog_analysis_definition("feature_association")
        feature_ids <- names(analysis_def$features)

        if (length(feature_ids) == 0) {
          stop("No features found in feature_association catalog definition.")
        }

        labels <- vapply(feature_ids, function(feature_id) {
          feature_def <- analysis_config$get_catalog_feature_definition(feature_id)
          display_name <- feature_def$display_name
          if (is.null(display_name) || !nzchar(display_name)) display_name <- feature_id
          display_name
        }, FUN.VALUE = character(1))

        values <- vapply(feature_ids, function(feature_id) {
          analysis_config$get_analysis_config(feature_id)$AnalysisVariableName[[1]] %||% feature_id
        }, FUN.VALUE = character(1))

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

    output$Study <- renderUI({
      validate(
        need(input$Feature != "", "")
      )

      choices <- r6()$Studies

      selected <- ifelse(nrow(choices) == 1, choices, character(0))
      disabled(
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
      disabled(
        awesomeCheckboxGroup(
          inputId = ns("Sex"),
          label = NULL,
          choices = sexes(),
          selected = sexes(),
          inline = TRUE,
          width = "90%"
        )
      )
    })

    ages <- reactive({
      r6()$Ages
    }) |>
      bindEvent(c(input$Study), ignoreNULL = TRUE)

    output$Age <- renderUI({
      disabled(
        numericRangeInput(
          inputId = ns("Age"),
          label = "Age range",
          value = ages(),
          width = "90%"
        )
      )
    })


    output$StatTest <- renderUI({
      disabled(
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
      )

    })

    output$Covariates <- renderUI({
      validate(
        need(input$StatTest != "", "")
      )

      choices <- r6()$CovariateChoices

      awesomeCheckboxGroup(
        inputId = ns("Covariates"),
        label = NULL,
        choices = choices,
        selected = choices,
        inline = TRUE
      )

    })

    output$AdjustmentMethod <- renderUI({
      disabled(
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
      )
    })

    adjusted <- reactive({
      input$AdjustmentMethod != "none"
    })

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
        !is.null(input$Covariates) && length(input$Covariates) > 0
      }
    )

    params <- reactive({
      validate(
        need(input$Covariates, "")
      )
      ifelse(
        is.null(input$Covariates),
        "none",
        glue_collapse(input$Covariates, ";")
      )
    })

    StudyData <- reactive({
      validate(
        need(input$getData > 0, ""),
        need(input$Study != "", ""),
        need(input$Karyotype != "", "")
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
        params = params()
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
