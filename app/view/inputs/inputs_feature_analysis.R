box::use(
  shiny[tags, bindEvent]
)

box::use(
  app/logic/feature_analysis/inputs/inputs_manager_factory[get_inputs_manager],
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
          title = "Click here to learn about setting dataset options to generate the volcano plot",
          placement = "top",
          html = TRUE
        )
      ),
      shiny::tags$div(
        id = ns("scrollableOptions"),
        style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shiny::tags$div(
          id = ns("Features"),
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("Feature")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shiny::tags$div(
          id = ns("Studies"),
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("Study")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$b("Karyotype"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("Karyotype")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        inputs_conditions_feature_analysis$ui(ns("conditions")),
        tags$b("Sex"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("Sex")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("Age")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("StatTest")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        tags$br(),
        tags$b("Adjust for covariates"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("Covariates")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("AdjustmentMethod")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;")
      ),
      footer = shiny::tagList(
        shinyjs::disabled(
          shiny::actionButton(
            ns("getData"),
            label = "Analyze & Plot",
            class = "refresh-btn",
            icon = shiny::icon("play")
          )
        )
      )
    )
  )

}

#' @export
server <- function(id, app_config, analysis_config) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$Feature <- shiny::renderUI({
      shiny::selectizeInput(
        inputId = ns("Feature"),
        label = "Set Analysis Option:",
        choices = c("Karyotype", "Age", "Sex", "BMI"),
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

    r6_obj <- shiny::reactiveVal(NULL)

    # Recreate the R6 instance when Feature changes
    shiny::observeEvent(input$Feature, ignoreInit = TRUE, {
      shiny::req(input$Feature)
      inst <- get_inputs_manager(
        app_config = app_config,
        analysis_config = app_config$get_analysis_config(input$Feature),
        input_config = app_config$get_input_config(input$Feature)
      )
      r6_obj(inst)
    })

    #expose a reactive that always reads the current instance
    r6 <- shiny::reactive({
      shiny::req(r6_obj())
      r6_obj()
    })

    server_utils$bind_events(
      ids = c("Study"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    output$Study <- shiny::renderUI({
      shiny::validate(
        shiny::need(input$Feature != "", "")
      )

      choices <- r6()$Studies

      selected <- ifelse(nrow(choices) == 1, choices, character(0))

      prettyRadioButtonsFieldSet(
        input_id = ns("Study"),
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

    shiny::observeEvent(c(input$Study), {

      shiny::validate(
        shiny::need(!is.null(input$Study), ""),
        shiny::need(input$Study != "", "")
      )

      shinyjs::removeClass(
        class = "refresh-btn",
        selector = paste0("#", ns("getData"))
      )
      shinyjs::addClass(
        class = "refresh-ready-btn",
        selector = paste0("#", ns("getData"))
      )

      if (r6()$namespace == "Comorbidity" & is.null(r6()$Conditions)) {
        shinyjs::disable(
          selector = paste0("#", ns("getData"))
        )
      } else {
        shinyjs::enable(
          selector = paste0("#", ns("getData"))
        )
      }

    }, ignoreInit = TRUE)

    karyotypes <- shiny::reactive({
      r6()$Karyotypes
    }) |>
      shiny::bindEvent(c(input$Study), ignoreNULL = TRUE)

    output$Karyotype <- shiny::renderUI({

      karyotype_choices <- karyotypes()

      input <- shinyWidgets::prettyRadioButtons(
        inputId = ns("Karyotype"),
        label = NULL,
        choiceNames = lapply(karyotype_choices$choiceNames, shiny::HTML),
        choiceValues = karyotype_choices$choiceValues,
        inline = FALSE,
        width = "90%"
      )

      if (nrow(karyotype_choices) == 1) {
        shinyjs::disabled(
          input
        )
      } else {
        input |>
          shiny::tagAppendAttributes(
            class = r6()$addInputSpecialClass("Karyotype", "disabled")
          )
      }

    })

    sexes <- shiny::reactive({
      r6()$Sexes
    }) |>
      shiny::bindEvent(c(input$Study), ignoreNULL = TRUE)

    output$Sex <- shiny::renderUI({

      shinyWidgets::awesomeCheckboxGroup(
        inputId = ns("Sex"),
        label = NULL,
        choices = sexes(),
        selected = sexes(),
        inline = TRUE,
        width = "90%"
      ) |>
      shiny::tagAppendAttributes(
        class = r6()$addInputSpecialClass("Sex", "disabled")
      )

    })

    ages <- shiny::reactive({
      r6()$Ages
    }) |>
      shiny::bindEvent(c(input$Study), ignoreNULL = TRUE)

    output$Age <- shiny::renderUI({

      shinyWidgets::numericRangeInput(
        inputId = ns("Age"),
        label = "Age range",
        value = ages(),
        width = "90%"
      )

    })

    # inputs_conditions_feature_analysis$server(
    #   id = "conditions",
    #   r6 = r6,
    #   parent = session
    # )

    output$StatTest <- shiny::renderUI({

      shinyWidgets::prettyRadioButtons(
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

    output$Covariates <- shiny::renderUI({
      shiny::validate(
        shiny::need(input$StatTest != "", "")
      )
      if (input$StatTest == "Linear Model") {

        choices <- r6()$CovariateChoices

        shiny::tagList(
          shinyWidgets::awesomeCheckboxGroup(
            inputId = ns("Covariates"),
            label = NULL,
            choices = choices,
            selected = choices,
            inline = TRUE
          )
        )
      } else {
        shiny::tagList(

        )
      }
    })

    output$AdjustmentMethod <- shiny::renderUI({

      shinyWidgets::prettyRadioButtons(
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

    StudyData <- shiny::reactive({
      shiny::validate(
        shiny::need(input$getData > 0, ""),
        shiny::need(input$Study != "", ""),
        shiny::need(input$Karyotype != "", ""),
        shiny::need(input$Sex != "", ""),
        shiny::need(input$Age[1] != "", ""),
        shiny::need(input$Age[2] != "", "")
        #shiny::need(r6()$validate_study_data(), "")
      )

      shinybusy::show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = glue::glue("Fetching {input$Study} data...")
        )

      data <- r6()$get_study_data(
        study = input$Study,
        karyotypes = input$Karyotype,
        sexes = input$Sex,
        ages = input$Age
      )

      shinybusy::remove_modal_spinner()

      data

    }) |>
      shiny::bindEvent(input$getData, ignoreInit = TRUE)

    return(
      list(
        feature = shiny::reactive(input$Feature),
        study = shiny::reactive(input$Study),
        study_data = StudyData,
        stat_test = shiny::reactive(input$StatTest),
        covariates = shiny::reactive(input$Covariates),
        adjustment_method = shiny::reactive(input$AdjustmentMethod)
      )
    )

  })
}
