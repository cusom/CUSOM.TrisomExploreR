box::use(
  shiny[tags, bindEvent]
)

box::use(
  app/logic/helpers/server_utils,
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
server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    server_utils$bind_events(
      ids = c("Study",
        "Karyotype",
        "Sex",
        "Age",
        "StatTest",
        "Covariates",
        "AdjustmentMethod"
      ),
      r6 = r6,
      session = session,
      parent_input = input
    )

    output$Study <- shiny::renderUI({

      choices <- r6$Studies #r6$getStudies()

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

      if (r6$namespace == "Comorbidity" & is.null(r6$Conditions)) {
        shinyjs::disable(
          selector = paste0("#", ns("getData"))
        )
      } else {
        shinyjs::enable(
          selector = paste0("#", ns("getData"))
        )
      }

      #gargoyle::trigger("validate_GSEA", session = session)

    }, ignoreInit = TRUE)

    output$Karyotype <- shiny::renderUI({

      shiny::validate(
        shiny::need(!is.null(input$Study), ""),
        shiny::need(input$Study != "", "")
      )
      
      karyotype_choices <- r6$Karyotypes #r6$getKaryotypeChoices()

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
            class = r6$addInputSpecialClass("Karyotype", "disabled")
          )
      }

    })

    output$Sex <- shiny::renderUI({

      shinyWidgets::awesomeCheckboxGroup(
        inputId = ns("Sex"),
        label = NULL,
        choices = r6$Sexes,#r6$getSexes(),
        selected = r6$Sexes, #r6$getSexes(),
        inline = TRUE,
        width = "90%"
      ) |>
      shiny::tagAppendAttributes(
        class = r6$addInputSpecialClass("Sex", "disabled")
      )

    })

    output$Age <- shiny::renderUI({

      shinyWidgets::numericRangeInput(
        inputId = ns("Age"),
        label = "Age range",
        value = r6$Ages, #r6$getAgeRange(),
        width = "90%"
      )

    })

    inputs_conditions_feature_analysis$server(
      id = "conditions",
      r6 = r6,
      parent = session
    )

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
        choiceNames = r6$StatTestNames,
        choiceValues = r6$StatTestValues
      )

    })

    output$Covariates <- shiny::renderUI({
      shiny::validate(
        shiny::need(input$StatTest != "", "")
      )
      if (input$StatTest == "Linear Model") {

        choices <- r6$CovariateChoices #r6$getCovariateChoices()

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
        choiceNames = r6$AdjustmentMethodNames,
        choiceValues = r6$AdjustmentMethodValues
      )

    })

    # Study <- shiny::eventReactive(c(input$Study), {
    #   input$Study
    # })

    StudyData <- shiny::reactive({#shiny::eventReactive(c(input$getData), {
      shiny::validate(
        shiny::need(input$getData > 0, ""),
        shiny::need(r6$validate_study_data(), "")
      )

      shinybusy::show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = glue::glue("Fetching {input$Study} data...")
        )

      data <- r6$get_study_data()

      shinybusy::remove_modal_spinner()

      data

    }) |>
      shiny::bindEvent(input$getData, ignoreInit = TRUE)

    # StatisticalParameters <- shiny::reactive({#shiny::eventReactive(c(input$getData), {
    #   # shiny::validate(
    #   #   shiny::need(input$getData > 0, "")
    #   # )
    #   browser()
    #   r6$get_selected_statistical_parameters()

    # }) |>
    #   shiny::bindEvent(input$getData, ignoreInit = TRUE)

    return(
      list(
        Study = shiny::reactive(input$Study),
        StudyData = StudyData,
        #StatisticalParameters = list(
        StatTest = shiny::reactive(input$StatTest),
        Covariates = shiny::reactive(input$Covariates),
        AdjustmentMethod = shiny::reactive(input$AdjustmentMethod)
        #)#shiny::reactive(r6$get_selected_statistical_parameters())
      )
    )

  })
}
