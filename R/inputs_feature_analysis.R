#' @export
feature_analysis_inputs_ui <- function(id, input_config) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      title = shiny::HTML(
        '<div class="dataset-options-title">Dataset Options
          <span
            data-toggle="tooltip"
            data-placement="auto right"
            title = ""
            class = "fas fa-filter"
            data-original-title="Set options below to generate volcano plot">
          </span>
        </div>'
      ),
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinyjs::disabled(
        bs4Dash::tooltip(
          shiny::actionButton(
            ns("PrimaryTutorial"),
            label = "Take Tutorial",
            class = "tutorial-btn",
            icon = icon("question-circle")
          ),
          title = "Click here to learn about setting dataset options to generate the volcano plot",
          placement = "top"
        )
      ),     
      shiny::tags$div(
        id = NS(id, "scrollableOptions"),
        style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shiny::tags$div(
          id = ns("Studies"),
          bs4Dash::tooltip(
            CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
              inputId = ns("Study"),
              label = "",
              fieldSetData = input_config$studiesTibble,
              selected = character(0) #input_config$studiesTibble[1,]
            ),
            title = "Select a study below",
            placement = "top"
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinycustomloader::withLoader(
          uiOutput(ns("Karyotype")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        uiOutput(ns("ConditionsInputs")),
        shinycustomloader::withLoader(
          uiOutput(ns("Sex")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::numericRangeInput(
          inputId = ns("Age"),
          label = "Age range",
          value = c(min(input_config$ages), max(input_config$ages)),
          width = "90%"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("StatTest"),
          label = "Statistical test",
          choices = NULL,
          selected = NULL,
          status = "primary",
          icon = NULL,
          inline = FALSE,
          width = "90%",
          choiceNames = input_config$statTestschoiceNames,
          choiceValues = input_config$statTests
        ),
        shinycustomloader::withLoader(
          uiOutput(ns("Covariates")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("AdjustmentMethod"),
          label = "Multiple hypothesis correction",
          choices = NULL,
          selected = NULL,
          status = "primary",
          icon = NULL,
          inline = FALSE,
          width = "90%",
          choiceNames = input_config$adjustmentMethodsNames,
          choiceValues = input_config$adjustmentMethods
        )
      ),
      footer = shiny::tagList(
        #shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shiny::actionButton(
          ns("getData"),
          label = "Analyze & Plot",
          class = "refresh-ready-btn",
          icon = icon("play")
        )
      )
    )
  )

}

#' @export
feature_analysis_inputs_server <- function(id, r6, input_config) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c("Study",
      "Conditions",
      "Karyotype",
      "Sex","Age",
      "StatTest",
      "Covariates",
      "AdjustmentMethod"
      ),
      r6 = r6,
      session = session,
      parent_input = input
    )

    observeEvent(c(input$Study),{
      validate(
        need(!is.null(input$Study), ""),
        need(input$Study != "", "")
      )

      gargoyle::trigger("validate_GSEA", session = session)

    }, ignoreInit = TRUE)

    output$Karyotype <- renderUI({

      validate(
        need(!is.null(input$Study), ""),
        need(input$Study != "", "")
      )

      karyotypeChoices <- r6$getKaryotypeChoices(input_config$karyotypes)

      input <- shinyWidgets::prettyRadioButtons(
        inputId = ns("Karyotype"),
        label = "Karyotype",
        choiceNames = lapply(karyotypeChoices$choiceNames, shiny::HTML),
        choiceValues = karyotypeChoices$choiceValues,
        inline = FALSE,
        width = "90%"
      )

      if(nrow(karyotypeChoices) == 1) {
        shinyjs::disabled(
          input
        )
      } else {
        input |>
          shiny::tagAppendAttributes(class = r6$addInputSpecialClass("Karyotype","disabled"))
      }

    })

    output$Sex <- renderUI({

      shinyWidgets::awesomeCheckboxGroup(
        inputId = ns("Sex"),
        label = "Sex",
        choices = input_config$sexes,
        selected = input_config$sexes,
        inline = TRUE,
        width = "90%"
      ) |>
      shiny::tagAppendAttributes(class = r6$addInputSpecialClass("Sex","disabled"))

    })

    output$ConditionsInputs <- renderUI({

      if(r6$namespace  == "Comorbidity") {
        shiny::tagList(
          actionButton(
            inputId = ns("SetConditions"),
            label = "Set Conditions",
            icon = icon("file-medical"),
            width = "99%"
          )
        )
      }
      else {
        shiny::tagList(
        )
      }

    })

    observeEvent(c(input$SetConditions),{

      validate(
        need(input$SetConditions > 0, "")
      )

      shiny::showModal(
        shiny::modalDialog(
          title = htmltools::tags$h3(glue::glue("Set Co-Occuring Conditions:")),
          size = "l",
          easyClose = TRUE,
          list(
            shiny::tagList(
              shiny::tags$b("Search for Co-Occuring Conditions")
              ,shiny::tags$br()
              ,shiny::tags$div(
                id = ns("Conditions-Picker"),
                  shinyTree::shinyTree(
                    outputId = ns("Conditions"),
                    search = TRUE,
                    multiple = TRUE,
                    theme = "proton",
                    themeIcons = FALSE,
                    themeDots = FALSE,
                    checkbox = TRUE
                  )
              )
              ,shiny::tags$br()
              ,shinyWidgets::actionBttn(
                inputId = ns("ConditionsReset"),
                label = "Reset Selected Conditions",
                icon = icon("undo"),
                style = "minimal",
                size = "xs",
                color = "primary",
                block = TRUE
              )
            )
          ),
          footer = shiny::tagList(
            shiny::modalButton(label = "Close")
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(c(input$ConditionsReset),{

      shinyjs::reset("ConditionsInputs")
      shinyjs::runjs(paste0("$('#",ns("Conditions"),"').jstree('deselect_all');"))

    }, ignoreInit = TRUE)

    conditions <- eventReactive(c(input$SetConditions),{
      input_config$ConditionChoices |>
        dplyr::select(ConditionClass, Condition)
    })

    output$Conditions <- shinyTree::renderTree({
       r6$getConditionTree(conditions())
    })

    output$Covariates <- renderUI({

      if(input$StatTest == "Linear Model") {

        choices <- r6$getCovariateChoices()

        shiny::tagList(
          shiny::tags$br(),
          shinyWidgets::awesomeCheckboxGroup(
            inputId =  ns("Covariates"),
            label = "Adjust for covariates",
            choices = choices,
            selected = choices,
            inline = TRUE
          )
        )
      }
      else {
        shiny::tagList(

        )
      }
    })

    observeEvent(c(input$getData),{

      validate(
        need(input$getData > 0, "")
      )

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
