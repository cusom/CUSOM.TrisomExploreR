#' @export
immunemap_feature_analysis_inputs_ui <- function(id, input_config) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      title = shiny::HTML(
        '<div class="dataset-options-title">Dataset Options
          <span
            data-toggle="tooltip"
            data-placement="auto right"
            title=""
            class="fas fa-filter"
            data-original-title="Set options below to generate volcano plot">
          </span>
        </div>'
      ),
      height = "auto",
      width = 12,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shinyjs::disabled(
        shiny::actionButton(
          ns("PrimaryTutorial"),
          label = "Take Tutorial",
          class ="tutorial-btn",
          icon = icon("question-circle")
        )
      ),
      shinyBS::bsTooltip(
        id = ns("PrimaryTutorial"),
        title = "Click here to learn about setting dataset options to generate the volcano plot",
        placement = "top",
        trigger = "hover"
      ),
      shiny::tags$div(
        id = NS(id, "scrollableOptions"),
        style = "height:630px;padding-left:10px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        #style = "height: 70vh;overflow-y:auto",
        #shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shiny::tags$div(
          id = ns("Studies"),
          CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
            inputId = ns("Study"),
            label = "",
            fieldSetData = input_config$studiesTibble,
            selected = input_config$studiesTibble[1,]
          ),
          shinyBS::bsTooltip(
            id = ns("info_study"),
            title = "Select a study below",
            placement = "top",
            trigger = "hover",
            options = list(
              delay = list(
                show = 500,
                hide = 100
              )
            )
          )
        ),
        shiny::tags$br(),
        shinycustomloader::withLoader(
          uiOutput(ns("CellType")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shinyjs::hidden(
          selectizeInput(
            inputId = NS(id,"Analysis"),
            label = "Analysis",
            width = "90%",
            choices = NULL,
            selected = NULL
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
immunemap_feature_analysis_inputs_server <- function(id, r6, input_config) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c("Study","CellType","Analysis","Conditions","Karyotype","Sex","Age","StatTest","Covariates","AdjustmentMethod"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    CellTypeArgs <- reactive({
      r6$getCellTypes(input$Study)
    })

    output$CellType <- renderUI({

      widget <- selectizeInput(
        inputId = ns("CellType"),
        label = "Analysis Choices",
        width = "90%",
        choices = CellTypeArgs()$choices,
        selected = CellTypeArgs()$choices,
        multiple = CellTypeArgs()$MultipleSelect,
        options = list(
          placeholder = 'Please select below',
          closeAfterSelect = FALSE,
          selectOnTab = TRUE,
          persist = FALSE,
          `live-search` = TRUE,
          dropupAuto = FALSE
        )
      )

      if(CellTypeArgs()$HideCellType) {

        widget <- shinyjs::hidden(widget)

      }

      widget

    })

    observeEvent(c(input$CellType),{

      Analysis <- r6$getAnalysisInputChoices()

      updateSelectizeInput(
        inputId = "Analysis",
        choices = Analysis,
        selected = Analysis
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$Karyotype <- renderUI({

      validate(
        need(input$CellType != "",""),
        need(input$Analysis != "","")
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

      if(r6$namespace == "Comorbidity") {

        shiny::tagList(
          shiny::tags$b("Co-Occuring Conditions")
          ,shiny::tags$br()
          ,shiny::tags$div(
            id = ns("Conditions-Picker"),
            shinyWidgets::dropdown(
              shiny::tags$h4("Conditions:"),
              size = "md",
              width = "auto",
              tooltip = TRUE,
              label = "Select Conditions",
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
          ,shiny::tags$hr()
        )
      }

    })

    output$Conditions <- shinyTree::renderTree({

      input_config$ConditionChoices |>
        dplyr::select(ConditionClass, Condition) |>
        CUSOMShinyHelpers::dfToTree()

    })

    output$Covariates <- renderUI({

      if(input$StatTest == "Linear Model") {

        choices <- r6$getCovariateChoices()

        shiny::tagList(
          shiny::tags$br(),
          CUSOMShinyHelpers::createInputControl(
            controlType = "checkboxGroupInput",
            inputId = ns("Covariates"),
            label = "Adjust for covariates",
            choices = choices ,
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

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
