#' @export
precalc_feature_analysis_inputs_ui <- function(id, input_config) {
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
        style = "height:570px;padding-left:10px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        shinyjs::hidden(
          div(
            id = ns("Studies"),
            CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
              inputId = ns("Study"),
              label = "",
              fieldSetData = input_config$studiesTibble,
              selected = input_config$studiesTibble[1,]
            )
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
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("StatTest"),
            label = "Statistical test",
            choices = NULL,
            selected = NULL,
            status = "primary",
            icon = NULL,
            inline = FALSE,
            width = "90%",
            choiceNames = input_config$statTestschoiceNames[1],
            choiceValues = input_config$statTests[1]
          )
        ),
        shinycustomloader::withLoader(
          uiOutput(ns("Covariates")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("AdjustmentMethod"),
            label = "Multiple hypothesis correction",
            choices = NULL,
            selected = NULL,
            status = "primary",
            icon = NULL,
            inline = FALSE,
            width = "90%",
            choiceNames = input_config$adjustmentMethodsNames[1],
            choiceValues = input_config$adjustmentMethods[1]
          )
        )
      ),
      footer = shiny::tagList(
        shiny::actionButton(
          inputId = ns("getData"),
          label = "Analyze & Plot",
          class = "refresh-ready-btn",
          icon = icon("play")
        ),
        tags$br(),
        #,shinyjs::hidden(
        tags$div(
          id = ns("GSEA"),
          tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
          tags$br(),
          tags$p(style="font-size:14px;font-weight: 700;font-style: italic;text-align: center", "Gene Set Enrichment Analysis"),
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("RunGSEA"),
              label = "Run GSEA",
              width = "90%",
              class = "refresh-ready-btn",
              icon = icon("chart-bar")
            )
          ),
          shinyBS::bsTooltip(
            id = ns("RunGSEA"),
            title = "Click here to see Gene Set Enrichment Analysis (GSEA)",
            placement = "top",
            trigger = "hover"
          )
        )
        #)
      )
    )
  )

}

#' @export
precalc_feature_analysis_inputs_server <- function(id, r6, input_config) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c("Study","Conditions","Karyotype","Sex","Age","StatTest","Covariates","AdjustmentMethod"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    output$Karyotype <- renderUI({

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

      shinyjs::enable("RunGSEA")

      gargoyle::trigger("enable_GSEA", session = session)

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

    observeEvent(c(input$RunGSEA),{

      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = glue::glue("Calculating GSEA...")
      )

      r6$getGSEAData()

      shinybusy::remove_modal_spinner()

      gargoyle::trigger("run_GSEA", session = session)

    }, ignoreInit = TRUE)

  })
}