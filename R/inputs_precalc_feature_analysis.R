#' Create input widgets for TrisomExploreR precalculated feature analysis - DESeq2
#' @param id - string - id for this module namespace
#' @param input_config - list - list of default values for various input widgets
#' @importFrom shinydashboard tabBox
#' @import shinyjs
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom CUSOMShinyHelpers prettyRadioButtonsFieldSet
#' @importFrom shinycustomloader withLoader
#' @export
precalc_feature_analysis_inputs_ui <- function(id, input_config) {
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
        shiny::selectizeInput(
          inputId = ns("CellType"),
          label = "Cell Type",
          choices = input_config$CellTypes,
          selected = NULL,
          multiple = FALSE,
          options = list(
            placeholder = "Choose Cell Type",
            onInitialize = I('function() { this.setValue(""); }'),
            closeAfterSelect = TRUE,
            selectOnTab = TRUE,
            persist = FALSE,
            `live-search` = TRUE,
            dropupAuto = FALSE
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
        tags$b("Adjust for covariates"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("Covariates")),
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
          icon = shiny::icon("play")
        )
      )
    )
  )

}

#' Server-side logic / processing for TrisomExploreR precalculated feature analysis inputs
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @param input_config - list - list of default values for various input widgets to be used server-side
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom gargoyle trigger
#' @export
precalc_feature_analysis_inputs_server <- function(id, r6, input_config) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c(
        "Study",
        "Conditions",
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

      choices <- r6$getStudies()

      selected <- ifelse(nrow(choices) == 1, choices, character(0))

      CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
        inputId = ns("Study"),
        label = "",
        fieldSetData = choices,
        selected = selected
      ) |>
        bsplus::bs_embed_tooltip(
          title = "Select a study below",
          placement = "top",
          html = TRUE
        )

    })

    output$Karyotype <- shiny::renderUI({

      karyotype_choices <- r6$getKaryotypeChoices(input_config$karyotypes)

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

    output$Covariates <- shiny::renderUI({

      if (input$StatTest == "Linear Model") {

        choices <- r6$getCovariateChoices()

        shiny::tagList(
          shiny::tags$br(),
          shinyWidgets::awesomeCheckboxGroup(
            inputId = ns("Covariates"),
            label = NULL,
            choices = choices,
            selected = choices,
            inline = TRUE
          )
        )
      } else {
        shiny::tagList()
      }

    })

    shiny::observeEvent(c(input$getData), {

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
