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
        shinyjs::hidden(
          shiny::tags$div(
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

    output$Karyotype <- shiny::renderUI({

      karyotypeChoices <- r6$getKaryotypeChoices(input_config$karyotypes)

      input <- shinyWidgets::prettyRadioButtons(
        inputId = ns("Karyotype"),
        label = "Karyotype",
        choiceNames = lapply(karyotypeChoices$choiceNames, shiny::HTML),
        choiceValues = karyotypeChoices$choiceValues,
        inline = FALSE,
        width = "90%"
      )

      if (nrow(karyotypeChoices) == 1) {
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
            label = "Adjust for covariates",
            choices = choices,
            selected = choices,
            inline = TRUE
          )
        )
      }
      else {
        shiny::tagList()
      }

    })

    shiny::observeEvent(c(input$getData), {

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
