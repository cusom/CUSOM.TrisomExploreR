#' Create input widgets for TrisomExploreR feature analysis
#' @param id - string - id for this module namespace
#' @param input_config - list - list of default values for various input widgets
#' @importFrom shinydashboardPlus box
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @import shinyjs
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom CUSOMShinyHelpers prettyRadioButtonsFieldSet
#' @importFrom shinycustomloader withLoader
#' @export
feature_analysis_inputs_ui <- function(id, input_config) {
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
          CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
            inputId = ns("Study"),
            label = "",
            fieldSetData = input_config$studiesTibble,
            selected = character(0)
          ) |>
          bsplus::bs_embed_tooltip(
            title = "Select a study below",
            placement = "top",
            html = TRUE
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
        TrisomExploreR::conditions_feature_analysis_inputs_ui(ns("conditions")),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("Sex")),
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
          shiny::uiOutput(ns("Covariates")),
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

#' Server-side logic / processing for TrisomExploreR feature analysis inputs
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic for inputs
#' @param input_config - list - list of default values for various input widgets to be used server-side
#' @import shinyjs
#' @importFrom gargoyle trigger
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @export
feature_analysis_inputs_server <- function(id, r6, input_config) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
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
      }
      else {
        shinyjs::enable(
          selector = paste0("#", ns("getData"))
        )
      }

      gargoyle::trigger("validate_GSEA", session = session)

    }, ignoreInit = TRUE)

    output$Karyotype <- shiny::renderUI({

      shiny::validate(
        shiny::need(!is.null(input$Study), ""),
        shiny::need(input$Study != "", "")
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

    output$Sex <- shiny::renderUI({

      shinyWidgets::awesomeCheckboxGroup(
        inputId = ns("Sex"),
        label = "Sex",
        choices = input_config$sexes,
        selected = input_config$sexes,
        inline = TRUE,
        width = "90%"
      ) |>
      shiny::tagAppendAttributes(
        class = r6$addInputSpecialClass("Sex", "disabled")
        )

    })

    TrisomExploreR::conditions_feature_analysis_inputs_server(
      id = "conditions",
      r6 = r6,
      input_config = input_config,
      parent = session
    )

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
        shiny::tagList(

        )
      }
    })

    shiny::observeEvent(c(input$getData), {

      shiny::validate(
        shiny::need(input$getData > 0, "")
      )

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
