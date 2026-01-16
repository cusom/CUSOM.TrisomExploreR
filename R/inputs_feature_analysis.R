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
        tags$b("Analysis"),
        shiny::tags$div(
          id = ns("Analyses"),
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("Analysis")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$b("Studies"),
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
        TrisomExploreR::conditions_feature_analysis_inputs_ui(ns("conditions")),
        tags$b("Sex"),
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
        tags$br(),
        tags$b("Adjust for covariates"),
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

    output$Analysis <- shiny::renderUI({

      choices <- r6$getAnalyses()

      shiny::selectizeInput(
        inputId = ns("Analysis"),
        label = "",
        choices = choices,
        multiple = FALSE,
        options = list(
          placeholder = "Select Analysis",
          onInitialize = I('function() { this.setValue(""); }'),
          closeAfterSelect = TRUE,
          selectOnTab = TRUE,
          persist = FALSE,
          `live-search` = TRUE,
          dropupAuto = FALSE
        )
      ) |>
        bsplus::bs_embed_tooltip(
          title = "Select an Analysis below",
          placement = "top",
          html = TRUE
        )
    })

    shiny::observeEvent(c(input$Analysis), {
      r6$analysisVariable <- input$Analysis
    }, ignoreInit = TRUE)

    Studies <- shiny::reactive({
      shiny::validate(
        shiny::need(input$Analysis != "", "")
      )
      r6$getStudies()
    }) |>
    shiny::bindEvent(input$Analysis, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$Study <- shiny::renderUI({

      choices <- Studies()

      selected <- ifelse(nrow(choices) == 1, choices, character(0))

      CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
        inputId = ns("Study"),
        label = NULL,
        fieldSetData = choices,
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

      gargoyle::trigger("validate_GSEA", session = session)

    }, ignoreInit = TRUE)

    output$Karyotype <- shiny::renderUI({

      shiny::validate(
        shiny::need(!is.null(input$Study), ""),
        shiny::need(input$Study != "", "")
      )

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

    output$Sex <- shiny::renderUI({

      shinyWidgets::awesomeCheckboxGroup(
        inputId = ns("Sex"),
        label = NULL,
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

    shiny::observeEvent(c(input$getData), {

      shiny::validate(
        shiny::need(input$getData > 0, "")
      )

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
