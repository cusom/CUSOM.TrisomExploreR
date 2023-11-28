#' Create standard inputs widgets for TrisomExploreR cell type analysis
#' @param id - string - id for this module namespace
#' @param input_config list - list of default values for various input widgets
#' @importFrom shinydashboardPlus box
#' @importFrom shinyWidgets prettyRadioButtons pickerInput awesomeCheckboxGroup numericRangeInput
#' @importFrom bsplus bs_embed_tooltip
#' @return ui module
#' @export
cell_type_inputs_ui <- function(id, input_config) {
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
            data-original-title = \"Set options below to generate plot\">
          </span>
        </div>"
      ),
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shiny::tags$div(
        id = ns("scrollableOptions"),
        style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("Platform"),
            label = "Platform",
            choices = sort(input_config$platforms),
            selected = input_config$platforms[1],
            width = "90%"
          )
        ),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$b("Cell Type(s)"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("CellType")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        tags$b("Gene"),
        shiny::tags$div(
          id = "AnalyteInput",
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("Analyte")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
        shiny::htmlOutput(ns("AnalyteSearchError")),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("Sex"),
          label = "Sex",
          choices = input_config$sexes,
          selected = input_config$sexes,
          inline = TRUE,
          width = "90%"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::numericRangeInput(
          inputId =  ns("Age"),
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
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;")
      ),
      footer = shiny::tagList(
        shiny::actionButton(
          ns("Refresh"),
          label = "Analyze & Plot",
          class = "refresh-btn",
          icon = shiny::icon("play")
        )
      )
    )
  )
}

#' Server-side logic / processing for TrisomExploreR cell type analysis inputs
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic for inputs
#' @import shinyjs
#' @importFrom gargoyle trigger
#' @importFrom promises future_promise then %...>% %...!%
#' @importFrom shinyWidgets pickerInput pickerOptions updatePickerInput
#' @export
cell_type_inputs_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c(
        "Platform",
        "CellType",
        "Sex",
        "Age",
        "StatTest",
        "Covariates",
        "AdjustmentMethod",
        "Analyte"
      ),
      r6 = r6,
      session = session,
      parent_input = input
    )

    output$CellType <- shiny::renderUI({

      promises::future_promise(
        r6$getCellTypes()
      )  %...!% warning() %...>%
      {
        shinyWidgets::pickerInput(
          inputId = ns("CellType"),
          label = NULL,
          choices = .,
          selected = .,
          options = list(
            `actions-box` = TRUE
          ),
          multiple = TRUE
        )
      }
    })

    output$Analyte <- shiny::renderUI({

      promises::future_promise(
        r6$getAnalytes()
      )  %...!% warning() %...>%
        {
          shinyWidgets::pickerInput(
            inputId = ns("Analyte"),
            label = NULL,
            choices = .,
            selected = character(0),
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              maxOptions = 1
            ),
            multiple = TRUE
          )
        }
    })

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

    shiny::observeEvent(c(input$Analyte), {

      if (input$Analyte == "") {
        shinyjs::removeClass(id = "Refresh", class = "refresh-ready-btn")
        shinyjs::addClass(id = "Refresh", class = "refresh-btn")
        shinyjs::disable(id = "Refresh")
      } else {
        shinyjs::removeClass(id = "Refresh", class = "refresh-btn")
        shinyjs::addClass(id = "Refresh", class = "refresh-ready-btn")
        shinyjs::enable(id = "Refresh")
      }

    }, ignoreInit = TRUE, domain = session)

    ## when a gene is chosen -- trigger the plot
    shiny::observeEvent(c(input$Refresh), {
      if (input$Analyte != "") {
        gargoyle::trigger("render_analyte_plot", session = session)
      }
    }, ignoreInit = TRUE)

  })

}
