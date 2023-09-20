#' Create standard inputs widgets for TrisomExploreR cell type analysis
#' @param id - string - id for this module namespace
#' @param input_config list - list of default values for various input widgets
#' @importFrom shinydashboardPlus box
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom bsplus bs_embed_tooltip
#' @return ui module
#' @export
cell_type_inputs_ui <- function(id, input_config) {
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
            data-original-title="Set options below to generate plot">
          </span>
        </div>'
      ),
      height = "auto",
      width = 12,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shiny::tags$div(
        id = ns("scrollableOptions"),
        style = "height:630px;padding-left:10px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("Platform"),
            label = "Platform",
            choices = sort(input_config$platforms),
            selected = input_config$platforms[1],
            width = "90%"
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("CellType")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
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
      shinyWidgets::pickerInput(
        inputId = ns("CellType"),
        label = "Cell Type(s)",
        choices = r6$getCellTypes(),
        selected = r6$getCellTypes(),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      )
    })

    output$Analyte <- shiny::renderUI({
      shiny::selectizeInput(
        inputId = ns("Analyte"),
        label = "Gene",
        choices = r6$getAnalytes(),
        options = list(
          labelField = "name",
          searchField = "name",
          valueField = "name",
          placeholder = "Search for Gene",
          onInitialize = I('function() { this.setValue(""); }'),
          closeAfterSelect = TRUE,
          selectOnTab = TRUE,
          persist = FALSE,
          `live-search` = TRUE,
          onType = I(paste0("
            function (str) {
              if(this.currentResults.total == 0) {
                Shiny.setInputValue(
                  '", ns("analyteSearchResults"), "',
                  {
                    query: this.currentResults.query,
                    total: this.currentResults.total
                  },
                  { priority: 'event' }
                );
              };
            }"))
        )
      )
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
        shinyjs::runjs(
            paste0("
              Shiny.setInputValue(
                '", ns("analyteSearchResults"), "',
                {
                  query: '", input$Analyte, "',
                  total: ", length(input$Analyte), "
                },
                { priority: 'event' }
              );"
            )
          )
      }

    },  domain = session)

    ## when a gene is chosen -- trigger the plot
    shiny::observeEvent(c(input$Refresh), {
      if (input$Analyte != "") {
        gargoyle::trigger("render_analyte_plot", session = session)
      }
    }, ignoreInit = TRUE)

    analyte_search_error_text <- shiny::eventReactive(
      c(input$analyteSearchResults, input$Analyte), {

      search_result_data <- input$analyteSearchResults
      shiny::req(search_result_data)

      if (length(input$Analyte) > 0 & input$Analyte != "") {
        shiny::HTML("")
      } else if (search_result_data$total == 0) {
        shiny::HTML(paste0('<span style="color:black;font-size:smaller;padding-left:10px;"><b>"',
        search_result_data$query,
        '"</b> not found. Please try another value</span>')
        )
      } else {
        shiny::HTML("")
      }
    }, domain = session, ignoreInit = TRUE)

    output$AnalyteSearchError <- shiny::renderUI({
      analyte_search_error_text()
    })

  })

}
