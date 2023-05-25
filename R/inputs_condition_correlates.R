#' Create input widgets for TrisomExploreR condition correlates explorer
#' @param id - string - id for this module namespace
#' @param input_config - list - list of default values for various input widgets
#' @importFrom shinydashboard tabBox
#' @import bsplus
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @export
condition_correlates_inputs_ui <- function(id, input_config) {
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
      shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
      bsplus::bs_accordion(id = ns("AccordionInputs")) |>
        bsplus::bs_set_opts(panel_type = "default", use_heading_link = TRUE) |>
        bsplus::bs_append(
          title = "1) Choose Platform",
          content = list(
            shinyWidgets::prettyRadioButtons(
              inputId = ns("Platform"),
              label = "Platforms",
              choices = sort(unique(input_config$PlatformExperiments$PlatformDisplayName)),
              selected = character(0),
              status = "primary",
              icon = NULL,
              inline = FALSE
            )
          )
        ) |>
        bsplus::bs_append(
          title = "2) Choose Experiment",
          content = list(
            shinyWidgets::prettyRadioButtons(
              inputId = ns("Experiment"),
              label = "Experiments",
              choices = NULL,
              selected = character(0),
              status = "primary",
              icon = NULL,
              inline = FALSE
            )
          )
        ) |>
        bsplus::bs_append(
          title = "3) Choose Query Analyte",
          content = list(
            shiny::selectizeInput(
              inputId = ns("QueryAnalyte"),
              label = "Analytes",
              width = "300px",
              choices = NULL,
              multiple = FALSE,
              options = list(
                labelField = "name",
                searchField = "name",
                valueField = "id",
                placeholder = "Choose an analyte",
                onInitialize = I('function() { this.setValue(""); }'),
                closeAfterSelect = TRUE,
                selectOnTab = TRUE,
                persist = FALSE,
                `live-search` = TRUE,
                dropupAuto = FALSE,
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
                }"
                ))
              )
            )
          )
        ) |>
        bsplus::bs_append(
          title = "4) Set Analysis Options (optional)",
          content = list(
            shinyjs::disabled(
              shinyWidgets::prettyRadioButtons(
                inputId = ns("Karyotype"),
                label = "Karyotype",
                choices = "Trisomy 21",
                selected = "Trisomy 21"
              )
            ),
            shiny::tags$br(),
            shiny::sliderInput(
              inputId = ns("MinComorbitityMembership"),
              label = "Minimum # of Participants for Co-occuring Condition",
              value = 3,
              min = 2,
              max = 10,
              step = 1,
              ticks = FALSE
            ),
            shiny::tags$br(),
            shinyWidgets::awesomeCheckboxGroup(
              inputId = ns("Sex"),
              label = "Sex",
              choices = input_config$sexes,
              selected = input_config$sexes,
              inline = TRUE
            ),
            shiny::tags$br(),
            shinyWidgets::numericRangeInput(
              inputId =  ns("Age"),
              label = "Age range",
              value = input_config$ages
            )
          )
        ) |>
        bsplus::bs_append(
          title = "5) Set Statistics (optional)",
          content = list(
            shinyWidgets::prettyRadioButtons(
              inputId = ns("StatTest"),
              label = "Statistical test",
              choices = NULL,
              selected = NULL,
              status = "primary",
              icon = NULL,
              inline = FALSE,
              choiceNames = input_config$statTestschoiceNames,
              choiceValues = input_config$statTests
            )
            , shiny::tags$br()
            , shiny::tags$div(
              id = ns("CovariateInput"),
              shinyWidgets::awesomeCheckboxGroup(
                inputId = ns("Covariates"),
                label = "Adjust for covariates",
                choices = c("Sex", "Age"),
                selected = c("Sex", "Age"),
                inline = TRUE
              )
            )
            , shiny::tags$br()
            , shinyWidgets::prettyRadioButtons(
                inputId = ns("AdjustmentMethod"),
                label = "Multiple hypothesis correction",
                choices = NULL,
                selected = NULL,
                status = "primary",
                icon = NULL,
                inline = FALSE,
                choiceNames = input_config$adjustmentMethodsNames,
                choiceValues = input_config$adjustmentMethods
              )
            )
          ),
          footer = shiny::tagList(
          shiny::actionButton(
            ns("getData"),
            label = "Analyze & Plot",
            class = "refresh-btn",
            icon = icon("play")
          )
        )
      )
    )
}

#' Server-side logic / processing for TrisomExploreR condition correlates explorer inputs
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @param input_config - list - list of default values for various input widgets to be used server-side
#' @import dplyr
#' @import tibble
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @import shinybusy
#' @import glue
#' @import shinyjs
#' @importFrom gargoyle trigger
#' @export
condition_correlates_inputs_server <- function(id, r6, input_config) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c(
        "Platform",
        "Experiment",
        "QueryAnalyte",
        "MinComorbitityMembership",
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

    shiny::observeEvent(c(input$Platform), {

      choices <- input_config$PlatformExperiments |>
        dplyr::filter(
          PlatformDisplayName == input$Platform,
          !is.na(ExperimentStudyName)
        ) |>
        dplyr::select(ExperimentStudyName, ExperimentID) |>
        dplyr::arrange(ExperimentStudyName) |>
        tibble::deframe()

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "Experiment",
        choices = choices,
        selected = character(0)
      )

      shinyjs::click(glue::glue("AccordionInputs-1-heading"), asis = FALSE)

    }, ignoreNULL = TRUE)

    shiny::observeEvent(c(input$Experiment), {

      shiny::validate(
        shiny::need(input$Experiment != "", "")
      )

      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = glue::glue("Getting Query Analytes for {input$Experiment}...")
      )

      queryAnalytes <- r6$getQueryAnalytes()

      shinybusy::remove_modal_spinner()

      updateSelectizeInput(
        session = session,
        inputId = "QueryAnalyte",
        choices = queryAnalytes
      )

      shinyjs::click(glue::glue("AccordionInputs-2-heading"), asis = FALSE)

    }, ignoreNULL = TRUE)

    shiny::observeEvent(c(input$QueryAnalyte), {
      if (input$QueryAnalyte != "") {
        shinyjs::click(glue::glue("AccordionInputs-3-heading"), asis = FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$getData), {
      gargoyle::trigger("get_volcano_data")
    }, ignoreInit = TRUE)

    shiny::observe({
      if (input$QueryAnalyte == "") {
        shinyjs::disable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-ready-btn")
        shinyjs::addClass(id = "getData", class = "refresh-btn")
      }
      else {
        shinyjs::enable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-btn")
        shinyjs::addClass(id = "getData", class = "refresh-ready-btn")
      }
    })

  })

}
