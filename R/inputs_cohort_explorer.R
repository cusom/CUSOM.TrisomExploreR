#' Create input wigets for TrisomExploreR cohort explorer
#' @param id - string - id for this module namespace
#' @param input_config - list - list of default values for various input widgets
#' @importFrom shinydashboardPlus box
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @export
cohort_explorer_inputs_ui <- function(id, input_config) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12, class = "col-lg-12",
        shinydashboardPlus::box(
          id = ns("CohortFilters"),
          title = shiny::HTML(
            "<div class=\"dataset-options-title\">Human Trisome Project Cohort Overview
              <span
                data-toggle=\"tooltip\"
                data-placement=\"auto right\"
                title=\"\"
                class=\"fas fa-filter\"
                data-original-title=\"Set options below\">
              </span>
            </div>"
          ),
          height = "auto",
          width = NULL,
          closable = FALSE,
          solidHeader = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          shiny::column(
            width = 12, class = "col-lg-2",
            shinyWidgets::awesomeCheckboxGroup(
              inputId = ns("Karyotypes"),
              label = "Karyotype",
              choices = input_config$karyotypes,
              selected = input_config$karyotypes,
              inline = TRUE
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-2",
            shinyWidgets::awesomeCheckboxGroup(
              inputId = ns("Sex"),
              label = "Sex",
              choices = input_config$sexes,
              selected = input_config$sexes,
              inline = TRUE
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-2",
            shinyWidgets::numericRangeInput(
              inputId =  ns("Age"),
              label = "Age range",
              value = input_config$ages,
              width = "90%"
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-2",
            shinyWidgets::pickerInput(
              inputId = ns("Samples"),
              label = "Samples Available",
              choices = sort(input_config$SamplesAvailable),
              selected = input_config$SamplesAvailable,
              multiple = TRUE
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-2",
            shinyWidgets::pickerInput(
              inputId = ns("OmicsSamples"),
              label = "Omics Analyses",
              choices = input_config$OmicsSamplesAvailable,
              selected = input_config$OmicsSamplesAvailable,
              multiple = TRUE
            )
          )
        )
      )
    )
  )
}

#' Server-side logic / processing for TrisomExploreR cohort explorer inpuots
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @param input_config - list - list of default values for various input widgets to be used server-side
#' @importFrom gargoyle trigger
#' @export
cohort_explorer_inputs_server <- function(id, r6, input_config) {

  shiny::moduleServer(id, function(input, output, session) {

     ns <- session

     TrisomExploreR::bind_events(
       ids = c("Karyotypes", "Sex", "Age", "Samples", "OmicsSamples"),
       r6 = r6,
       session = session,
       parent_input = input
     )

     shiny::observeEvent(
      c(input$Karyotypes, input$Sex, input$Age, input$Samples, input$OmicsSamples), {
       r6$getParticipantData()
       gargoyle::trigger("get_cohort_data")
     })

  })
}
