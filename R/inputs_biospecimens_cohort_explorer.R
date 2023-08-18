#' Create input widgets for TrisomExploreR biospecimen feature analysis
#' @param id - string - id for this module namespace
#' @export
biospecimens_cohort_explorer_inputs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      class = "form-group shiny-input-container",
      shiny::uiOutput(ns("BiospecimenInput"))
    )
  )
}

#' Server-side logic / processing for TrisomExploreR conditions feature analysis inputs
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic for inputs
#' @param input_config - list - list of default values for various input widgets to be used server-side
#' @param parent - session object - parent session
#' @import dplyr
#' @importFrom bsplus bs_modal bs_attach_modal
#' @import glue
#' @import shinyTree
#' @import shinyjs
#' @export
biospecimens_cohort_explorer_inputs_server <- function(id, r6, input_config, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c(
        "Biospecimens"
      ),
      r6 = r6,
      session = session,
      parent_input = input
    )

    output$BiospecimenInput <- shiny::renderUI({

      shiny::tagList(
        bsplus::bs_modal(
          id = ns("Biospecimen-Picker-Modal"),
          title = shiny::tags$h3(glue::glue("Biospecimens:")),
          size = "large",
          body = list(
            shiny::tagList(
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  class = "col-lg-6",
                  shiny::tags$b("Search/Select Biospecimens:"),
                  shiny::tags$div(
                    id = ns("Biospecimen-Picker"),
                    shinyTree::shinyTree(
                      outputId = ns("Biospecimens"),
                      search = TRUE,
                      multiple = TRUE,
                      theme = "proton",
                      themeIcons = FALSE,
                      themeDots = FALSE,
                      checkbox = TRUE
                    )
                  )
                ),
                shiny::column(
                  width = 12,
                  class = "col-lg-6",
                  tags$b("Selected Biospecimens"),
                  shiny::htmlOutput(
                    ns("selectedBiospecimens"),
                    placeholder = TRUE
                  )
                )
              ),
              shiny::tags$hr(),
              shinyWidgets::actionBttn(
                inputId = ns("BiospecimenReset"),
                label = "Reset Selected Biospecimens",
                icon = shiny::icon("undo"),
                style = "minimal",
                size = "xs",
                color = "primary",
                block = TRUE
              )
            )
          )
        ),
        shiny::tags$label(
          class = "control-label",
          `for` = "cohort-inputs-OmicsSamples",
          "Biospecimens"
        ),
        shiny::actionButton(
          inputId = ns("SetBiospecimens"),
          label = "Choose Biospecimens",
          icon = shiny::icon("vial-circle-check"),
          width = "99%",
          style = "padding-top:5px;"
        ) |>
        bsplus::bs_attach_modal(id_modal = ns("Biospecimen-Picker-Modal"))
      )

    })

    shiny::observeEvent(c(input$BiospecimenReset), {

      #shinyjs::reset("BiospecimenTree")
      shinyjs::runjs(paste0("$('#", ns("Biospecimens"), "').jstree('deselect_all');"))

    }, ignoreInit = TRUE)

    output$Biospecimens <- shinyTree::renderTree({
      as.list(r6$getBiospecimenSampleHierarchy())
    })

    selectedBiospecimenList <- shiny::eventReactive(c(input$Biospecimens), {

      shiny::validate(
        shiny::need(length(shinyTree::get_selected(input$Biospecimens)) > 0, "")
      )

      r6$getSelectedBiospecimenList()

    })

    output$selectedBiospecimens <- shiny::renderText({
      selectedBiospecimenList()
    })

    shiny::observeEvent(c(input$Biospecimens), {
      gargoyle::trigger("get_cohort_data")
     })

  })

}
