#' @export
conditions_feature_analysis_inputs_ui <- function(id, input_config) {
  ns <- shiny::NS(id)
  shiny::tagList(
    uiOutput(ns("ConditionsInputs")),
    tags$br()
  )
}

#' @export
conditions_feature_analysis_inputs_server <- function(id, r6, input_config, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c(
        "Conditions"
      ),
      r6 = r6,
      session = session,
      parent_input = input
    )

    output$ConditionsInputs <- renderUI({

      if (r6$namespace  == "Comorbidity") {
        shiny::tagList(
          bsplus::bs_modal(
            id = ns("Conditions-Picker"),
            title = htmltools::tags$h3(glue::glue("Set Co-Occuring Conditions:")),
            size = "large",
            body = list(
              shiny::tagList(
                fluidRow(
                  column(
                    width = 12,
                    class = "col-lg-6",
                    shiny::tags$b("Search for Co-Occuring Conditions"),
                    shiny::tags$div(
                      id = ns("Conditions-Picker"),
                      shinyTree::shinyTree(
                        outputId = ns("Conditions"),
                        search = TRUE,
                        multiple = TRUE,
                        theme = "proton",
                        themeIcons = FALSE,
                        themeDots = FALSE,
                        checkbox = TRUE
                      )
                    )
                  ),
                  column(
                    width = 12,
                    class = "col-lg-6",
                    tags$b("Selected Co-Occuring Conditions"),
                    htmlOutput(ns("selectedConditions"), placeholder = TRUE)
                  )
                ),
                shiny::tags$hr(),
                shinyWidgets::actionBttn(
                  inputId = ns("ConditionsReset"),
                  label = "Reset Selected Conditions",
                  icon = icon("undo"),
                  style = "minimal",
                  size = "xs",
                  color = "primary",
                  block = TRUE
                )
              )
            )
          ),
          actionButton(
            inputId = ns("SetConditions"),
            label = "Choose Co-Occuring Conditions",
            icon = icon("file-medical"),
            width = "99%"
          ) |>
            bsplus::bs_attach_modal(id_modal = ns("Conditions-Picker"))
        )
      }
      else {
        shiny::tagList(
        )
      }

    })

    observeEvent(c(input$ConditionsReset),{

      shinyjs::reset("ConditionsInputs")
      shinyjs::runjs(paste0("$('#",ns("Conditions"),"').jstree('deselect_all');"))

    }, ignoreInit = TRUE)

    conditions <- eventReactive(c(input$SetConditions), {
      input_config$ConditionChoices |>
        dplyr::select(ConditionClass, Condition)
    })

    output$Conditions <- shinyTree::renderTree({
      r6$getConditionTree(conditions())
    })

    selectedConditionList <- eventReactive(c(input$Conditions),{
      validate(
        need(length(shinyTree::get_selected(input$Conditions)) > 0, "")
      )
      r6$getSelectedConditionList()
    })

    output$selectedConditions <- renderText({
      selectedConditionList()
    })

    observe({
      if (length(shinyTree::get_selected(input$Conditions)) > 0) {
        shinyjs::enable(
          selector = paste0("#", parent$ns("getData"))
        )
      }
      else {
        shinyjs::disable(
          selector = paste0("#", parent$ns("getData"))
        )
      }
    })

  })

}
