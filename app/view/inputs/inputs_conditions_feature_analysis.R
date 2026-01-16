box::use(
  shiny[tags]
)

box::use(
  app/logic/helpers/server_utils,
  app/view/custom_ui/input_widgets[dfToTree],
)



#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("ConditionsInputs"))
  )
}

#' @export
server <- function(id, r6, parent) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    server_utils$bind_events(
      ids = c(
        "Conditions"
      ),
      r6 = r6,
      session = session,
      parent_input = input
    )

    output$ConditionsInputs <- shiny::renderUI({

      if (r6$namespace  == "Comorbidity") {
        shiny::tagList(
          bsplus::bs_modal(
            id = ns("Conditions-Picker"),
            title = shiny::tags$h3(glue::glue("Set Co-Occuring Conditions:")),
            size = "large",
            body = list(
              shiny::tagList(
                shiny::fluidRow(
                  shiny::column(
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
                  shiny::column(
                    width = 12,
                    class = "col-lg-6",
                    tags$b("Selected Co-Occuring Conditions"),
                    shiny::htmlOutput(ns("selectedConditions"), placeholder = TRUE)
                  )
                ),
                shiny::tags$hr(),
                shinyWidgets::actionBttn(
                  inputId = ns("ConditionsReset"),
                  label = "Reset Selected Conditions",
                  icon = shiny::icon("undo"),
                  style = "minimal",
                  size = "xs",
                  color = "primary",
                  block = TRUE
                )
              )
            )
          ),
          shiny::actionButton(
            inputId = ns("SetConditions"),
            label = "Choose Co-Occuring Conditions",
            icon = shiny::icon("file-medical"),
            width = "99%"
          ) |>
            bsplus::bs_attach_modal(id_modal = ns("Conditions-Picker")),
          shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;")
        )
      } else {
        shiny::tagList(
        )
      }

    })

    shiny::observeEvent(c(input$ConditionsReset), {

      shinyjs::reset("ConditionsInputs")
      shinyjs::runjs(paste0("$('#", ns("Conditions"), "').jstree('deselect_all');"))

    }, ignoreInit = TRUE)

    conditions <- shiny::eventReactive(c(input$SetConditions), {
      r6$ConditionChoices
    })

    output$Conditions <- shinyTree::renderTree({

      conditions() |>
        dfToTree() |>
        r6$setConditionTreeAttributes()

    })

    selectedConditionList <- shiny::eventReactive(c(input$Conditions), {
      shiny::validate(
        shiny::need(length(shinyTree::get_selected(input$Conditions)) > 0, "")
      )
      r6$SelectedConditionList
    })

    output$selectedConditions <- shiny::renderText({
      selectedConditionList()
    })

    shiny::observe({
      if (length(shinyTree::get_selected(input$Conditions)) > 0) {
        shinyjs::enable(
          selector = paste0("#", parent$ns("getData"))
        )

      } else {
        shinyjs::disable(
          selector = paste0("#", parent$ns("getData"))
        )
      }
    })

  })

}
