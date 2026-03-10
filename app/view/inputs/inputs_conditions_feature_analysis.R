box::use(
  shiny[
    NS,
    actionButton,
    column,
    eventReactive,
    fluidRow,
    htmlOutput,
    moduleServer,
    need,
    observe,
    observeEvent,
    reactive,
    renderText,
    renderUI,
    tagList,
    uiOutput,
    validate,
    icon,
    tags
  ],
  bsplus[bs_attach_modal, bs_modal],
  glue[glue],
  shinyTree[get_selected, renderTree, shinyTree],
  shinyWidgets[actionBttn],
  shinyjs[disable, enable, reset, runjs]
)

box::use(
  app/logic/shared/server_utils,
  app/view/custom_ui/input_widgets[dfToTree, treeToDf],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ConditionsInputs"))
  )
}

#' @export
server <- function(id, r6, parent) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    output$ConditionsInputs <- renderUI({

      tagList(
        bs_modal(
          id = ns("Conditions-Picker"),
          title = tags$h3(glue("Set Co-Occuring Conditions:")),
          size = "large",
          body = list(
            tagList(
              fluidRow(
                column(
                  width = 12,
                  class = "col-lg-6",
                  tags$b("Search for Co-Occuring Conditions"),
                  tags$div(
                    id = ns("Conditions-Picker"),
                    shinyTree(
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
              tags$hr(),
              actionBttn(
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
          bs_attach_modal(id_modal = ns("Conditions-Picker")),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;")
      )

    })

    observeEvent(c(input$ConditionsReset), {

      reset("ConditionsInputs")
      runjs(paste0("$('#", ns("Conditions"), "').jstree('deselect_all');"))

    }, ignoreInit = TRUE)

    conditions <- eventReactive(c(input$SetConditions), {
      r6()$ConditionChoices
    })

    output$Conditions <- renderTree({
      conditions() |>
        r6()$getConditionTree()

    })

    selectedConditionList <- eventReactive(c(input$Conditions), {
      validate(
        need(length(get_selected(input$Conditions)) > 0, "")
      )
      r6()$get_selected_condition_list(input$Conditions)
    })

    output$selectedConditions <- renderText({
      selectedConditionList()
    })

    observe({
      if (length(get_selected(input$Conditions)) > 0) {
        enable(
          selector = paste0("#", parent$ns("getData"))
        )

      } else {
        disable(
          selector = paste0("#", parent$ns("getData"))
        )
      }
    })

    selected_conditions <- reactive({
      r6()$get_selected_conditions(input$Conditions)
    })

    return(
      list(
        selected_conditions = selected_conditions
      )
    )
  })

}
