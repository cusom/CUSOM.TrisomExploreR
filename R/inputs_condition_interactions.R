#' @export
condition_interactions_inputs_ui <- function(id, input_config) {
  ns <- NS(id)
  shiny::tagList(
    shinydashboardPlus::box(
      title = HTML(
        '<div class="dataset-options-title">Set Dataset Options
          <span
            data-toggle="tooltip"
            data-placement="auto right"
            title=""
            class="fas fa-info-circle gtooltip info-tooltip"
            data-original-title="Set options below to generate volcano plot">
          </span>
        </div>'
      ),
      height = "auto",
      width = NULL,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      headerBorder = FALSE,
      shiny::tags$div(
        id = NS(id, "scrollableOptions"),
        style = "height:600px;padding-left:10px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        shinyWidgets::prettyRadioButtons(
          inputId = ns("selectedAnnotationLevel"),
          label = "Annotation level",
          choices = c('Classes of Conditions','Specific Conditions') ,
          selected = "Classes of Conditions"
        ),
        tags$br(),
        shinyWidgets::pickerInput(
          inputId = ns("SelectedConditions"),
          label = "",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            title = "Please select below",
            selectedTextFormat = "count",
            `max-options` = 6,
            countSelectedText = "{0} chosen (out of {1})",
            dropupAuto = FALSE,
            liveSearch = TRUE,
            liveSearchPlaceholder = "Choose below",
            liveSearchStyle = "contains",
            maxOptionsText = "Maximum number selected"
          )
        ),
        tags$br(),
        tags$hr(style="margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("Karyotypes"),
          label = "Karyotype",
          choices = input_config$karyotypes,
          selected = input_config$karyotypes,
          inline = TRUE
        ),
        tags$br(),
        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("Sex"),
          label = "Sex",
          choices = input_config$sexes,
          selected = input_config$sexes,
          inline = TRUE
        ),
        tags$br(),
        shinyWidgets::numericRangeInput(
          inputId =  ns("Age"),
          label = "Age range",
          value = input_config$ages,
          width = "90%"
        )
      ),
      footer = shiny::tagList(
        actionButton(
          ns("getData"),
          label = "Analyze & Plot",
          class = "refresh-ready-btn",
          icon = icon("play")
        ),
        tags$hr(style="margin-top:5px;margin-bottom:10px;"),
        actionButton(
          ns("Reset"),
          label = "Reset Inputs",
          class = "refresh-btn",
          icon = icon("undo")
        )
      )
    )
  )
}

#' @export
condition_interactions_inputs_server <- function(id, r6, input_config) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c("selectedAnnotationLevel","SelectedConditions","Karyotypes","Sex","Age"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    observeEvent(c(input$selectedAnnotationLevel),{

      if(input$selectedAnnotationLevel == "Classes of Conditions") {

        shinyWidgets::updatePickerInput(
          session,
          'SelectedConditions',
          label = "Class (select up to 6)",
          choices = sort(input_config$ConditionClasses),
          selected = ""
        )

      } else {

        shinyWidgets::updatePickerInput(
          session,
          'SelectedConditions',
          label = "Condition (select up to 6)",
          choices = input_config$ConditionsChoices,
          selected = ""
        )

      }

      # if(input$selectedAnnotationLevel != rv$selectedAnnotationLevel & rv$selectedAnnotationLevel != "" ) {
      #
      #   rv$clearPlot <- TRUE
      #
      #   updatePickerInput(
      #     session,
      #     'SelectedConditions',
      #     selected = NULL
      #   )
      #
      # }
      #
      # else {
      #   rv$clearPlot <- FALSE
      # }
      #
      # rv$selectedAnnotationLevel <<- input$selectedAnnotationLevel

    })

    observe({

      if(length(input$SelectedConditions) < 2 ) {

        shinyjs::disable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-ready-btn")
        shinyjs::addClass(id = "getData", class = "refresh-btn")

      }

      else {

        shinyjs::enable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-btn")
        shinyjs::addClass(id = "getData", class = "refresh-ready-btn")
        #rv$clearPlot <- FALSE

      }

    })

    observeEvent(c(input$getData),{
      r6$update_upset_plot_data()
      gargoyle::trigger("get_interactions_plot")
    }, ignoreInit = TRUE)


    observeEvent(c(input$Reset),{

      shinyjs::reset("selectedAnnotationLevel")
      shinyjs::reset("SelectedConditions")
      shinyjs::reset("Karyotypes")
      shinyjs::reset("Sex")
      shinyjs::reset("Age")
      #rv$clearPlot <- TRUE
      r6$clear_upset_plot_data()
      gargoyle::trigger("get_interactions_plot")

    }, ignoreInit = TRUE)

  })


}
