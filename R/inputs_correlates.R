#' Create input widgets for TrisomExploreR cross-omics correlates analysis
#' @param id - string - id for this module namespace
#' @param input_config - list - list of default values for various input widgets
#' @importFrom shinydashboardPlus box
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shinyWidgets numericRangeInput
#' @import shinyjs
#' @importFrom bsplus bs_embed_tooltip
#' @export
correlates_inputs_ui <- function(id, input_config) {
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
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("Study"),
            label = "",
            choiceNames = "NA",
            choiceValues = "NA",
            selected = "NA"
          )
        ),
        tags$b("1) Select Query Platform"),
        shinycustomloader::withLoader(
          shiny::uiOutput(ns("QueryPlatform")),
          type = "html",
          loader = "loader6",
          proxy.height = "20px"
        ),
        shiny::tags$br(),
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("StatTest"),
            label = "Statistical test",
            choices = NULL,
            selected = NULL,
            status = "primary",
            icon = NULL,
            inline = FALSE,
            width = NULL,
            choiceNames = input_config$statTestschoiceNames[1],
            choiceValues = input_config$statTests[1]
          )
        ),
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("AdjustmentMethod"),
            label = "Multiple hypothesis correction",
            choices = NULL,
            selected = NULL,
            status = "primary",
            icon = NULL,
            inline = FALSE,
            width = NULL,
            choiceNames = input_config$adjustmentMethodsNames,
            choiceValues = input_config$adjustmentMethods
          )
        ),
        shinyjs::hidden(
          shinyWidgets::awesomeCheckboxGroup(
            inputId = ns("Sex"),
            label = "Sex",
            choices = input_config$sexes,
            selected = input_config$sexes
          )
        ),
        shinyjs::hidden(
          shinyWidgets::numericRangeInput(
            inputId =  ns("Age"),
            label = "Age range",
            value = c(min(input_config$ages), max(input_config$ages))
          )
        ),
        shiny::tags$div(
          id = ns("QueryAnalyteInput"),
          shiny::selectizeInput(
            inputId = ns("QueryAnalyte"),
            label = "2) Select Query Analyte",
            choices = NULL,
            options = list(
              placeholder = "Please select below",
              onInitialize = I('function() { this.setValue(""); }'),
              closeAfterSelect = TRUE,
              selectOnTab = TRUE,
              persist = FALSE,
              `live-search` = TRUE,
              maxoptions = 1
            )
          )
        ),
        shiny::tags$div(
          id = ns("ComparisonPlatformInput"),
          shiny::selectizeInput(
            inputId = ns("ComparisonPlatform"),
            label = "3) Choose Comparison Platform",
            choices = NULL,
            options = list(
              placeholder = "Choose Comparison Platform",
              onInitialize = I('function() { this.setValue(""); }'),
              closeAfterSelect = TRUE,
              selectOnTab = TRUE,
              persist = FALSE,
              `live-search` = TRUE,
              maxoptions = 1
            )
          )
        )
      ),
      footer = shiny::tagList(
        shiny::actionButton(
          ns("getData"),
          label = "Analyze & Plot",
          class = "refresh-btn",
          icon = shiny::icon("play")
        )
      )
    )
  )
}

#' Server-side logic / processing for TrisomExploreR cross-omics correlates analysis inputs
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic for inputs
#' @import shinybusy
#' @import shinyjs
#' @import glue
#' @importFrom gargoyle trigger
#' @export
correlates_inputs_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- list(
      QueryPlatform = "",
      Queryanalyte = "",
      ComparisonPlatform = "",
      ComparisonAnalyte = ""
    )

    TrisomExploreR::bind_events(
      ids = c(
        "Study",
        "QueryPlatform",
        "Experiment",
        "QueryAnalyte",
        "ComparisonPlatform",
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

    output$QueryPlatform <- shiny::renderUI({

      platforms <- r6$getQueryPlatforms()

      shinyWidgets::prettyRadioButtons(
        inputId = ns("QueryPlatform"),
        label = NULL,
        choiceNames = platforms,
        choiceValues = platforms,
        selected = character(0)
      )

    })

    shiny::observeEvent(c(input$QueryPlatform), {

      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = glue::glue("Getting {input$QueryPlatform} Query Analytes...")
      )

      analyte_choices <- r6$getQueryAnalytes()

      shiny::updateSelectizeInput(
        session = session,
        inputId = "QueryAnalyte",
        label = "2) Select Query Analyte",
        choices = analyte_choices,
        options = list(
          placeholder = "Choose Query Analyte",
          onInitialize = I('function() { this.setValue(""); }'),
          closeAfterSelect = TRUE,
          selectOnTab = TRUE,
          persist = FALSE,
          `live-search` = TRUE,
          maxoptions = 1
        )
      )

      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = glue::glue("Getting {input$QueryPlatform} Comparison Platforms...")
      )

      comparison_platforms <- r6$getComparisonPlatforms()

      shiny::updateSelectizeInput(
        session = session,
        inputId = "ComparisonPlatform",
        label = "3) Choose Comparison Platform",
        choices = comparison_platforms,
        options = list(
          placeholder = "Choose Comparison Platform",
          onInitialize = I('function() { this.setValue(""); }'),
          closeAfterSelect = TRUE,
          selectOnTab = TRUE,
          persist = FALSE,
          `live-search` = TRUE,
          maxoptions = 1
        )

      )

      shinybusy::remove_modal_spinner()

    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$QueryAnalyte), {

      if (input$QueryAnalyte == "") {

        shinyjs::disable(id = "ComparisonPlatform")

        purge_plot(session, ns, "VolcanoPlot", r6)
        purge_plot(session, ns, "AnalytePlot", r6)

        shiny::updateSelectizeInput(
          session = session,
          inputId = "ComparisonAnalyte",
          selected = ""
        )

      } else {

        shinyjs::enable(id = "ComparisonPlatform")

      }

    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$ComparisonPlatform), {

      shiny::validate(
        shiny::need(!is.null(input$ComparisonPlatform), ""),
        shiny::need(input$ComparisonPlatform != "", "")
      )

      if (input$ComparisonPlatform != rv$ComparisonPlatform && rv$ComparisonPlatform != "") {

        # clear plots
        purge_plot(session, ns, "VolcanoPlot", r6)
        purge_plot(session, ns, "AnalytePlot", r6)

        #Reset Analyte
        r6$Analyte <- ""
        gargoyle::trigger("sync_analyte_choice", session = session)

      }

      gargoyle::trigger("validate_GSEA", session = session)

      rv$ComparisonPlatform <<- input$ComparisonPlatform

    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$QueryPlatform, input$QueryAnalyte, input$ComparisonPlatform), {

      if (length(input$QueryPlatform) != 1 |  input$QueryAnalyte == "" | input$ComparisonPlatform == "") {
        shinyjs::disable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-ready-btn")
        shinyjs::addClass(id = "getData", class = "refresh-btn")
      } else {
        shinyjs::enable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-btn")
        shinyjs::addClass(id = "getData", class = "refresh-ready-btn")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$getData), {

      shiny::validate(
        shiny::need(input$getData > 0, "")
      )

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
