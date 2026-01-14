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
        tags$b("1) Select Query Dataset"),
        shiny::tags$div(
          id = ns("QueryStudies"),
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("QueryExperiment")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
        shiny::tags$hr(),
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
        tags$b("2) Select Query Analyte"),
        shiny::tags$div(
          id = ns("QueryAnalyteInput"),
          shiny::selectizeInput(
            inputId = ns("QueryAnalyte"),
            label = "",
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
        shiny::tags$hr(),
        shiny::tags$div(
          id = ns("CompareExperiments"),
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("CompareExperiment")),
            type = "html",
            loader = "loader6",
            proxy.height = "20px"
          )
        ),
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
      QueryExperiment = "",
      QueryAnalyte = "",
      CompareExperiment = "",
      ComparisonAnalyte = ""
    )

    TrisomExploreR::bind_events(
      ids = c(
        "Study",
        "QueryExperiment",
        "Experiment",
        "QueryAnalyte",
        "CompareExperiment",
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

    output$QueryExperiment <- shiny::renderUI({

      choices <- r6$getQueryExperiments()

      selected <- ifelse(nrow(choices) == 1, choices, character(0))

      CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
        inputId = ns("QueryExperiment"),
        label = NULL,
        fieldSetData = choices,
        selected = selected
      ) |>
        bsplus::bs_embed_tooltip(
          title = "Select a study below",
          placement = "top",
          html = TRUE
        )

    })

    shiny::observeEvent(c(input$QueryExperiment), {

      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = glue::glue("Getting Query Analytes...")
      )

      analyte_choices <- r6$getQueryAnalytes()

      shiny::updateSelectizeInput(
        session = session,
        inputId = "QueryAnalyte",
        label = "",
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

      shinybusy::remove_modal_spinner()

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    ComparisonExperiments <- shiny::reactive({
        r6$getComparisonExperiments()
    }) |>
      shiny::bindEvent(c(input$QueryExperiment), ignoreInit = TRUE, ignoreNULL = TRUE)

    output$CompareExperiment <- shiny::renderUI({

      if (!is.null(ComparisonExperiments())) {
        shiny::tagList(
          tags$b("3) Select Comparison Dataset"),
          CUSOMShinyHelpers::prettyRadioButtonsFieldSet(
            inputId = ns("CompareExperiment"),
            label = NULL,
            fieldSetData = ComparisonExperiments(),
            selected = ComparisonExperiments()
          ) |>
            bsplus::bs_embed_tooltip(
              title = "Select a study below",
              placement = "top",
              html = TRUE
            )
        )
      } else {
        shiny::tagList()
      }

    })

    shiny::observeEvent(c(input$QueryAnalyte), {

      if (input$QueryAnalyte == "") {

        shinyjs::disable(id = "CompareExperiment")

        purge_plot(session, ns, "VolcanoPlot", r6)
        purge_plot(session, ns, "AnalytePlot", r6)

        shiny::updateSelectizeInput(
          session = session,
          inputId = "ComparisonAnalyte",
          selected = ""
        )

      } else {

        shinyjs::enable(id = "CompareExperiment")

      }

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observeEvent(c(input$CompareExperiment), {
      shiny::validate(
        shiny::need(!is.null(input$CompareExperiment), ""),
        shiny::need(input$CompareExperiment != "", "")
      )

      if (input$CompareExperiment != rv$CompareExperiment && rv$CompareExperiment != "") {

        # clear plots
        purge_plot(session, ns, "VolcanoPlot", r6)
        purge_plot(session, ns, "AnalytePlot", r6)

        #Reset Analyte
        r6$Analyte <- ""
        gargoyle::trigger("sync_analyte_choice", session = session)

      }

      gargoyle::trigger("validate_GSEA", session = session)

      rv$CompareExperiment <<- input$CompareExperiment

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observeEvent(c(input$QueryExperiment, input$QueryAnalyte, input$CompareExperiment), {

      if (any(length(input$QueryExperiment) != 1 |  input$QueryAnalyte == "" | is.null(input$CompareExperiment))) {
        shinyjs::disable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-ready-btn")
        shinyjs::addClass(id = "getData", class = "refresh-btn")
      } else {
        shinyjs::enable("getData")
        shinyjs::removeClass(id = "getData", class = "refresh-btn")
        shinyjs::addClass(id = "getData", class = "refresh-ready-btn")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observeEvent(c(input$getData), {

      shiny::validate(
        shiny::need(input$getData > 0, ""),
        shiny::need(input$QueryExperiment != "", ""),
        shiny::need(input$QueryAnalyte != "", ""),
        shiny::need(input$CompareExperiment != "", "")
      )

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
