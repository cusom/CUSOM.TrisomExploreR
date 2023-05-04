#' @export
immunemap_correlates_inputs_ui <- function(id, input_config) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      title = shiny::HTML(
        '<div class="dataset-options-title">Dataset Options
          <span
            data-toggle="tooltip"
            data-placement="auto right"
            title = ""
            class = "fas fa-filter"
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
      shinyjs::disabled(
        bs4Dash::tooltip(
          shiny::actionButton(
            ns("PrimaryTutorial"),
            label = "Take Tutorial",
            class = "tutorial-btn",
            icon = icon("question-circle")
          ),
          title = "Click here to learn about setting dataset options to generate the volcano plot",
          placement = "top"
        )
      ),
      shiny::tags$div(
        id = NS(id, "scrollableOptions"),
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
        shinyWidgets::prettyRadioButtons(
          inputId = ns("QueryPlatform"),
          label = "1) Select Query Platform",
          choiceNames = input_config$Queryplatforms,
          choiceValues = input_config$Queryplatforms,
          selected = character(0)
        ),
        tags$br(),
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
        )
        ,div(
          id = ns("QueryAnalyteInput"),
          selectizeInput(
            inputId = ns("QueryAnalyte"),
            label="2) Select Query Analyte",
            choices= NULL,
            options = list(
              placeholder = 'Please select below',
              onInitialize = I('function() { this.setValue(""); }'),
              closeAfterSelect = TRUE,
              selectOnTab = TRUE,
              persist = FALSE,
              `live-search` = TRUE,
              maxoptions = 1
            )
          )
        ),
        div(
          id = ns("ComparisonPlatformInput"),
          selectizeInput(
            inputId = ns("ComparisonPlatform"),
            label ="3) Choose Comparison Platform",
            choices = input_config$Comparisonplatforms,
            options = list(
              placeholder = 'Choose Comparison Platform',
              onInitialize = I('function() { this.setValue(""); }'),
              closeAfterSelect = TRUE,
              selectOnTab = TRUE,
              persist = FALSE,
              `live-search` = TRUE,
              maxoptions = 1
            )
          )
        ),
        tags$hr(),
        tags$div(
          style = "display:inline-block; max-width:100%;margin-bottom:5px;font-weight:700",
          "4) Set Statistics (pre-calculated)"
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("StatTest"),
          label = "Statistical test",
          choices = NULL,
          selected = NULL,
          status = "primary",
          icon = NULL,
          inline = FALSE,
          width = NULL,
          choiceNames = list(CUSOMShinyHelpers::createTooltip("Beta Regression","","Beta regression is useful in situations where the dependent variable is continuous and restricted to the unit interval (0, 1), e.g., resulting from rates or proportions. It is modeled to be beta-distributed with parametrization using mean and precision parameter (called phi). The mean is linked, as in generalized linear models (GLMs), to the responses through a link function (logit) and a linear predictor.")),
          choiceValues = "Beta"
        ),
        tags$br(),
        tags$div(
         id = ns("CovariateInput"),
         shinyjs::disabled(
           shinyWidgets::awesomeCheckboxGroup(
             inputId =  ns("Covariates"),
             label = "Adjust for covariates",
             choices = c("Sex","Age","Source"),
             selected = c("Sex","Age","Source"),
             inline = TRUE
           )
         )
       )
       ,tags$br()
       ,shinyWidgets::prettyRadioButtons(
         inputId = ns("AdjustmentMethod"),
         label = "Multiple hypothesis correction",
         choices = NULL,
         selected = NULL,
         status = "primary",
         icon = NULL,
         inline = FALSE,
         width = NULL,
         choiceNames = input_config$adjustmentMethodsNames[1],
         choiceValues = input_config$adjustmentMethods[1]
       )
       #)
        # bs_accordion(id = ns("AccordionInputs")) |>
        #   bs_set_opts(panel_type = "default", use_heading_link = TRUE) |>
        #   bs_append(
        #     title = "1) Choose Query Platform",
        #     content = list(
        #       prettyRadioButtons(
        #         inputId = ns("QueryPlatform"),
        #         label = "Platforms",
        #         choices = sort(Queryplatforms),
        #         selected = character(0),
        #         status = "primary",
        #         icon = NULL,
        #         inline = FALSE
        #       )
        #     )
        #   ) |>
        #   bs_append(
        #     title = "2) Choose Experiment",
        #     content = list(
        #       prettyRadioButtons(
        #         inputId = ns("Experiment"),
        #         label = "Experiments",
        #         choices = NULL,
        #         selected = character(0),
        #         status = "primary",
        #         icon = NULL,
        #         inline = FALSE
        #       )
        #     )
        #   ) |>
        #   bs_append(
        #     title = "3) Choose Query Analyte",
        #     content = list(
        #       selectizeInput(
        #         inputId = ns("QueryAnalyte"),
        #         label = "Analytes",
        #         width = "300px",
        #         choices = NULL,
        #         multiple = FALSE,
        #         options = list(
        #           labelField ='name',
        #           searchField ='name',
        #           valueField = 'id',
        #           placeholder = 'Choose an analyte',
        #           onInitialize = I('function() { this.setValue(""); }'),
        #           closeAfterSelect = TRUE,
        #           selectOnTab = TRUE,
        #           persist = FALSE,
        #           `live-search` = TRUE,
        #           #maxoptions = 1,
        #           dropupAuto = FALSE,
        #           onType = I(paste0("
        #           function (str) {
        #             if(this.currentResults.total == 0) {
        #               Shiny.setInputValue(
        #                 '",id,"-analyteSearchResults',
        #                 {
        #                   query: this.currentResults.query,
        #                   total: this.currentResults.total
        #                 },
        #                 { priority: 'event' }
        #               );
        #             };
        #           }"
        #           ))
        #         )
        #       )
        #     )
        #   ) |>
        #   bs_append(
        #     title = "4) Choose Comparison Platform",
        #     content = list(
        #       selectizeInput(
        #         inputId = ns("ComparisonPlatform"),
        #         label= "Comparison Platform",
        #         choices= Comparisonplatforms,
        #         options = list(
        #           placeholder = 'Choose Comparison Platform',
        #           onInitialize = I('function() { this.setValue(""); }'),
        #           closeAfterSelect = TRUE,
        #           selectOnTab = TRUE,
        #           persist = FALSE,
        #           `live-search` = TRUE,
        #           maxoptions = 1
        #         )
        #       )
        #     )
        #   ) |>
        #   bs_append(
        #     title = "5) Set Analysis Options (optional)",
        #     content = list(
        #
        #       CUSOMShinyHelpers::createInputControl(
        #         controlType = "checkboxGroupInput",
        #         inputId = ns("Sex"),
        #         label = "Sex",
        #         choices = sexes,
        #         selected = sexes,
        #         inline=TRUE
        #       ),
        #       tags$br(),
        #       numericRangeInput(
        #         inputId =  ns("Age"),
        #         label = "Age range",
        #         value = c(min(ages), max(ages))
        #       )
        #     )
        #   ) |>
        #   bs_append(
        #     title = "6) Set Statistics (optional)",
        #     content = list(
        #       prettyRadioButtons(
        #         inputId = ns("StatTest"),
        #         label = "Statistical test",
        #         choices = NULL,
        #         selected = NULL,
        #         status = "primary",
        #         icon = NULL,
        #         inline = FALSE,
        #         choiceNames = statTestschoiceNames,
        #         choiceValues = statTests
        #       )
        #       ,tags$br()
        #       ,div(
        #         id=ns("CovariateInput"),
        #         CUSOMShinyHelpers::createInputControl(
        #           controlType = "checkboxGroupInput",
        #           inputId = ns("Covariates"),
        #           label = "Adjust for covariates",
        #           choices = c("Sex","Age") ,
        #           selected = c("Sex","Age"),
        #           inline = TRUE
        #         )
        #       )
        #       ,tags$br()
        #       ,prettyRadioButtons(
        #         inputId = ns("AdjustmentMethod"),
        #         label = "Multiple hypothesis correction",
        #         choices = NULL,
        #         selected = NULL,
        #         status = "primary",
        #         icon = NULL,
        #         inline = FALSE,
        #         choiceNames = adjustmentMethodsNames,
        #         choiceValues = adjustmentMethods
        #       )
        #     )
        #   ),
      ), footer = shiny::tagList(
        actionButton(
          ns("getData"),
          label = "Analyze & Plot",
          class = "refresh-btn",
          icon = icon("play")
        )       
      )
    )
  )


}

#' @export
immunemap_correlates_inputs_server <- function(id, r6, input_config, remoteDB, localDB) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

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

    observeEvent(c(input$QueryPlatform), {

      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#3c8dbc",
        text = glue::glue("Getting {input$QueryPlatform} Query Analytes...")
      )

      analyteChoices <- remoteDB$getQuery(
        "[shiny].[GetQueryAnalytes] ?",
        tibble::tibble("QueryPlatform" = input$QueryPlatform)
        ) |>
        dplyr::rename("QueryAnalyteID" = QueryAnalyte) |>
        tidyr::separate(QueryAnalyteID, sep = ";", into = c("CellType","Lineage","Analyte"), remove = FALSE) |>
        dplyr::mutate(QueryAnalyte = glue::glue("{Lineage};{Analyte}")) |>
        dplyr::select(QueryAnalyte,QueryAnalyteID) |>
        dplyr::arrange(QueryAnalyte) |>
        tibble::deframe()

      updateSelectizeInput(
        session = session,
        inputId = "QueryAnalyte",
        label = "2) Select Query Analyte",
        choices = analyteChoices,
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
   
    }, ignoreInit = TRUE)

    observeEvent(c(input$ComparisonPlatform),{
      validate(
        need(!is.null(input$ComparisonPlatform), ""),
        need(input$ComparisonPlatform != "", "")
      )
      gargoyle::trigger("validate_GSEA", session = session)

    }, ignoreInit = TRUE)

    observe({
      if(input$QueryAnalyte == "" & input$ComparisonPlatform == "") {
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

    observeEvent(c(input$getData),{

      validate(
        need(input$getData > 0, "")
      )

      gargoyle::trigger("get_volcano_data", session = session)

    }, ignoreInit = TRUE)

  })
}
