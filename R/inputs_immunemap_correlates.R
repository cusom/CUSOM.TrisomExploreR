#' @export
immunemap_correlates_inputs_ui <- function(id, input_config) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      title = HTML(
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
      shiny::tags$div(
        id = NS(id, "scrollableOptions"),
        style = "height:630px;padding-left:10px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
        #style = "overflow-y:auto",
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("QueryPlatform"),
          label = "1) Select Query Platform",
          choiceNames = input_config$Queryplatforms,
          choiceValues = input_config$Queryplatforms,
          selected = input_config$Queryplatforms[1]
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
        ,tags$br()
        ,shinyjs::hidden(
          tags$div(
            id = ns("GSEA")
            ,tags$hr(style = "margin-top:5px;margin-bottom:10px;")
            ,tags$br()
            ,tags$p(style="font-size:14px;font-weight: 700;font-style: italic;text-align: center", "Gene Set Enrichment Analysis")
            ,actionButton(
              ns("RunGSEA"),
              label = "Run GSEA",
              width = "90%",
              class = "refresh-btn",
              icon = icon("chart-bar")
            )
            ,shinyBS::bsTooltip(
              id = ns("RunGSEA"),
              title = "Click here to see Gene Set Enrichment Analysis (GSEA)",
              placement = "top",
              trigger = "hover"
            )
          )
        )
      )
    )
  )


}

#' @export
immunemap_correlates_inputs_server <- function(id, r6, input_config, remoteDB, localDB) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    bind_events(
      ids = c("QueryPlatform","Experiment","QueryAnalyte","ComparisonPlatform","Sex","Age","StatTest","Covariates","AdjustmentMethod"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    observeEvent(c(input$QueryPlatform),{

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

      # choices <- PlatformExperiments |>
      #   filter(
      #     PlatformDisplayName == input$QueryPlatform,
      #     !is.na(ExperimentStudyName)
      #   ) |>
      #   dplyr::ExperimentStudyName,ExperimentID) |>
      #   arrange(ExperimentStudyName) |>
      #   tibble::deframe()
      #
      # updatePrettyRadioButtons(
      #   session = session,
      #   inputId = "Experiment",
      #   choices = choices,
      #   selected = character(0)
      # )
      #
      # shinyjs::click(glue("AccordionInputs-1-heading"), asis = FALSE)

    })



    # observeEvent(c(input$Experiment), {
    #
    #   validate(
    #     need(input$Experiment != "","")
    #   )
    #
    #   show_modal_spinner(
    #     spin = "half-circle",
    #     color = "#3c8dbc",
    #     text = glue("Getting Query Analytes for {input$Experiment}...")
    #   )
    #
    #   queryAnalytes <- CUSOMShinyHelpers::getDataframeFromDatabase(
    #     "EXEC [shiny].[GetAnalytesByPlatformExperiment] ?, ?"
    #     ,tibble::tibble(
    #       "Platform" = input$QueryPlatform,
    #       "ExperimentID" = input$Experiment
    #     )
    #     ,conn_args = conn_args
    #   ) |>
    #     arrange(Analyte)
    #
    #   remove_modal_spinner()
    #
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = "QueryAnalyte",
    #     choices = queryAnalytes
    #   )
    #
    #   shinyjs::click(glue("AccordionInputs-2-heading"), asis = FALSE)
    #
    # }, ignoreNULL = TRUE)


    # observeEvent(c(input$QueryAnalyte),{
    #   if(input$QueryAnalyte != "") {
    #     #shinyjs::click(glue("AccordionInputs-3-heading"), asis = FALSE)
    #   }
    # }, ignoreInit = TRUE)

    # observeEvent(c(input$ComparisonPlatform), {
    #
    #   if(grepl("SOMA",input$ComparisonPlatform) | grepl("Transcriptome",input$ComparisonPlatform)) {
    #     shinyjs::show("GSEA")
    #     if(!is.null(r6$VolcanoSummaryData)) {
    #       gargoyle::trigger("enable_GSEA", session = session)
    #     }
    #   }
    #   else {
    #     gargoyle::trigger("disable_GSEA", session = session)
    #     shinyjs::hide("GSEA")
    #   }
    # },ignoreInit = TRUE)

    # observeEvent(c(gargoyle::watch("enable_GSEA"), session = session),{
    #
    #   shinyjs::enable("RunGSEA")
    #   shinyjs::addClass(id = "RunGSEA", class = "refresh-ready-btn")
    #
    #
    # }, ignoreInit = TRUE)
    #
    # observeEvent(c(gargoyle::watch("disable_GSEA"), session = session),{
    #   shinyjs::disable("RunGSEA")
    #   shinyjs::removeClass(id = "RunGSEA", class = "refresh-ready-btn")
    #
    # }, ignoreInit = TRUE)


    # observeEvent(c(input$ComparisonPlatform), {
    #
    #   if(grepl("SOMA",input$ComparisonPlatform) | grepl("Transcriptome",input$ComparisonPlatform)) {
    #     # shinyjs::enable("RunGSEA")
    #     # shinyjs::addClass(id = "RunGSEA", class = "refresh-ready-btn")
    #     shinyjs::show("GSEA")
    #     #gargoyle::trigger("enable_GSEA")
    #   }
    #   else {
    #     # shinyjs::disable("RunGSEA")
    #     # shinyjs::removeClass(id = "RunGSEA", class = "refresh-ready-btn")
    #     #shinyjs::hide("GSEA")
    #     gargoyle::trigger("disable_GSEA")
    #   }
    #
    #   #shinyjs::click(glue("AccordionInputs-4-heading"), asis = FALSE)
    # }, ignoreInit = TRUE)

    observeEvent(c(input$getData),{

      gargoyle::trigger("get_volcano_data", session = session)

      if(grepl("SOMA",input$ComparisonPlatform) | grepl("Transcriptome",input$ComparisonPlatform)) {
        shinyjs::show("GSEA")
        shinyjs::enable("RunGSEA")
        shinyjs::addClass(id = "RunGSEA", class = "refresh-ready-btn")
        gargoyle::trigger("enable_GSEA", session = session)
      }
      else {
        shinyjs::disable("RunGSEA")
        shinyjs::removeClass(id = "RunGSEA", class = "refresh-ready-btn")
        gargoyle::trigger("disable_GSEA", session = session)
      }
      #gargoyle::trigger("get_correlates_data", session = session)
    }, ignoreInit = TRUE)

    observeEvent(c(input$RunGSEA),{

      shinybusy::show_modal_spinner(
        spin = "half-circle",
        color = "#3c8dbc",
        text = glue::glue("Calculating GSEA...")
      )

      r6$getGSEAData()

      shinybusy::remove_modal_spinner()

      gargoyle::trigger("run_GSEA", session = session)

    }, ignoreInit = TRUE)

      #   plotName <- glue("{id}-VolcanoPlot")
      #   keys <- ""
      #   shinyjs::runjs(glue('removeExcessPlotTraces("{plotName}",3);'))
      #   shinyjs::runjs(glue('removeExcessAnnotations("{plotName}",5);'))
      #   shinyjs::runjs(glue('updateSelectedKeys("{plotName}","{keys}");') )
      #
      #
      #
      # }
      #
      # else {
      #
      # }



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


  })

}
