#' @export
cell_type_inputs_ui <- function(id, input_config) {
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
            data-original-title="Set options below to generate plot">
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
        shinyjs::hidden(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("Platform"),
            label = "Platform",
            choices = sort(input_config$platforms),
            selected = input_config$platforms[1],
            width = "90%"
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::pickerInput(
          inputId = ns("CellType"),
          label = "Cell Type(s)",
          choices = input_config$CellTypes,
          selected = input_config$CellTypes,
          options = list(
            `actions-box` = TRUE
          ),
          multiple = TRUE
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        div(
          id = "AnalyteInput",
          shiny::selectizeInput(
            inputId = ns("Analyte"),
            label = "Gene",
            choices = input_config$Genes,
            options = list(
              labelField = 'name',
              searchField = 'name',
              valueField = 'name',
              placeholder = 'Search for Gene',
              onInitialize = I('function() { this.setValue(""); }'),
              closeAfterSelect = TRUE,
              selectOnTab = TRUE,
              persist = FALSE,
              `live-search` = TRUE,
              #dropdownDirection = "down",
              # onDropdownOpen = I("
              #       function($dropdown){
              #         $dropdown.css({
              #           bottom: '100%',
              #           top: ''
              #         }).width(this.$control.outerWidth());
              #       }
              #     "
              # ),
              onType = I(paste0("
                function (str) {
                  if(this.currentResults.total == 0) {
                    Shiny.setInputValue(
                      '", ns("analyteSearchResults"), "',
                      {
                        query: this.currentResults.query,
                        total: this.currentResults.total
                      },
                      { priority: 'event' }
                    );
                  };
                }"))
            )
          ) |>
          bsplus::bs_embed_tooltip(
            title = 'Genes are searchable by ENTREZ GeneID. <br /> Not all available genes are available in dataset.',
            placement = "right",
            html = TRUE
          )
        ),
        htmlOutput(ns("AnalyteSearchError")),                
        shiny::tags$hr(style="margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("Sex"),
          label = "Sex",
          choices = input_config$sexes,
          selected = input_config$sexes,
          inline = TRUE,
          width = "90%"
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::numericRangeInput(
          inputId =  ns("Age"),
          label = "Age range",
          value = c(min(input_config$ages), max(input_config$ages)),
          width = "90%"
        ),
        tags$hr(style="margin-top:5px;margin-bottom:10px;"),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("StatTest"),
          label = "Statistical test",
          choices = NULL,
          selected = NULL,
          status = "primary",
          icon = NULL,
          inline = FALSE,
          width = "90%",
          choiceNames = input_config$statTestschoiceNames,
          choiceValues = input_config$statTests
        ),
        div(
          id = ns("CovariateInput"),
          CUSOMShinyHelpers::createInputControl(
            controlType = "checkboxGroupInput",
            inputId = ns("Covariates"),
            label = "Adjust for covariates",
            choices = c("Sex","Age") ,
            selected = c("Sex","Age"),
            inline = TRUE
          )
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("AdjustmentMethod"),
          label = "Multiple hypothesis correction",
          choices = NULL,
          selected = NULL,
          status = "primary",
          icon = NULL,
          inline = FALSE,
          width = "90%",
          choiceNames = input_config$adjustmentMethodsNames,
          choiceValues = input_config$adjustmentMethods
        ),
        tags$hr(style="margin-top:5px;margin-bottom:10px;")
      ),
      footer = shiny::tagList(
        shiny::actionButton(
          ns("Refresh"),
          label = "Analyze & Plot",
          class = "refresh-btn",
          icon = icon("play")
        )
      )
    )
  )
}

#' @export
cell_type_inputs_server <- function(id,r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    TrisomExploreR::bind_events(
      ids = c("Platform","CellType","Sex","Age","StatTest","Covariates","AdjustmentMethod","Analyte"),
      r6 = r6,
      session = session,
      parent_input = input
    )

    observeEvent(c(input$Analyte),{

      if (input$Analyte == "") {
        shinyjs::removeClass(id = "Refresh", class = "refresh-ready-btn")
        shinyjs::addClass(id = "Refresh", class = "refresh-btn")
        shinyjs::disable(id = "Refresh")
      }
      else {
        shinyjs::removeClass(id = "Refresh", class = "refresh-btn")
        shinyjs::addClass(id = "Refresh", class = "refresh-ready-btn")
        shinyjs::enable(id = "Refresh")
        shinyjs::runjs(
            paste0("
              Shiny.setInputValue(
                '", ns("analyteSearchResults"), "',
                {
                  query: '", input$Analyte, "',
                  total: ", length(input$Analyte), "
                },
                { priority: 'event' }
              );"
            )
          )
      }

    },  domain = session)

    ## when a gene is chosen -- trigger the plot
    observeEvent(c(input$Refresh),{
      if(input$Analyte != "") {
        gargoyle::trigger("render_analyte_plot", session = session)
      }
    }, ignoreInit = TRUE)

    AnalyteSearchErrorText <- eventReactive(c(input$analyteSearchResults, input$Analyte), {

      searchResultData <- input$analyteSearchResults
      req(searchResultData)

      if (length(input$Analyte) > 0 & input$Analyte != "") {
        HTML("")
      }
      else if(searchResultData$total == 0) {
        HTML(paste0('<span style="color:black;font-size:smaller;padding-left:10px;"><b>"', searchResultData$query, '"</b> not found. Please try another value</span>'))
      }
      else {
        HTML("")
      }
    }, domain = session)

    output$AnalyteSearchError <- renderUI({
      AnalyteSearchErrorText()
    })

    # output$AnalyteSearchError <- renderUI({

    #   searchResultData <- input$analyteSearchResults
    #   req(searchResultData)

    #   if(searchResultData$total == 0) {

    #     HTML(paste0('<span style="color:black;"><b>"',searchResultData$query,'"</b> not found. <br /> Please try searching for another gene</span>'))
    #   }
    #   else {

    #     HTML("")
    #   }
    # })

  })

}
