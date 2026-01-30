box::use(
    shiny[NS, moduleServer, tagList, tags, HTML, uiOutput, renderUI, htmlOutput,
        actionButton, selectizeInput, updateSelectizeInput, icon, validate, need,
        observeEvent, reactive, bindEvent],
    shinydashboardPlus[box],
    shinyjs[hidden, disabled],
    shinyWidgets[prettyRadioButtons, awesomeCheckboxGroup, numericRangeInput,
        pickerInput],
    shinycustomloader[withLoader],
    promises[future_promise, `%...!%`, `%...>%`],
    shinyjs[addClass, removeClass, disable, enable]
)

box::use(
    app/logic/shared/server_utils,
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = HTML(
                "<div class=\"dataset-options-title\">Dataset Options
                <span
                    data-toggle=\"tooltip\"
                    data-placement=\"auto right\"
                    title = \"\"
                    class = \"fas fa-filter\"
                    data-original-title = \"Set options below to generate plot\">
                </span>
                </div>"
            ),
            height = "auto",
            width = NULL,
            closable = FALSE,
            solidHeader = FALSE,
            collapsible = FALSE,
            headerBorder = FALSE,
            tags$div(
                id = ns("scrollableOptions"),
                style = "height:70vh;padding-left:2px;max-height:700px;overflow-y:auto;overflow-x:hidden;",
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                tags$b("Cell Type(s)"),
                withLoader(
                    uiOutput(ns("CellType")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                tags$div(
                    id = "AnalyteInput",
                    selectizeInput(
                        inputId = ns("Analyte"),
                        label = "Gene",
                        choices = NULL,
                        multiple = FALSE,
                        options = list(
                            placeholder = "Select gene",
                            onInitialize = I('function() { this.setValue(""); }'),
                            closeAfterSelect = TRUE,
                            selectOnTab = TRUE,
                            persist = FALSE,
                            `live-search` = TRUE,
                            dropupAuto = FALSE,
                            maxOptions = 30
                        )
                    )
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                uiOutput(ns("Sex")),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                uiOutput(ns("Age")),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                uiOutput(ns("StatTest")),
                tags$br(),
                tags$b("Adjust for covariates"),
                withLoader(
                    uiOutput(ns("Covariates")),
                    type = "html",
                    loader = "loader6",
                    proxy.height = "20px"
                ),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
                uiOutput(ns("AdjustmentMethod")),
                tags$hr(style = "margin-top:5px;margin-bottom:10px;")
            ),
            footer = tagList(
                actionButton(
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
server <- function(id, r6) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        server_utils$bind_events(
            ids = c(
                "CellType",
                "Analyte",
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

        output$CellType <- renderUI({
            disabled(
                pickerInput(
                    inputId = ns("CellType"),
                    label = "Disabled - Coming Soon",
                    choices = r6$CellTypes,
                    selected = r6$CellTypes,
                    options = list(
                        `actions-box` = TRUE
                    ),
                    multiple = TRUE
                )
            )
        })

        observeEvent(input$CellType, {
            disable(id = "Analyte")
            future_promise(
                r6$Analytes
            )  %...!% warning() %...>% {
                updateSelectizeInput(
                    session = session,
                    inputId = "Analyte",
                    choices = .,
                    selected = character(0),
                    server = TRUE
                )
                enable(id = "Analyte")
            }
        }, once = TRUE)

        output$Sex <- renderUI({
            awesomeCheckboxGroup(
                inputId = ns("Sex"),
                label = "Sex",
                choices = r6$Sexes,
                selected = r6$Sexes,
                inline = TRUE,
                width = "90%"
            )
        })

        output$Age <- renderUI({
            numericRangeInput(
                inputId = ns("Age"),
                label = "Age range",
                value = r6$Ages,
                width = "90%"
            )
        })

        output$StatTest <- renderUI({
            prettyRadioButtons(
                inputId = ns("StatTest"),
                label = "Statistical test",
                choices = NULL,
                selected = NULL,
                status = "primary",
                icon = NULL,
                inline = FALSE,
                width = "90%",
                choiceNames = r6$StatTestNames,
                choiceValues = r6$StatTestValues
            )
        })

        output$Covariates <- renderUI({
            validate(
                need(input$StatTest != "", "")
            )
            if (input$StatTest == "Linear Model") {
                choices <- r6$CovariateChoices
                tagList(
                    awesomeCheckboxGroup(
                        inputId = ns("Covariates"),
                        label = NULL,
                        choices = choices,
                        selected = choices,
                        inline = TRUE
                    )
                )
            } else {
                tagList(

                )
            }
        })

        output$AdjustmentMethod <- renderUI({
            prettyRadioButtons(
                inputId = ns("AdjustmentMethod"),
                label = "Multiple hypothesis correction",
                choices = NULL,
                selected = NULL,
                status = "primary",
                icon = NULL,
                inline = FALSE,
                width = "90%",
                choiceNames = r6$AdjustmentMethodNames,
                choiceValues = r6$AdjustmentMethodValues
            )
        })

        observeEvent(c(input$Analyte), {

            if (input$Analyte == "") {
                removeClass(id = "Refresh", class = "refresh-ready-btn")
                addClass(id = "Refresh", class = "refresh-btn")
                disable(id = "Refresh")
            } else {
                removeClass(id = "Refresh", class = "refresh-btn")
                addClass(id = "Refresh", class = "refresh-ready-btn")
                enable(id = "Refresh")
            }

        }, ignoreInit = TRUE, domain = session)

        cell_type_data <- reactive({
            r6$cell_type_data()
        }) |>
            bindEvent(input$Refresh, ignoreInit = TRUE, ignoreNULL = TRUE)

        return(
            list(
                "cell_type_data" = cell_type_data
            )
        )

    })

}