box::use(
    shiny[NS, tagList, tags, moduleServer, reactive, validate, need, bindEvent,
        actionButton, icon,
        isolate, reactiveVal, observeEvent, req],
    shinyjs[enable, disable, runjs],
    shinydashboardPlus[box],
    shinycustomloader[withLoader],
    shinyWidgets[prettyRadioButtons],
    plotly[plotlyOutput, renderPlotly, event_data, event_register, toWebGL, plotlyProxy, plotlyProxyInvoke],
    shinybusy[show_modal_spinner, remove_modal_spinner],
)

box::use(
    app/logic/tofa_analysis/timeseries/TimeseriesPlot[getTimeseriesPlot],
    app/logic/shared/plot_utils[set_plot_source, object_is_rendered],
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = tags$div(
                style = "display:flex;justify-content:space-between;align-items:center;gap:12px;width:100%;padding-right:12px;",
                tags$span(style = "line-height:1.2;", "Timeseries Plot")
            ),
            height = "70vh",
            width = NULL,
            closable = FALSE,
            solidHeader = FALSE,
            collapsible = FALSE,
            headerBorder = FALSE,
            withLoader(
                plotlyOutput(
                    ns("plot"),
                    height = "70vh",
                    width = "99%"
                ),
                type = "html",
                loader = "dnaspin"
            )
        )
    )
}

#' @export
server <- function(id, scores, cohort, feature, plot_type) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        r6_obj <- reactiveVal(NULL)

        # Recreate the R6 instance when Feature changes
        observeEvent(c(cohort(), scores, plot_type(), feature()), {
            req(cohort())
            req(scores)
            req(plot_type())
            req(feature())
            validate(
                need(!is.null(cohort()), ""),
            )
            inst <- getTimeseriesPlot(
                type = plot_type(),
                dataset = scores,
                cohort = cohort(),
                feature = feature()
            )
            r6_obj(inst)
        })

        #expose a reactive that always reads the current instance
        r6 <- reactive({
            req(r6_obj())
            r6_obj()
        })

        output$plot <- renderPlotly({
            req(cohort())
            req(scores)
            req(plot_type())

            show_modal_spinner(
                spin = "atom",
                color = "#3c8dbc",
                text = "Rendering Plot..."
            )
            on.exit(remove_modal_spinner(), add = TRUE)

            p <- r6()$get_plot() |>
                set_plot_source(ns("plot")) |>
                event_register("plotly_click") |>
                event_register("plotly_doubleclick")

            p

        })

    })
}