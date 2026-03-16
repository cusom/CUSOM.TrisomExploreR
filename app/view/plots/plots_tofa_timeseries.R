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
                tags$span(style = "line-height:1.2;", "Timeseries Plot"),
                actionButton(
                    ns("clear_highlight"),
                    label = "Clear Highlight",
                    icon = icon("eraser"),
                    class = "btn btn-default btn-sm",
                    style = "margin-left:auto;",
                    disabled = "disabled"
                )
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

        observeEvent(event_data("plotly_click", source = ns("plot")), {
            enable("clear_highlight")
        })

        observeEvent(event_data("plotly_doubleclick", source = ns("plot")), {
            disable("clear_highlight")
        })

        observeEvent(c(plot_type(), feature()), {
            disable("clear_highlight")
        })

        observeEvent(input$clear_highlight, {
            req(plot_type())
            req(feature())

            feature_tag <- gsub("[^A-Za-z0-9]+", "_", as.character(feature()))
            highlight_group <- paste0("participant_id_", plot_type(), "_", feature_tag)
            highlight_group_js <- shQuote(highlight_group)
            plot_container_js <- shQuote(ns("plot"))

            runjs(sprintf(
                "App.clearTimeseriesHighlight(%s, %s);",
                highlight_group_js,
                plot_container_js
            ))

            plotlyProxy("plot", session) |>
                plotlyProxyInvoke(
                    "relayout",
                    list(
                        "xaxis.autorange" = TRUE,
                        "yaxis.autorange" = TRUE
                    )
                )
            disable("clear_highlight")
        }, ignoreInit = TRUE)

    })
}