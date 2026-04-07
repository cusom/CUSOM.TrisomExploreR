box::use(
    shiny[bootstrapPage, div, moduleServer, NS, renderUI, uiOutput, tags, tagList, icon, HTML],
    shinydashboardPlus[dashboardPage, dashboardHeader, dashboardSidebar, dashboardFooter],
    shinydashboard[sidebarMenu, menuItem, dashboardBody, tabItems, tabItem],
    waiter[spin_orbiter],
    glue[glue],
    shinyjs[useShinyjs, runjs]
)

box::use(
    app/logic/shared/ui_utils[create_app_links],
    app/view/overviews/overview_proteome,
    app/view/layouts/feature_analysis,
    app/view/layouts/correlates_analysis,
    app/view/layouts/tofa_timeseries_analysis,
)

#' @export
ui <- function(id) {

    ns <- NS(id)

    dashboardPage(
        preloader = list(
            html = tagList(
                spin_orbiter(),
                glue("Loading TrisomExplorer...")
            ),
            color = "#3c8dbc"
        ),
        title = "",
        header = dashboardHeader(
            title = tags$a(
                href = "",
                tags$img(
                    src = "/static/htp_logo.png",
                    height = "30"
                ),
                "TrisomExplorer",
                style = "color:#fff;"
            ),
            titleWidth = 300,
            controlbarIcon = icon("bars"),
            tags$li(
                class = "dropdown",
                uiOutput(ns("links"))
            )
        ),
        sidebar = dashboardSidebar(
            collapsed = FALSE,
            width = 300,
            sidebarMenu(
                id = "sidebar",
                menuItem(
                    text = "Overview",
                    icon = icon("home"),
                    tabName = ns("overview"),
                    href = NULL,
                    newtab = TRUE,
                    selected = TRUE
                ),
                menuItem(
                    text = "Feature Analysis",
                    icon = icon("chart-line"),
                    tabName = ns("feature"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),
                menuItem(
                    text = "Cross Omics Correlates",
                    icon = icon("circle-nodes"),
                    tabName = ns("correlates"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),
                menuItem(
                    text = "TOFA Analysis",
                    icon = icon("book-medical"),
                    tabName = ns("tofa_timeseries"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                )
            )
        ),

        body = dashboardBody(
            #tags$head(tags$html("ga/google-analytics.html")),
            tags$head(HTML('<meta name="robots" content="noindex">')),
            tags$head(tags$script(src = "custom-assets/js/script.min.js")),
            tags$head(tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);")),
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom-assets/css/style.css")),
            tags$link(rel = "icon", href = "www/favicon.png"),
            useShinyjs(),
            tabItems(
                tabItem(
                    tabName = ns("overview"),
                    tags$div(
                        overview_proteome$ui(ns("overview"))
                    )
                ),
                tabItem(
                    tabName = ns("feature"),
                    tags$div(
                        feature_analysis$ui(ns("feature"))
                    )
                ),
                tabItem(
                    tabName = ns("correlates"),
                    tags$div(
                        correlates_analysis$ui(ns("correlates"))
                    )
                ),
                tabItem(
                    tabName = ns("tofa_timeseries"),
                    tags$div(
                        tofa_timeseries_analysis$ui(ns("tofa_timeseries"))
                    )
                )
            )
        ),
        footer = dashboardFooter(
            tags$p("")
        )
    )

}

#' @export
server <- function(id, app_config) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        runjs(
            "App.setPageTitle('" |>
            paste0(app_config$app_config$applicationTitle, "');")
        )

        output$links <- renderUI({
            create_app_links(app_config$app_config$applicationLinks)
        })

        overview_proteome$server(ns("overview"))

        feature_analysis$server(
            "feature",
            app_config = app_config,
            analysis_config = app_config,
            input_config = app_config
        )

        correlates_analysis$server(
            "correlates",
            app_config = app_config,
            analysis_config = app_config,
            input_config = app_config
        )

        tofa_timeseries_analysis$server(
            id = "tofa_timeseries",
            app_config = app_config
        )

    })
}
