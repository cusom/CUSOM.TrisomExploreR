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
    app/view/overviews/overview_tofa,
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
                #uiOutput(ns("links"))
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
                    text = "Analysis",
                    icon = icon("chart-line"),
                    tabName = "analysis_tab"
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
                        overview_tofa$ui(ns("overview"))
                    )
                ),
                tabItem(
                    tabName = "analysis_tab",
                    tags$div(
                        tofa_timeseries_analysis$ui(ns("analysis"))
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

        # output$links <- renderUI({
        #     create_app_links(app_config$app_config$applicationLinks)
        # })

        overview_tofa$server(ns("overview"))

        tofa_timeseries_analysis$server(
            id = "analysis",
            analysis_config = app_config
        )

    })
}
