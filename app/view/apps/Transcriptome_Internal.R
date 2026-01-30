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
    app/view/overviews/overview_transcriptome,
    app/view/layouts/cell_type_analysis
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
                    text = "Effect of Triomsy 21 - Cell Types",
                    icon = icon("chart-bar"),
                    tabName = ns("cell_types"),
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
                        overview_transcriptome$ui(ns("overview"))
                    )
                ),
                tabItem(
                    tabName = ns("cell_types"),
                    tags$div(
                        cell_type_analysis$ui(ns("cell-types"))
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

        cell_type_analysis$server(
            "cell-types",
            app_config = app_config,
            app_config$get_analysis_config("celltype"),
            app_config$get_input_config("celltype")
        )

    })
}
