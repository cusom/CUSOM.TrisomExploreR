box::use(
    shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon] 
)

#' @export
ui <- function(id) {

    ns <- NS(id)

    shinydashboardPlus::dashboardPage(
        # preloader = list(
        #     html = shiny::tagList(
        #         waiter::spin_orbiter(),
        #         glue::glue("Loading  Explorer...")
        #     ),
        #     color = "#3c8dbc"
        # ),

        title = "Metabolome",

        header = shinydashboardPlus::dashboardHeader(
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
            controlbarIcon = shiny::icon("bars")
            # shiny::tags$li(
            #     class = "dropdown",
            #     ui_utils$createApplicationLinks(appGlobals$app_config$applicationLinks)
            # )
        ),

        sidebar = shinydashboardPlus::dashboardSidebar(
            collapsed = FALSE,
            width = 300,
            shinydashboard::sidebarMenu(
                id = "sidebar",
                shinydashboard::menuItem(
                    text = "Overview",
                    icon = icon("home"),
                    tabName = ns("overview"),
                    href = NULL,
                    newtab = TRUE,
                    selected = TRUE
                ),
                shinydashboard::menuItem(
                    text = "analysis",
                    icon = icon("database"),
                    tabName = ns("analysis"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                )
            )
        ),

        body = shinydashboard::dashboardBody(
            #tags$head(tags$html("ga/google-analytics.html")),
            shiny::tags$head(shiny::HTML('<meta name="robots" content="noindex">')),
            shiny::tags$head(tags$script(src = "custom-assets/js/script.min.js")),
            shiny::tags$head(tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);")),
            shiny::tags$head(tags$link(rel="stylesheet", type = "text/css", href = "custom-assets/css/style.css")),
            shiny::tags$link(rel = "icon", href = "www/favicon.png"),
            shinyjs::useShinyjs(),
            shinydashboard::tabItems(
                shinydashboard::tabItem(
                    tabName = ns("overview"),
                    tags$div(
                        class = "page-header",
                        tags$p("Metabolome")
                    )
                ),
                shinydashboard::tabItem(
                    tabName = ns("analysis"),
                    tags$div(
                        class = "page-header",
                        tags$p("Metabolome analysis")
                    )
                )
            )
            
        ),

        footer = shinydashboardPlus::dashboardFooter(
            tags$p("")
        )
    )

}

#' @export
server <- function(id) {
    moduleServer(id, function(input, output, session) {

    })
}
