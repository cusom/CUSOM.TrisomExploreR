box::use(
    shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon] 
)
box::use(
    app/view/overviews/overview_proteome,
    app/view/layouts/feature_analysis
)


#' @export
ui <- function(id) {

    ns <- NS(id)

    shinydashboardPlus::dashboardPage(
        preloader = list(
            html = shiny::tagList(
                waiter::spin_orbiter(),
                glue::glue("Loading  Explorer...")
            ),
            color = "#3c8dbc"
        ),

        title = "Proteome",

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
                    text = "Effect of trisomy 21",
                    icon = icon("dna"),
                    tabName = ns("karyotype"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),  
                shinydashboard::menuItem(
                    text = "Effects of age",
                    icon = icon("chart-line"),
                    tabName = ns("age"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),  
                shinydashboard::menuItem(
                    text = "Sex differences",
                    icon = icon("venus-mars"),
                    tabName = ns("sex"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),
                shinydashboard::menuItem(
                    text = "Effect of Co-Occuring Conditions",
                    icon = icon("file-medical-alt"),
                    tabName = ns("comorbidity"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),
                shinydashboard::menuItem(
                    text = "Cross Omics Correlates",
                    icon = icon("circle-nodes"),
                    tabName = ns("correlates"),
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
                        overview_proteome$ui(ns("overview"))
                    )
                ),
                shinydashboard::tabItem(
                    tabName = ns("karyotype"),
                    tags$div(
                        feature_analysis$ui(ns("karyotype"))
                    )
                ),
                shinydashboard::tabItem(
                    tabName = ns("age"),
                    tags$div(
                        feature_analysis$ui(ns("age"))
                    )
                ),
                shinydashboard::tabItem(
                    tabName = ns("sex"),
                    tags$div(
                        feature_analysis$ui(ns("sex"))
                    )
                ),
                shinydashboard::tabItem(
                    tabName = ns("comorbidity"),
                    tags$div(
                        feature_analysis$ui(ns("comorbidity"))
                    )
                ),
                shinydashboard::tabItem(
                    tabName = ns("correlates"),
                    tags$div(
                        tags$p("TBD")
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
server <- function(id, app_config) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        overview_proteome$server(ns("overview"))

        sapply(c("karyotype", "age", "sex", "comorbidity"), function(x) {
            do.call(
                what = eval(parse(text = "feature_analysis$server")),
                args = list(
                    id = x,
                    analysis_config = app_config$get_analysis_config(x),
                    input_config = app_config$get_input_config(x)
                )
            )
        })

    })
}
