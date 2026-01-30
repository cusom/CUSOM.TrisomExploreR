box::use(
    shiny[bootstrapPage, div, moduleServer, NS, renderUI, uiOutput, tags, tagList, icon, HTML],
    shinydashboardPlus[dashboardPage, dashboardHeader, dashboardSidebar, dashboardFooter],
    shinydashboard[sidebarMenu, menuItem, dashboardBody, tabItems, tabItem],
    waiter[spin_orbiter],
    glue[glue],
    shinyjs[useShinyjs]
)

box::use(
    app/logic/shared/ui_utils[create_app_links],
    app/view/overviews/overview_proteome,
    app/view/layouts/feature_analysis,
    app/view/layouts/correlates_analysis
)

#' @export
ui <- function(id) {

    ns <- NS(id)

    dashboardPage(
        preloader = list(
            html = tagList(
                spin_orbiter(),
                glue("Loading  Proteome Explorer...")
            ),
            color = "#3c8dbc"
        ),
        title = "Proteome",
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
                    text = "Effect of trisomy 21",
                    icon = icon("dna"),
                    tabName = ns("karyotype"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),
                menuItem(
                    text = "Effects of age",
                    icon = icon("chart-line"),
                    tabName = ns("age"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),
                menuItem(
                    text = "Sex differences",
                    icon = icon("venus-mars"),
                    tabName = ns("sex"),
                    href = NULL,
                    newtab = TRUE,
                    selected = FALSE
                ),
                menuItem(
                    text = "Effect of Co-Occuring Conditions",
                    icon = icon("file-medical-alt"),
                    tabName = ns("comorbidity"),
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
                    tabName = ns("karyotype"),
                    tags$div(
                        feature_analysis$ui(ns("karyotype"))
                    )
                ),
                tabItem(
                    tabName = ns("age"),
                    tags$div(
                        feature_analysis$ui(ns("age"))
                    )
                ),
                tabItem(
                    tabName = ns("sex"),
                    tags$div(
                        feature_analysis$ui(ns("sex"))
                    )
                ),
                tabItem(
                    tabName = ns("comorbidity"),
                    tags$div(
                        feature_analysis$ui(ns("comorbidity"))
                    )
                ),
                tabItem(
                    tabName = ns("correlates"),
                    tags$div(
                        correlates_analysis$ui(ns("correlates"))
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

        output$links <- renderUI({
            create_app_links(app_config$app_config$applicationLinks)
        })

        overview_proteome$server(ns("overview"))

        sapply(c("karyotype", "age", "sex", "comorbidity"), function(x) {
        #sapply(c("karyotype"), function(x) {
            do.call(
                what = eval(parse(text = "feature_analysis$server")),
                args = list(
                    id = x,
                    app_config = app_config,
                    analysis_config = app_config$get_analysis_config(x),
                    input_config = app_config$get_input_config(x)
                )
            )
        })

        correlates_analysis$server(
            "correlates",
            app_config = app_config,
            app_config$get_analysis_config("correlates"),
            app_config$get_input_config("correlates")
        )

    })
}
