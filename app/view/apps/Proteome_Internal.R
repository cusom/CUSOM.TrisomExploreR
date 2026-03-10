box::use(
    shiny[bootstrapPage, moduleServer, NS, renderUI, uiOutput, tags, tagList, icon, HTML],
    shinyjs[useShinyjs, runjs],
    stringr[str_replace]
)

box::use(
    app/logic/shared/ui_utils[create_app_dropdown_links],
    app/view/overviews/overview_proteome,
    app/view/layouts/feature_analysis,
    app/view/layouts/correlates_analysis
)

#' @export
ui <- function(id) {

    ns <- NS(id)

    bootstrapPage(
        tags$head(
            HTML('<meta name="robots" content="noindex">'),
            tags$script(src = "custom-assets/js/script.min.js"),
            tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);"),
            #tags$link(rel = "stylesheet", type = "text/css", href = "custom-assets/css/style.css"),
            tags$link(rel = "icon", href = "www/favicon.png")
        ),
        useShinyjs(),
        tags$div(
            class = "nav-bleed",
            tags$nav(
                id = "NavBar",
                class = "navbar-custom",
                tags$div(
                    class = "container custom-nav-container d-flex align-items-center justify-content-between flex-wrap",
                    tags$div(
                        class = "d-flex align-items-center",
                        style = "margin-right: 2rem;",
                        tags$a(
                            class = "navbar-brand",
                            href = "#/overview",
                            style = "display:flex; align-items:center; gap:0.75rem;",
                            tags$span("TrisomExplorer"),
                            tags$img(src = "/static/htp_logo.png", alt = "TrisomExplorer", height = "40")
                        )
                    ),
                    tags$ul(
                        class = "nav navdcc flex-row flex-nowrap align-items-center mx-auto",
                        tags$li(
                            class = "nav-item",
                            tags$a(href = "#/overview", class = "nav-link-underline", tags$span("Overview"))
                        ),
                        tags$li(
                            class = "nav-item",
                            tags$a(href = "#/feature", class = "nav-link-underline", tags$span("Feature Analysis"))
                        ),
                        tags$li(
                            class = "nav-item",
                            tags$a(href = "#/correlates", class = "nav-link-underline", tags$span("Cross Omics Correlates"))
                        ),
                        tags$li(
                            class = "nav-item dropdown custom-dropdown app-switcher",
                            tags$a(
                                class = "nav-link-underline",
                                href = "#",
                                id = ns("appsDropdown"),
                                role = "button",
                                tags$i(class = "fas fa-th", `aria-hidden` = "true"),
                                tags$span(style = "margin-left:0.5rem;", "App Switcher"),
                                tags$i(class = "fas fa-chevron-down caret-icon", `aria-hidden` = "true")
                            ),
                            tags$div(
                                class = "dropdown-menu custom-dropdown-menu app-switcher-menu",
                                `aria-labelledby` = ns("appsDropdown"),
                                uiOutput(ns("links"))
                            )
                        )
                    )
                )
            )
        ),
        tags$div(
            id = ns("MainDiv"),
            class = "container-fluid",
            uiOutput(ns("route_view"))
        ),
        tags$div(
            id = "FooterDiv",
            class = "container-fluid",
            tags$p("")
        )
    )

}

#' @export
server <- function(id, app_config) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        session$onFlushed(function() {
            runjs(
                "if (window.App && typeof App.setPageTitle === 'function') {" |>
                paste0(
                    "App.setPageTitle('",
                    app_config$app_config$applicationTitle,
                    "');",
                    "} else { document.title = '",
                    app_config$app_config$applicationTitle,
                    "'; }"
                )
            )

            runjs(
                "if (window.App && typeof App.initHashRouter === 'function') {" |>
                paste0(
                    "App.initHashRouter('",
                    ns("current_route"),
                    "', 'overview');",
                    "}"
                )
            )
        }, once = TRUE)

        output$links <- renderUI({
            create_app_dropdown_links(app_config$app_config$applicationLinks)
        })

        output$route_view <- renderUI({
            route <- input$current_route
            route <- ifelse(is.null(route) || route == "", "overview", route)
            route <- str_replace(route, "^/", "")

            switch(
                route,
                overview = tags$div(overview_proteome$ui(ns("overview"))),
                feature = tags$div(feature_analysis$ui(ns("feature"))),
                correlates = tags$div(correlates_analysis$ui(ns("correlates"))),
                tags$div(overview_proteome$ui(ns("overview")))
            )
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

    })
}
