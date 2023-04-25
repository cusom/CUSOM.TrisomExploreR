#' @export
app_ui <- function (app_title, app_label, app_url, header_links, ui_config, app_namespaces, input_config){

  ui <- shinydashboardPlus::dashboardPage(
      # preloader = list(
      #   html = tagList(
      #     waiter::spin_orbiter(),
      #     glue::glue("Loading {appConfig$applicationLabel[1]} Explorer...")
      #   ), color = "#3c8dbc"
      # ),
      title = app_title,
      header = shinydashboardPlus::dashboardHeader(
        title = shiny::tags$a(
          href = app_url,
          shiny::tags$img(
            src = "www/htp_logo.png",
            height = '30'
          ),
          app_label,
          style = "color:#fff;"
        ),
        titleWidth = 300,
        controlbarIcon = shiny::icon("bars"),
        shiny::tags$li(
          class = "dropdown",
          CUSOMShinyHelpers::createApplicationLinks(header_links)
        )
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        collapsed = FALSE,
        width = 300,
        TrisomExploreR::create_menu_items(as.list(app_namespaces), ui_config)
        # shinydashboard::sidebarMenu(
        #   id = "sidebar",
        #   sapply(
        #     as.list(app_namespaces),
        #     TrisomExploreR::create_menu_item,
        #     ui_config = ui_config
        #   )
        # )
      ),

      body = shinydashboard::dashboardBody(
        #tags$head(tags$html("ga/google-analytics.html")),
        shiny::tags$head(shiny::HTML('<meta name="robots" content="noindex">')),
        #tags$head(tags$script(HTML("custom-assets/js/script.min.js"))),
        shiny::tags$head(tags$script(src = "custom-assets/js/script.min.js")),
        shiny::tags$head(tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);")),
        #includeCSS(system.file("assets/css/style.css", package = "CUSOMShinyHelpers")),
        #includeScript(system.file("assets/js/script.min.js", package = "CUSOMShinyHelpers")),
        shiny::tags$head(tags$link(rel="stylesheet", type = "text/css", href = "custom-assets/css/style.css")),
        shiny::tags$link(rel = "icon", href = "www/favicon.png"),
        shinyjs::useShinyjs(),
        #introjsUI(),
        TrisomExploreR::create_tab_items(as.list(app_namespaces), ui_config, input_config)
        #TrisomExploreRMods::create_tab_items(as.list(app_namespaces))
      ),
      footer = shinydashboardPlus::dashboardFooter(
        HTML(
          CUSOMShinyHelpers::getSOMStandardFooter(footerImageFilePath = "www/medicine_h_clr.png")
        )
      )
    )

  return(
    ui
  )

}
