#' Basic UI skin for TrisomExplorer Applications. Shinydashboard structure with sidebar tabs. 
#'
#' @param app_title - string - page title for application (see shinydashboardPlus::dashboardPage)
#' @param app_label - string - header label to be shown (see shinydashboardPlus::dashboardHeader)
#' @param app_url - string - target URL for app_label clicks 
#' @param header_links - list - list of header tiles / links (see CUSOMShinyHelpers::createApplicationLinks)
#' @param ui_config - tibble - configuration values used to dynamically build sidebar tab / content 
#'                    see TrisomExploreR::create_tab_items for more detail 
#' @param app_namespaces - list - list of namespaces / tabs to be dynamically created 
#'                    see TrisomExploreR::create_menu_items for more detail 
#' @param input_config - list - list of input values to be passed to UI modules to hydrate input widgets on init 
#' 
#' @return shinydashbaordPlus dashboardPage UI element 
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom waiter spin_orbiter
#' @importFrom glue glue
#' @importFrom shinydashboardPlus dashboardHeader
#' @importFrom CUSOMShinyHelpers createApplicationLinks
#' @importFrom shinydashboardPlus dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus dashboardFooter
#' @importFrom CUSOMShinyHelpers getSOMStandardFooter
#' @export
app_ui <- function (
  app_title,
  app_label,
  app_url,
  header_links,
  ui_config,
  app_namespaces,
  input_config
  ){

  ui <- shinydashboardPlus::dashboardPage(
      preloader = list(
        html = shiny::tagList(
          waiter::spin_orbiter(),
          glue::glue("Loading {app_label} Explorer...")
        ),
        color = "#3c8dbc"
      ),
      title = app_title,
      header = shinydashboardPlus::dashboardHeader(
        title = shiny::tags$a(
          href = app_url,
          shiny::tags$img(
            src = "www/htp_logo.png",
            height = "30"
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
      ),

      body = shinydashboard::dashboardBody(
        #tags$head(tags$html("ga/google-analytics.html")),
        shiny::tags$head(shiny::HTML('<meta name="robots" content="noindex">')),
        shiny::tags$head(tags$script(src = "custom-assets/js/script.min.js")),
        shiny::tags$head(tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);")),
        shiny::tags$head(tags$link(rel="stylesheet", type = "text/css", href = "custom-assets/css/style.css")),
        shiny::tags$link(rel = "icon", href = "www/favicon.png"),
        shinyjs::useShinyjs(),
        #introjsUI(),
        TrisomExploreR::create_tab_items(as.list(app_namespaces), ui_config, input_config)
      ),
      footer = shinydashboardPlus::dashboardFooter(
        shiny::HTML(
          CUSOMShinyHelpers::getSOMStandardFooter(footerImageFilePath = "www/medicine_h_clr.png")
        )
      )
    )

  return(
    ui
  )

}
