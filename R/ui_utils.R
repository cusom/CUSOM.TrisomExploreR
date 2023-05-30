#' Utility UI function to dynamically create `shinydashboard::sidebarMenu` items from config
#' @param namespaces - list of namespaces / tab items
#' @param ui_config - list of ui configurations for menu item (`text`,`name`,`icon`)
#' @importFrom shinydashboard sidebarMenu
#' @export
create_menu_items <- function(namespaces, ui_config) {
  do.call(
    shinydashboard::sidebarMenu,
    sapply(
      namespaces,
      create_menu_item,
      ui_config = ui_config
    )
  )
}

#' Utility UI function to create `shinydashboard::menuItem` from config
#' @param id - string - namespace for this instance of `menuItem` object
#' @param ui_config - list of ui configurations to build `menuItem` object
#' @import dplyr
#' @importFrom shinydashboard menuItem
#' @export
create_menu_item <- function(id, ui_config) {

  Namespace <- NULL

  menu_config <- ui_config |>
    dplyr::filter(Namespace == id)

  return(
    shiny::tagList(
      shinydashboard::menuItem(
        text = menu_config$TabText,
        tabName = menu_config$Namespace,
        icon = shiny::icon(menu_config$TabIcon, verify_fa = FALSE),
        newtab = TRUE
      )
    )
  )
}

#' Utility UI function to dynamically create `shinydashboard::tabItems` from config
#' @param namespaces - list of namespaces / tab items
#' @param ui_config - list of ui configurations to build `tabItem`
#' @param input_config - list of ui input configurations to be passed as argument to ui module
#' @importFrom shinydashboard tabItems
#' @export
create_tab_items <- function(namespaces, ui_config, input_config) {
  do.call(
    shinydashboard::tabItems,
    sapply(
      namespaces,
      create_tab_item,
      ui_config = ui_config,
      input_config = input_config
    )
  )
}

#' Utility UI function to create `shinydashboard::tabItem` from config
#' @param id - string - namespace for this instance of `tabItem`
#' @param ui_config - list of ui configuration for this instance of `tabItem`
#' @param input_config - list of input configurations to be passed to appropriate ui module
#' @import dplyr
#' @importFrom stringr str_replace
#' @importFrom shinydashboard tabItem
#' @export
create_tab_item <- function(id, ui_config, input_config) {

  Namespace <- NULL

  tab_config <- ui_config |>
    dplyr::filter(Namespace == id)

  module_ui_name <- stringr::str_replace(
    tab_config$ModuleServerName, "_server", "_ui"
  )

  return(
    shiny::tagList(
      shinydashboard::tabItem(
        tabName = tab_config$Namespace,
        do.call(module_ui_name, list(id = id, input_config = input_config))
      )
    )
  )
}
