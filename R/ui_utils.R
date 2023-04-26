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


#' @importFrom shinydashboard menuItem
#' @export
create_menu_item <- function(id, ui_config) {

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

#' @export
create_tab_item <- function(id, ui_config, input_config) {

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
