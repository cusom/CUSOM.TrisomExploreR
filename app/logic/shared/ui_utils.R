


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

#' @export
create_app_links <- function(linkData) {

  requiredColumns <- c("label", "imageURL", "link", "IsCurrentApplication")

  if (length(requiredColumns) > length(colnames(linkData))) {

    missingArguments <- paste0(
      intersect(requiredColumns, colnames(linkData)),
      collapse = ", "
    )

    msg <- paste0(
      "Missing the following required columns: ",
      missingArguments, " "
    )

    stop(msg)

  }

  linkItems <- vector("list", nrow(linkData))

  for (i in seq_along(linkItems)) {
    linkItems[[i]] <- list(
      inputId = linkData$label[[i]],
      label = linkData$label[[i]],
      imageURL = linkData$imageURL [[i]],
      onclick = linkData$link[[i]],
      IsCurrentApplication = linkData$IsCurrentApplication[[i]]
    )
  }

  return(
    lapply(linkItems, getActionButtonLink)
  )

}

#' @export
create_app_dropdown_links <- function(linkData) {
  requiredColumns <- c("label", "link", "IsCurrentApplication")

  if (!all(requiredColumns %in% colnames(linkData))) {
    missingColumns <- requiredColumns[!requiredColumns %in% colnames(linkData)]
    stop(paste0("Missing the following required columns: ", paste(missingColumns, collapse = ", ")))
  }

  links <- lapply(seq_len(nrow(linkData)), function(i) {
    label <- as.character(linkData$label[[i]])
    href <- as.character(linkData$link[[i]])
    is_current <- suppressWarnings(as.integer(linkData$IsCurrentApplication[[i]]) == 1)
    description <- if ("description" %in% colnames(linkData)) {
      as.character(linkData$description[[i]])
    } else {
      "Open this TrisomExplorer app"
    }

    if (is.na(href) || !nzchar(href)) {
      return(
        shiny::tags$div(
          class = "dropdown-entry app-switcher-tile is-disabled",
          shiny::tags$h5(class = "font-weight-bold mb-1", label),
          shiny::tags$p(class = "mb-0 small text-light", description)
        )
      )
    }

    shiny::tags$a(
      class = "dropdown-entry app-switcher-tile",
      href = href,
      target = "_blank",
      rel = "noopener noreferrer",
      shiny::tags$h5(class = "font-weight-bold mb-1", label),
      shiny::tags$p(
        class = "mb-0 small text-light",
        if (isTRUE(is_current)) "Current application" else description
      )
    )
  })

  shiny::tags$div(
    class = "app-switcher-grid",
    shiny::tagList(links)
  )
}


getActionButtonLink <- function(x) {

  if (x$IsCurrentApplication[1] == 1) {
    return(
      shiny::actionButton(
        inputId = x$label,
        label =  x$label,
        class = "header-button-active",
        style = paste0("background-image: url('", x$imageURL, "');"),
      )
    )
  } else {
    return(
      shiny::actionButton(
        inputId = x$label,
        label =  x$label,
        class = "header-button",
        style = paste0("background-image: url('", x$imageURL, "');"),
        onclick = paste0("window.open('", x$onclick, "', '_blank')")
      )
    )
  }
}

#' @export
createTooltip <- function(Text, URL, TooltipText, ShowTooltip = TRUE, ...) {

  if (!ShowTooltip) {
    return(
      shiny::HTML(
        glue::glue("<div>{Text}</div>")
      )
    )
  } else {

    if (URL != "" && !is.na(URL)) {
      return(
        shiny::HTML(
          glue::glue(
            '<div>{Text}
              <span
                data-html="true"
                onclick="window.open(\'{URL}\');"
                data-toggle="tooltip"
                data-placement="auto right"
                title=""
                class="fas fa-info-circle gtooltip info-tooltip"
                data-original-title="{TooltipText}">
              </span>
            </div>'
          )
        )
      )
    } else {
      return(
        shiny::HTML(
          glue::glue(
            '<div>{Text}
              <span
                data-html="true"
                data-toggle="tooltip"
                data-placement="auto right"
                title=""
                class="fas fa-info-circle gtooltip info-tooltip"
                data-original-title="{TooltipText}">
              </span>
            </div>'
          )
        )
      )
    }
  }
}
