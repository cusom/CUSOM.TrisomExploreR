#' Helper function to clone volcano plot traces for pathway / GSEA analysis
#' Calls custom JS scripts which can be found in `inst/assets/js/script.js`
#' @param session - shiny session object - session for target plot
#' @param ns - NS - namespace - not currently used
#' @param plot_name - string - name of target plot - defaults to "VolcanoPlot"
#' @param r6 - R6 class - R6 class with namespace logic
#' @param action - string - if "add", will clone and add traces to volcano plot, otherwise will remove traces
#' @importFrom shinyjs runjs
#' @importFrom glue glue 
toggle_GSEA_volcano_plot_trace <- function(
    session, 
    ns, 
    namespace,
    plot_name = "VolcanoPlot", 
    expected_trace_count = 3,
    analytes = "",
    trace_name = "GSEA Pathway",
    action = "add"
  ) {

  namespaced_plot_name <- get_object_name_from_namespace_session(
    session = session,
    #assume namespace is of form app-parent_namespace-module_namespace-child_namespace-child...
    namespace = get_namespace_by_level(namespace, 3),
    object_name = plot_name
  )

  shinyjs::runjs(glue::glue('App.resetPlotTraceVisibility("{namespaced_plot_name}");'))

  shinyjs::runjs(glue::glue('App.removeExcessPlotTraces("{namespaced_plot_name}",{expected_trace_count});'))

  shinyjs::runjs(glue::glue('App.removeExcessAnnotations("{namespaced_plot_name}",5);'))
  shinyjs::runjs(glue::glue('App.clearSelectedKeys("{namespaced_plot_name}");'))

  if (action == "add") {

    shinyjs::runjs(glue::glue('App.cloneTraceByKeys("{namespaced_plot_name}","{analytes}","{trace_name}");'))
    shinyjs::runjs(glue::glue('App.isolateTraceVisibility("{namespaced_plot_name}","{trace_name}");'))

  }

}

#' Helper function to purge plotly plot
#' Calls custom JS scripts which can be found in `inst/assets/js/script.js`
#' @param session - shiny session object
#' @param ns - NS - namespace - not currently used
#' @param plot_name - string - name of target plot
#' @param r6 - R6 class - R6 class with namespace logic
#' @importFrom shinyjs runjs 
#' @importFrom glue glue
purge_plot <- function(session, ns, plot_name, r6) {

  plotName <- get_object_name_from_namespace_session(
    session = session,
    namespace = r6$namespace,
    object_name = plot_name
  )

  shinyjs::runjs(glue::glue('App.PurgePlot("{plotName}");'))

}


#' Helper function to find and return fully-qualified / namespaced object name
#' @param session - shiny session object
#' @param namespace - string - namespace for target object
#' @param object_name - string - name of target object
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @importFrom stringr str_split_i
get_object_name_from_namespace_session <- function(session, namespace, object_name) {

  if (is.null(session) | is.null(namespace) | is.null(object_name)) {
    stop("session, namespace, and object_name are required parameters")
  }

  matching_objects <- tibble::tibble(
    "obj_name"  = names(session$clientData)
    ) |>
    tidyr::separate(
        obj_name,
        into = c("type", "object", "property"), 
        sep = "_", extra = "drop", fill = "right"
    ) |>
    tidyr::separate(
        object, into = c("app", "Parent", "id", "module", "output"), remove = FALSE,
        sep = "-", extra = "drop", fill = "right"
    ) |>
    dplyr::filter(
        tolower(id) == tolower(namespace),
        property != "hidden"
    ) |>
    dplyr::filter(output == object_name) |>
    dplyr::distinct(object)

  if (nrow(matching_objects) == 1) {
    return(
      matching_objects |>
        dplyr::pull()
    )
  }
  if (nrow(matching_objects) > 1) {
    return(
      matching_objects
    )
  }
}


get_namespace_by_level <- function(namespace, level = 1) {
  return(stringr::str_split_i(namespace, "-", level))
}