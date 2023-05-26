#' Helper function to clone volcano plot traces for pathway / GSEA analysis
#' Calls custom JS scripts which can be found in `inst/assets/js/script.js`
#' @param session - shiny session object - session for target plot
#' @param ns - NS - namespace - not currently used
#' @param plot_name - string - name of target plot - defaults to "VolcanoPlot"
#' @param r6 - R6 class - R6 class with namespace logic
#' @param action - string - if "add", will clone and add traces to volcano plot, otherwise will remove traces
#' @importFrom shinyjs runjs
#' @importFrom glue glue 
toggle_GSEA_volcano_plot_trace <- function(session, ns, plot_name = "VolcanoPlot", r6, action = "add") {

  plotName <- get_object_name_from_namespace_session(
    session = session,
    namespace = r6$namespace,
    object_name = plot_name
  )

  shinyjs::runjs(glue::glue('resetPlotTraceVisibility("{plotName}");'))

  shinyjs::runjs(glue::glue('removeExcessPlotTraces("{plotName}",{r6$volcanoPlotExpectedTraceCount});'))

  shinyjs::runjs(glue::glue('removeExcessAnnotations("{plotName}",5);'))

  shinyjs::runjs(glue::glue('clearSelectedKeys("{plotName}");'))

  if (action == "add") {

    shinyjs::runjs(glue::glue('cloneTraceByKeys("{plotName}","{r6$GSEAAnalytes}","{r6$GSEATraceName}");'))

    shinyjs::runjs(glue::glue('isolateTraceVisibility("{plotName}","{r6$GSEATraceName}");'))

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

  shinyjs::runjs(glue::glue("PurgePlot('{plotName}');"))

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

  obj_name <- object <- id <- property <- full_object_name <- NULL

  matching_objects <- tibble::tibble(
    "obj_name"  = names(session$clientData)
    ) |>
    tidyr::separate(
      obj_name,
      into = c("type", "object", "property"), sep = "_"
    ) |>
    dplyr::mutate(
      id = stringr::str_split_i(object, "-", 1),
      full_object_name = object
    ) |>
    dplyr::filter(
      id == namespace,
      property != "hidden"
    ) |>
    tidyr::separate_rows(object, sep = "-", convert = TRUE) |>
    dplyr::filter(object == object_name) |>
    dplyr::distinct(full_object_name)

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
