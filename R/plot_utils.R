toggle_GSEA_volcano_plot_trace <- function(session, ns, plot_name = "VolcanoPlot", r6, action = "add") {

  plotName <- get_object_name_from_namespace_session(session = session, namespace = r6$namespace, object_name = plot_name)

  shinyjs::runjs(glue::glue('resetPlotTraceVisibility("{plotName}");'))

  shinyjs::runjs(glue::glue('removeExcessPlotTraces("{plotName}",{r6$volcanoPlotExpectedTraceCount});'))

  shinyjs::runjs(glue::glue('removeExcessAnnotations("{plotName}",5);'))

  shinyjs::runjs(glue::glue('clearSelectedKeys("{plotName}");'))

  if (action == "add") {
 
    shinyjs::runjs(glue::glue('cloneTraceByKeys("{plotName}","{r6$GSEAAnalytes}","{r6$GSEATraceName}", true);'))

    shinyjs::runjs(glue::glue('isolateTraceVisibility("{plotName}","{r6$GSEATraceName}");'))

  } 

}

get_object_name_from_namespace_session <- function(session, namespace, object_name) {

  matching_objects <- tibble::tibble(
    "obj_name"  = names(session$clientData)
    ) |>
    tidyr::separate(obj_name, into = c("type", "object", "property"), sep = "_") |>
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
