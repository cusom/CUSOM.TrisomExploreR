box::use(
  glue[glue, glue_collapse],
  shinyjs[runjs],
  tibble[tibble],
  tidyr[separate],
  dplyr[select, filter, distinct, pull, mutate],
  stringr[str_split_i],
  rlang[enquo],
  purrr[imap, map2, compact]
)
box::use(
  app/logic/shared/statistical_analysis[formatPValue]
)

#' @export
set_plot_source <- function(p, source_name) {
  p$x$source <- source_name
  return(p)
}

#' @export
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

  runjs(glue('App.resetPlotTraceVisibility("{namespaced_plot_name}");'))

  runjs(glue('App.removeExcessPlotTraces("{namespaced_plot_name}",{expected_trace_count});'))

  runjs(glue('App.removeExcessAnnotations("{namespaced_plot_name}",5);'))
  runjs(glue('App.clearSelectedKeys("{namespaced_plot_name}");'))

  if (action == "add") {

    runjs(glue('App.cloneTraceByKeys("{namespaced_plot_name}","{analytes}","{trace_name}");'))
    runjs(glue('App.isolateTraceVisibility("{namespaced_plot_name}","{trace_name}");'))

  }

}

#' @export
purge_plot <- function(session, ns, plot_name, r6) {

  plotName <- get_object_name_from_namespace_session(
    session = session,
    namespace = r6$namespace,
    object_name = plot_name
  )

  runjs(glue('App.PurgePlot("{plotName}");'))

}

#' @export
object_is_rendered <- function(session, target_obj_name) {
  return(
    tibble(
      "obj_name"  = names(session$clientData)
    ) |>
    separate(
      obj_name,
      into = c("type", "object", "property", "value"),
      sep = "_", extra = "drop", fill = "right"
    ) |>
    filter(tolower(object) == tolower(target_obj_name)) |>
    distinct(object) |>
    pull() |>
    length() |>
    as.logical()
  )
}

#' @export
get_object_name_from_namespace_session <- function(session, namespace, object_name) {

  if (is.null(session) || is.null(namespace) || is.null(object_name)) {
    stop("session, namespace, and object_name are required parameters")
  }

  matching_objects <- tibble(
    "obj_name"  = names(session$clientData)
    ) |>
    separate(
        obj_name,
        into = c("type", "object", "property"),
        sep = "_", extra = "drop", fill = "right"
    ) |>
    separate(
        object, into = c("app", "Parent", "id", "module", "output"), remove = FALSE,
        sep = "-", extra = "drop", fill = "right"
    ) |>
    filter(
        tolower(id) == tolower(namespace),
        property != "hidden"
    ) |>
    filter(output == object_name) |>
    distinct(object)

  if (nrow(matching_objects) == 1) {
    return(
      matching_objects |>
        pull()
    )
  }
  if (nrow(matching_objects) > 1) {
    return(
      matching_objects
    )
  }
}

#' @export
get_namespace_by_level <- function(namespace, level = 1) {
  return(str_split_i(namespace, "-", level))
}

#' @export
getStatAnnotationAnchorLines <- function(
  .data,
  group,
  group_members,
  significance_variable,
  group_is_significant,
  include_insignificant_values = FALSE
) {

  group <- enquo(group)
  significance_variable <- enquo(significance_variable)
  group_is_significant <- enquo(group_is_significant)

  data_group_memebers <- .data |>
    select(!!group) |>
    distinct() |>
    pull()

  all_group_members <- intersect(group_members, data_group_memebers)

  line_template <- list(
    type = "line",
    x = 0,
    y = 0,
    line = list(
      color = "grey"
    ),
    xref = "x",
    yref = "paper",
    showarrow = FALSE
  )

  return(
    map2(
      all_group_members,
      seq(-0.2, length(all_group_members) - 1, 1),
      function(x, xpos) {
        stat_result <- .data |>
          filter(!!group == x) |>
          distinct(!!significance_variable) |>
          pull()

        is_significant <- .data |>
          filter(!!group == x) |>
          select(!!group_is_significant) |>
          distinct() |>
          pull()

        if (!is.na(stat_result) || include_insignificant_values) {
          line <- line_template
          line[["x0"]] <- xpos
          line[["x1"]] <- xpos + 0.5
          line[["y0"]] <- 1
          line[["y1"]] <- 1
          line[["statResult"]] <- stat_result
          line[["isSignificant"]] <- is_significant
          line[["group"]] <- x
          line
        } else {
          NULL
        }
      }
    ) |>
      compact()
  )
}

#' @export
getGroupedStatAnnotations <- function(
  AnnotationAnchorLines,
  statTest,
  covariates,
  adjustmentMethod,
  ...
  ) {

  annotation_template <- list(
    type = "line",
    x = 0,
    y = 0,
    line = list(
      color = "blue"
    ),
    font = list(
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 12
    ),
    text = "",
    xref = "x",
    yref = "paper",
    showarrow = FALSE
  )

  annotations <- imap(
      AnnotationAnchorLines,
      function(x, i) {
        annotation <- annotation_template
        stat_result <- AnnotationAnchorLines[[i]]$statResult
        annotation[["x"]] <- AnnotationAnchorLines[[i]]$x1 -
          (AnnotationAnchorLines[[i]]$x1 - (AnnotationAnchorLines[[i]]$x0) / 2)
        annotation[["y"]] <- 1.05
        annotation[["text"]] <- dplyr::case_when(
          is.na(stat_result) ~ "NA",
          !AnnotationAnchorLines[[i]]$isSignificant ~ "ns",
          stat_result <= 0.001 ~  "***",
          stat_result <= 0.01 ~ "**",
          TRUE ~ "*"
        )
        annotation[["hovertext"]] <- formatPValue(stat_result, adjustmentMethod, ...)
        return(annotation)
      }
    ) |>
      compact()

  keyText <- ifelse(
    adjustmentMethod != "none",
    "<span><b>Statistical Significance Key</b>:        ns q > 0.1
     * q <= 0.1         ** q <= 0.01        *** q <= 0.001</span>",
    "<span><b>Statistical Significance Key</b>:        ns p > 0.05
      * p <= 0.05        ** p <= 0.01        *** p <= 0.001</span>"
  )

  adjLetterString <- ifelse(adjustmentMethod == "none", "p", "q")
  covariatesString <- ifelse(
    !is.null(covariates) & statTest == "Linear Model",
    glue(' adjusted for {glue_collapse(covariates,", ", last = " and ")}'),
    ""
  )
  correctionString <- ifelse(
    adjustmentMethod == "none",
    "",
    glue(", corrected using the {adjustmentMethod} method")
  )

  hoverText <- glue("{adjLetterString}-values calculated based on a {statTest}{covariatesString}{correctionString}.")

  significanceKey <- list(
    type = "line",
    align = "left",
    x = 0.5,
    y = 1.12,
    hovertext = hoverText,
    text = keyText,
    font = list(
      size = 12
    ),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE
  )

  annotations <- c(annotations, list(significanceKey))

  return(annotations)

}
