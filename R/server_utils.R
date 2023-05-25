#' @export
load_modules <- function(id, module_config) {

  namespace_config <- dplyr::filter(module_config, Namespace == id)

  if (namespace_config$UseR6Class) {

    r6_name <- glue::glue("r6_{id}")

    f <- eval(parse(text = glue::glue("{namespace_config$R6ClassName}$new")))

    assign(x = r6_name, value = do.call(f, list(namespace_config)))

    do.call(
      namespace_config$ModuleServerName,
      list(id, base::get(r6_name))
    )

  } else {

    do.call(namespace_config$ModuleServerName, list(id))

  }
}

### sync input events to r6 attrs.
#' @export
bind_events <- function(ids, r6, session, parent_input) {
  lapply(
    ids, function(x){
      shiny::observeEvent(parent_input[[x]], {
        r6[[x]] <- parent_input[[x]]
      }, domain = session)
    }
  )
}