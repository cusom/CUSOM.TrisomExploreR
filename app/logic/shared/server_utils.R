#' Utility server function to dynamically load classes and modules
#' @param id namespace for this module instance
#' @param module_config list - list of configurations for module
#' @import dplyr
#' @export
load_modules <- function(id, module_config) {

  Namespace <- NULL

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

#' Utility server function to sync input values to appropriate R6 class
#' @param ids - character vector of input ids to sync to R6 class fields
#' @param r6 - target R6 class instance
#' @param session - shiny session - session for inputs to sync
#' @param parent_input - shiny input object from target session
#' @export
bind_events <- function(ids, r6, session, parent_input, setter_candidates = c("set_input", "set", "update")) {

  stopifnot(is.function(r6))  # ensure we received a reactive, not an object

  lapply(ids, function(id) {
    shiny::observeEvent(parent_input[[id]], ignoreInit = FALSE, {
      inst <- r6()
      shiny::req(inst)  # current class instance must exist

      val <- parent_input[[id]]

      # 1) Prefer a class method to set inputs
      for (m in setter_candidates) {
        if (isTRUE(is.function(inst[[m]]))) {
          # Isolate to avoid reactive feedback if your setter triggers input updates
          shiny::isolate(inst[[m]](id, val))
          return(invisible(NULL))
        }
      }

      # 2) Fallback: assign to a public field with `$` (if one exists)
      # Guard against redundant writes to reduce loops
      current_val <- tryCatch(inst[[id]], error = function(e) NULL)
      if (!identical(current_val, val)) {
        # Use $ if you know the field name, otherwise [[ is fine if supported
        inst[[id]] <- val
      }
    }, domain = session)
  })
}