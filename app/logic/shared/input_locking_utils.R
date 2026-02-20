box::use(
    shinyjs[enable, disable],
    shiny[observe, observeEvent, session]
)

#' Bind feature-based input locking
#' @param input shiny input object
#' @param session shiny session
#' @param feature_locked_inputs named list mapping feature value -> vector of input ids to disable
#' @param trigger_ids character vector of input ids that should trigger re-application after render
#' @param feature_input_id input id containing the selected feature value
#' @export
bind_feature_locked_inputs <- function(
    input,
    session,
    feature_locked_inputs,
    trigger_ids = c(""),
    feature_input_id = "Feature"
) {
    apply_feature_locked_inputs <- function(selected_feature) {
        all_locked_input_ids <- unique(unlist(feature_locked_inputs, use.names = FALSE))
        for (input_id in all_locked_input_ids) {
            enable(selector = paste0("#", session$ns(input_id)))
        }

        if (is.null(selected_feature) || selected_feature == "") {
            return(invisible())
        }

        ids_to_disable <- feature_locked_inputs[[selected_feature]]
        if (is.null(ids_to_disable)) {
            return(invisible())
        }

        for (input_id in ids_to_disable) {
            disable(selector = paste0("#", session$ns(input_id)))
        }
    }

    observeEvent(
        {
            lapply(trigger_ids, function(id) input[[id]])
        },
        {
            selected_feature <- input[[feature_input_id]]
            session$onFlushed(function() {
                apply_feature_locked_inputs(selected_feature)
            }, once = TRUE)
        },
        ignoreInit = FALSE,
        ignoreNULL = TRUE
    )
}

#' Bind action button state (class + enable/disable)
#' @param session shiny session
#' @param button_id button input id
#' @param is_ready_fn function returning TRUE when ready styling should be applied
#' @param can_enable_fn function returning TRUE when button should be enabled
#' @param default_class CSS class when not ready
#' @param ready_class CSS class when ready
#' @export
bind_action_button_state <- function(
    session,
    button_id,
    is_ready_fn,
    can_enable_fn,
    default_class = "refresh-btn",
    ready_class = "refresh-ready-btn"
) {
    observe({
        is_ready <- isTRUE(is_ready_fn())
        can_enable <- isTRUE(can_enable_fn())
        selector <- paste0("#", session$ns(button_id))

        removeClass(class = default_class, selector = selector)
        removeClass(class = ready_class, selector = selector)
        addClass(
            class = if (is_ready) ready_class else default_class,
            selector = selector
        )

        if (can_enable) {
            enable(selector = selector)
        } else {
            disable(selector = selector)
        }
    })
}
