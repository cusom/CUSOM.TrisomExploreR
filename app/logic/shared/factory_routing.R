
#' @export
resolve_class <- function(map, key, key_name = "key") {
    if (!key %in% names(map)) {
        stop(sprintf("Unknown %s '%s'. Allowed: %s",
                    key_name, key, paste(names(map), collapse = ", ")),
            call. = FALSE)
    }
    map[[key]]
}


#' @export
resolve_route_map <- function(route_map, keys, key_names = names(keys)) {
    node <- route_map

    for (i in seq_along(keys)) {
        k <- keys[[i]]
        nm <- if (!is.null(key_names) && nzchar(key_names[[i]])) key_names[[i]] else paste0("key", i)

        if (!is.list(node)) {
        stop(sprintf("Routing map error: expected list at level '%s' but got %s.",
                    nm, typeof(node)), call. = FALSE)
        }

        if (!k %in% names(node)) {
        stop(sprintf(
            "Unknown %s: '%s'. Allowed: %s",
            nm, k, paste(names(node), collapse = ", ")
        ), call. = FALSE)
        }

        node <- node[[k]]
    }

    node
}
