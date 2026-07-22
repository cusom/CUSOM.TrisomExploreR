#' @export
resolve_column_name <- function(
    data,
    preferred = NULL,
    exact_candidates,
    pattern_candidates,
    output_name,
    data_label = "plot-data"
) {
    cols <- colnames(data)

    if (!is.null(preferred) && nzchar(preferred) && preferred %in% cols) {
        return(preferred)
    }

    exact_match <- exact_candidates[exact_candidates %in% cols]
    if (length(exact_match) > 0) {
        return(exact_match[[1]])
    }

    for (pattern in pattern_candidates) {
        matches <- cols[grepl(pattern, cols, ignore.case = TRUE, perl = TRUE)]
        if (length(matches) > 0) {
            return(matches[[1]])
        }
    }

    stop(
        paste0(
            "Could not determine ",
            output_name,
            " from ",
            data_label,
            " columns. Available columns: ",
            paste(utils::head(cols, 20), collapse = ", ")
        ),
        call. = FALSE
    )
}
