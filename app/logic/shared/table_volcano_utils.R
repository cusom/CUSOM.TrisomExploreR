normalize_column_label <- function(label) {
    normalized_label <- tolower(label)
    normalized_label <- gsub("<[^>]*>", "", normalized_label, perl = TRUE)
    normalized_label <- gsub("&[a-z]+;", "", normalized_label, perl = TRUE)
    normalized_label <- gsub("[^[:alnum:]]", "", normalized_label, perl = TRUE)

    normalized_label
}


find_matching_columns <- function(columns, label, exclude_log = FALSE) {
    normalized_columns <- normalize_column_label(columns)
    normalized_label <- normalize_column_label(label)

    candidates <- columns[normalized_columns == normalized_label]

    if (length(candidates) == 0) {
        candidates <- columns[grepl(normalized_label, normalized_columns, fixed = TRUE)]
    }

    if (exclude_log && length(candidates) > 0) {
        candidates <- candidates[
            !grepl("log", normalize_column_label(candidates), fixed = TRUE)
        ]
    }

    candidates
}


resolve_unique_column <- function(columns, label, column_role, exclude_log = FALSE) {
    candidates <- find_matching_columns(columns, label, exclude_log)

    if (length(candidates) == 1) {
        return(list(column = candidates[[1]], error = NULL))
    }

    if (length(candidates) == 0) {
        return(list(column = NULL, error = paste0("Unable to find ", column_role, ".")))
    }

    list(column = NULL, error = paste0("Unable to uniquely identify ", column_role, "."))
}


resolve_fold_change_column <- function(columns, fold_change_label) {
    resolve_unique_column(
        columns = columns,
        label = fold_change_label,
        column_role = "fold-change column"
    )
}


resolve_significance_column <- function(columns, adjusted) {
    exp_sig_col <- ifelse(adjusted, "q-value", "p.value")

    resolve_unique_column(
        columns = columns,
        label = exp_sig_col,
        column_role = "significance column",
        exclude_log = TRUE
    )
}


parse_significance_cutoff <- function(significance_level) {
    if (significance_level == "all") {
        return(1)
    }

    as.numeric(sub(".*&le;\\s*", "", significance_level))
}


filter_volcano_table_data <- function(
    summary_df,
    fold_change_column,
    significance_column,
    fold_change_range,
    p_cut
) {
    in_range <- summary_df[[fold_change_column]] >= fold_change_range[1] &
        summary_df[[fold_change_column]] <= fold_change_range[2]

    is_significant <- summary_df[[significance_column]] <= p_cut

    keep_rows <- in_range & is_significant
    keep_rows[is.na(keep_rows)] <- FALSE

    summary_df[keep_rows, , drop = FALSE]
}

#' @export
get_fold_change_column_label <- function(summary_df) {
    fold_change_candidates <- c("log2FoldChange", "log2(Fold Change)", "logFC", "log2FC", 
        "log2 fold change", "log fold change", "Fold Change", "fold change", "log(fold change)", "logfc", "logfc")

    for (candidate in fold_change_candidates) {
        matching_columns <- find_matching_columns(colnames(summary_df), candidate, exclude_log = TRUE)
        if (length(matching_columns) > 0) {
            return(matching_columns[[1]])
        }
    }

    NULL
}

#' @export
get_significance_column_label <- function(summary_df) {
    significance_candidates <- c("q-value", "q.value" , "p.value", "p-value", "p")

    for (candidate in significance_candidates) {
        matching_columns <- find_matching_columns(colnames(summary_df), candidate, exclude_log = TRUE)
        if (length(matching_columns) > 0) {
            return(matching_columns[[1]])
        }
    }

    NULL
}

#' @export
get_fold_change_slider_settings <- function(summary_df, fold_change_label) {
    fold_change_column <- resolve_fold_change_column(colnames(summary_df), fold_change_label)

    if (!is.null(fold_change_column$error)) {
        return(list(error = fold_change_column$error))
    }

    fc_values <- summary_df[[fold_change_column$column]]
    finite_fc_values <- fc_values[is.finite(fc_values)]

    if (length(finite_fc_values) == 0) {
        return(list(error = "No finite fold-change values available."))
    }

    lim <- as.integer(ceiling(max(abs(finite_fc_values), na.rm = TRUE)))
    step <- round(1 / max((lim * 2), 1), 1)

    list(
        error = NULL,
        min = -lim,
        max = lim,
        step = step,
        value = c(-lim, lim)
    )
}


#' @export
prepare_volcano_table_data <- function(
    summary_df,
    fold_change_label,
    significance_level,
    fold_change_range,
    adjusted
) {
    fold_change_column <- resolve_fold_change_column(colnames(summary_df), fold_change_label)
    if (!is.null(fold_change_column$error)) {
        return(list(error = fold_change_column$error, data = NULL))
    }

    significance_column <- resolve_significance_column(colnames(summary_df), adjusted)
    if (!is.null(significance_column$error)) {
        return(list(error = significance_column$error, data = NULL))
    }

    p_cut <- parse_significance_cutoff(significance_level)

    list(
        error = NULL,
        data = filter_volcano_table_data(
            summary_df = summary_df,
            fold_change_column = fold_change_column$column,
            significance_column = significance_column$column,
            fold_change_range = fold_change_range,
            p_cut = p_cut
        )
    )
}