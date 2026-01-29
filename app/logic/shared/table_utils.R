box::use(
    dplyr[select, rename_with]
)


#' @export
format_summary_data <- function(.data, adjusted = TRUE, cols_to_drop = c("")) {

    p_val_label <- ifelse(adjusted, "q-value", "p-value")
    log_10_p_val_label <- ifelse(adjusted, "-log<sub>10</sub>(q-value)", "-log<sub>10</sub>(p-value)")

    old_names <- c("log2FoldChange", "p.value.adjustment.method", "p.value.original",
        "FoldChange", "p.value", "-log10pvalue", "lmFormula"
    )
    new_names <- c("log<sub>2</sub>(Fold Change)", "adjustment method", "p-value (original)",
        "Fold Change", p_val_label, log_10_p_val_label, "model"
    )

    return(
        .data |>
            rename_with(~ new_names, all_of(old_names)) |>
            select(-cols_to_drop)
    )

}
