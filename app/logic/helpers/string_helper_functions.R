box::use(
    stringr[str_split],
    stringi[stri_replace_all_regex]
)


#' @export
parse_delimited_string <- function(
    string,
    position = 1,
    patterns = "[,,.,;,:]+"
    ) {

    clean_string <- stri_replace_all_regex(
        string,
        patterns,
        "|",
        vectorize_all = FALSE
    )

    return(
        str_split(clean_string, "\\|", simplify = TRUE)[position]
    )

}