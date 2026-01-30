box::use(
    shiny[HTML],
    glue[glue],
    dplyr[all_of]
)

#' @export
createTooltip <- function(Text, URL, TooltipText, ShowTooltip = TRUE, ...) {

    if (!ShowTooltip) {
        return(
            HTML(
                glue("<div>{Text}</div>")
            )
        )
    } else {

        if (URL != "" && !is.na(URL)) {
            return(
                HTML(
                    glue(
                        "<div>{Text}
                            <span
                                data-html=\"true\"
                                onclick=\"window.open(\'{URL}\');\"
                                data-toggle=\"tooltip\"
                                data-placement=\"auto right\"
                                title=\"\"
                                class=\"fas fa-info-circle gtooltip info-tooltip\"
                                data-original-title=\"{TooltipText}\">
                            </span>
                        </div>"
                    )
                )
            )
        } else {
            return(
                HTML(
                    glue(
                        "<div>{Text}
                            <span
                                data-html=\"true\"
                                data-toggle=\"tooltip\"
                                data-placement=\"auto right\"
                                title=\"\"
                                class=\"fas fa-info-circle gtooltip info-tooltip\"
                                data-original-title=\"{TooltipText}\">
                            </span>
                        </div>"
                    )
                )
            )
        }
    }
}