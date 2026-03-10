box::use(
    shiny[NS, tagList, tags, uiOutput, moduleServer, renderUI, reactive,
        actionButton, icon, observe, updateActionButton],
    bsplus[bs_modal, bs_modal_closebutton, bs_attach_modal],
    glue[glue],
    shinyjs[disabled, toggleState]
)

#' @export
ui <- function(
    id,
    button_icon = "link",
    button_class = "",
    ...
) {
    ns <- NS(id)

    tagList(
        bs_modal(
            id = ns("analyte-links-modal"),
            title = tags$h4("Analyte Resources"),
            size = "small",
            body = list(
                uiOutput(ns("analyte_links"))
            ),
            footer = bs_modal_closebutton(label = "Close")
        ),
        disabled(
            actionButton(
                inputId = ns("open"),
                label = "Learn more",
                class = button_class,
                icon = icon(button_icon)
            )
            ) |>
        bs_attach_modal(id_modal = ns("analyte-links-modal"))
    )
}

#' @export
server <- function(id, analyte, ...) {

    moduleServer(id, function(input, output, session) {

        external_links <- data.frame(
            name = c("Pubmed", "GeneCards", "GTEx", "NCBI", "Wikipedia"),
            url = c(
                "https://www.ncbi.nlm.nih.gov/pubmed/?term=",
                "https://www.genecards.org/Search/Keyword?queryString=",
                "https://www.gtexportal.org/home/gene/",
                "https://www.ncbi.nlm.nih.gov/gene/?term=",
                "https://en.wikipedia.org/w/index.php?search="
            ),
            stringsAsFactors = FALSE
        )

        analyte_label <- reactive({
            analyte_value <- analyte()

            if (is.null(analyte_value) || length(analyte_value) == 0) {
                return(NULL)
            }

            first_value <- as.character(analyte_value[[1]])
            if (!nzchar(first_value)) {
                return(NULL)
            }

            trimws(strsplit(first_value, "[|,;]")[[1]][1])
        })

        observe({
            is_available <- !is.null(analyte_label())
            label <- if (is_available) {
                glue("Learn more about {analyte_label()}")
            } else {
                "Learn more"
            }

            updateActionButton(
                session = session,
                inputId = "open",
                label = label
            )

            toggleState(id = "open", condition = is_available)
        })

        output$analyte_links <- renderUI({
            if (is.null(analyte_label())) {
                return(tags$p("Select an analyte to see related resources."))
            }

            encoded_analyte <- utils::URLencode(analyte_label(), reserved = TRUE)

            links_ui <- lapply(seq_len(nrow(external_links)), function(i) {
                link_name <- external_links$name[[i]]
                href <- paste0(external_links$url[[i]], encoded_analyte)

                tagList(
                    tags$a(
                        href = href,
                        target = "_blank",
                        rel = "noopener noreferrer",
                        icon("external-link-alt"),
                        link_name
                    ),
                    tags$br()
                )
            })

            tagList(links_ui)
        })
    })
}
