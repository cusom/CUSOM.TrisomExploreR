box::use(
    R6[R6Class],
    tibble[tibble, deframe],
    dplyr[select, filter, pull],
    vroom[vroom_write],
    writexl[write_xlsx],
    shiny[moduleServer, showModal, modalDialog, tags, tagList, req,
        downloadButton, downloadLink, icon, modalButton, downloadHandler, observeEvent],
    shinybusy[show_modal_spinner, remove_modal_spinner],
    glue[glue],
    shinyWidgets[prettyRadioButtons],
    shinyalert[shinyalert]
)

download_manager <- R6Class(
    "download_manager",
    private = list(
        download_config = tibble(
            file_type = c("Comma-Delimited", "Tab-Delimited",
                "G-Zipped Tab-Delimited", "Excel"),
            delimiter = c(",", "\t", "\t", NA),
            extension = c("csv", "tsv", "tsv.gz", "xlsx"),
            download_method = c(rep("vroom_write", 3), "write_xlsx")
        )
    ),
    active = list(
        avaialable_download_types = function(value) {
            return(
                private$download_config |>
                    select(file_type, extension) |>
                    deframe()
            )
        },
        full_file_name = function(value) {
            return(
                glue("{self$file_name}.{self$file_type}")
            )
        },
        file_delimiter = function(value) {
            return(
                private$download_config |>
                    filter(extension == self$file_type) |>
                    pull(delimiter)
            )
        }
    ),
    public = list(
        file_name = NULL,
        download_data = NULL,
        file_type = NULL,
        initialize = function(file_name, download_data) {
            self$file_name <- file_name
            self$download_data <- download_data
        }
    )
)

#' @export
download_file <- function(id, file_name, download_data, download_btn_label = "Download") {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        download_manager <- download_manager$new(file_name, download_data)

        showModal(
            tags$div(
                id = ns("DataDownloadAlert"),
                modalDialog(
                    title = tags$h3(glue("Download options:")),
                    size = "m",
                    easyClose = TRUE,
                    list(
                        tags$div(
                            style = "text-align:left",
                            prettyRadioButtons(
                                inputId = ns("download_file_type"),
                                label = "Choose file type:",
                                choices = download_manager$avaialable_download_types,
                                status = "success"
                            )
                        )
                    ),
                    footer = tagList(
                        downloadButton(
                            outputId = ns("download"),
                            label = download_btn_label,
                            icon = icon("download"),
                            style = "float:left;"
                        ),
                        modalButton(label = "Cancel")
                    )
                )
            )
        )

        observeEvent(input$download_file_type, {
            download_manager$file_type <- input$download_file_type
        })

        output$download  <- downloadHandler(

            filename = function() {
                download_manager$full_file_name
            },

            content = function(file) {
                req(download_manager$file_type)

                show_modal_spinner(
                    spin  = "hollow-dots",
                    color = "#3c8dbc",
                    text  = "Preparing download..."
                )

                on.exit(remove_modal_spinner())

                if (identical(download_manager$file_type, "xlsx")) {
                    write_xlsx(download_manager$download_data, path = file)
                } else {
                    vroom_write(download_manager$download_data, file = file, delim = download_manager$file_delimiter)
                }

                if (!file.exists(file)) stop("Download failed: output file was not created.")

                shinyalert::shinyalert(
                    title = "Success!",
                    html = TRUE,
                    text = glue("{download_manager$full_file_name} \nDownload complete"),
                    type = "success",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    showCancelButton = FALSE,
                    timer = 1000
                )
            }
        )

    })

}
