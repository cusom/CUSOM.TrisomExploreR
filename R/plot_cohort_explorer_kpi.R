#' Create KPI outputs for TrisomExploreR cohort explorer
#' @param id - string - id for this module namespace
#' @importFrom shinydashboard valueBoxOutput
#' @importFrom bsplus bs_embed_tooltip
#' @export
cohort_explorer_kpi_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12, class = "col-lg-12",
        shinydashboard::valueBoxOutput(ns("value1"), width = 3) |>
        bsplus::bs_embed_tooltip(
          title = "Total number consented",
          placement = "top"
        ),
        shinydashboard::valueBoxOutput(ns("value2"), width = 3) |>
        bsplus::bs_embed_tooltip(
          title = "Individuals with Trisomy 21 (T21)",
          placement = "top",
          html = TRUE
        ),
        shinydashboard::valueBoxOutput(ns("value3"), width = 3) |>
        bsplus::bs_embed_tooltip(
          title = "Individuals with other intellectual and/or developmental 
          disability (IDD) and individuals with sex chromosome anomalies (SCA)",
          placement = "top",
          html = TRUE
        ),
        shinydashboard::valueBoxOutput(ns("value4"), width = 3) |>
        bsplus::bs_embed_tooltip(
          title = "Controls",
          placement = "top",
          html = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12, offset = 2, class = "col-lg-10",
        shinydashboard::valueBoxOutput(ns("value5"), width = 4),
        shinydashboard::valueBoxOutput(ns("value6"), width = 4) |>
        bsplus::bs_embed_tooltip(
          title = "Total participants matching currently selected filters",
          placement = "top",
          html = TRUE
        )
      )
    )
  )
}

#' Server for KPI outputs for TrisomExploreR cohort explorer
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @importFrom shinydashboard renderValueBox
#' @importFrom shinydashboard valueBox
#' @importFrom gargoyle watch
#' @export
cohort_explorer_kpi_server <- function(id, r6) {

  Karyotype <- ParticipantCount <- record_id <- NULL

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$value1 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = formatC(
          sum(r6$Enrollments$ParticipantCount),
          format = "f",
          big.mark = ",",
          digits = 0
        ),
        subtitle = "Total consented",
        icon = shiny::icon("users", lib = "font-awesome"),
        color = "blue",
        width = 3
      )
    })

    output$value2 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = r6$Enrollments |>
          dplyr::filter(Karyotype == "Trisomy 21") |>
          dplyr::select(ParticipantCount) |>
          dplyr::pull(),
        subtitle = "Trisomy 21 (T21)",
        icon = shiny::icon("user-plus", lib = "font-awesome"),
        color = "light-blue",
        width = 3
      )
    })

    output$value3 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = -1, #749,
        subtitle = "Other IDDs",
        icon = shiny::icon("user-circle", lib = "font-awesome"),
        color = "teal",
        width = 3
      )
    })

    output$value4 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = r6$Enrollments |>
          dplyr::filter(Karyotype == "Control") |>
          dplyr::select(ParticipantCount) |>
          dplyr::pull(),
        subtitle = "Controls (D21)",
        icon = shiny::icon("user-minus", lib = "font-awesome"),
        color = "teal",
        width = 3
      )
    })

    output$value5 <- shinydashboard::renderValueBox({

      shinydashboard::valueBox(
        value = r6$AllN,
        subtitle = "Total in database",
        icon = shiny::icon("database", lib = "font-awesome"),
        color = "green",
        width = 3
      )

    })

    participantData <- shiny::eventReactive(
      c(gargoyle::watch("get_cohort_data")), {
      r6$ParticipantData
    })


    output$value6 <- shinydashboard::renderValueBox({

      dataframe <- participantData()

      if (is.null(dataframe)) {
        n <- 0
      } else {

        n <- dataframe |>
          dplyr::summarise(n = dplyr::n_distinct(record_id))
      }

      shinydashboard::valueBox(
        value = n,
        subtitle = "Total currently selected",
        icon = shiny::icon("hashtag", lib = "font-awesome"),
        color = "olive",
        width = 3
      )
    })

  })

}
