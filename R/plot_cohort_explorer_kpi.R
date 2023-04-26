#' @export
cohort_explorer_kpi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12, class = "col-lg-12",
        valueBoxOutput(ns("value1"),width = 3)
        ,shinyBS::bsTooltip(
          id = ns("value1"),
          title = "Total number consented",
          placement = "top",
          trigger = "hover"
        )
        ,valueBoxOutput(ns("value2"),width = 3)
        ,shinyBS::bsTooltip(
          id = ns("value2"),
          title = "Individuals with Trisomy 21 (T21)",
          placement = "top",
          trigger = "hover"
        )
        ,valueBoxOutput(ns("value3"),width = 3)
        ,shinyBS::bsTooltip(
          id = ns("value3"),
          title = "Individuals with other intellectual and/or developmental disability (IDD) and individuals with sex chromosome anomalies (SCA)",
          placement = "top",
          trigger = "hover"
        )
        ,valueBoxOutput(ns("value4"),width = 3)
        ,shinyBS::bsTooltip(
          id = ns("value4"),
          title = "Controls",
          placement = "top",
          trigger = "hover"
        )
      )
    ),
    fluidRow(
      column(
        width = 12, offset = 2, class = "col-lg-10",
         valueBoxOutput(ns("value5"),width = 4)
        ,valueBoxOutput(ns("value6"),width = 4)
        ,shinyBS::bsTooltip(
          id = ns("value6"),
          title = "Total participants matching currently selected filters",
          placement = "top",
          trigger = "hover"
        )
      )
    )
  )
}

#' @export
cohort_explorer_kpi_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$value1 <- renderValueBox({
      valueBox(
        value = formatC(sum(r6$Enrollments$ParticipantCount), format="f", big.mark=",", digits=0),
        subtitle = 'Total consented',
        icon = icon("users",lib='font-awesome'),
        color = "blue",
        width = 3
      )
    })

    output$value2 <- renderValueBox({
      valueBox(
        value = r6$Enrollments |> dplyr::filter(Karyotype == "Trisomy 21") |> dplyr::select(ParticipantCount) |> dplyr::pull(),
        subtitle = 'Trisomy 21 (T21)',
        icon = icon("user-plus",lib='font-awesome'),
        color = "light-blue",
        width = 3
      )
    })

    output$value3 <- renderValueBox({
      valueBox(
        value = -1, #749,
        subtitle = 'Other IDDs',
        icon = icon("user-circle",lib='font-awesome'),
        color = "teal",
        width = 3
      )
    })

    output$value4 <- renderValueBox({
      valueBox(
        value = r6$Enrollments |> dplyr::filter(Karyotype == "Control") |> dplyr::select(ParticipantCount) |> dplyr::pull(),
        subtitle = 'Controls (D21)',
        icon = icon("user-minus",lib='font-awesome'),
        color = "teal",
        width = 3
      )
    })

    output$value5 <- renderValueBox({

      valueBox(
        value = r6$AllN,
        subtitle = 'Total in database',
        icon = icon("database", lib='font-awesome'),
        color = "green",
        width = 3
      )

    })

    participantData <- eventReactive(c(gargoyle::watch("get_cohort_data")),{
      r6$ParticipantData
    })


    output$value6 <- renderValueBox({

      dataframe <- participantData()

      if(is.null(dataframe)) {
        n <- 0
      } else {

        n <- dataframe |>
          dplyr::summarise(n = dplyr::n_distinct(record_id))
      }

      valueBox(
        value = n,
        subtitle = 'Total currently selected',
        icon = icon("hashtag",lib='font-awesome'),
        color = "olive",
        width = 3
      )
    })

  })


}