#' Create layout / skeleton / objects for TrisomExploreR cohort explorer
#' @param id - string - id for this module namespace
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom shinydashboardPlus box
#' @importFrom leaflet leafletOutput
#' @return ui module
#' @export
cohort_explorer_ui <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      cohort_explorer_inputs_ui(ns("inputs"),  ...),
      cohort_explorer_kpi_ui(ns("kpi")),
      shiny::tags$div(
        style = "max-height:75vh; overflow-y: scroll;",
        shiny::fluidRow(
          shiny::column(
            width = 12, class = "col-lg-6",
            shiny::tags$div(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("SexesOverview"),
                  height = "400px"
                )
              )
            ) |>
            bsplus::bs_embed_tooltip(
              title = "Breakdown of controls and T21 by sex",
              placement = "top",
              html = TRUE
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-6",
            shiny::tags$div(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("AgeDistributionOverview"),
                  height = "400px"
                )
              )
            ) |>
            bsplus::bs_embed_tooltip(
              title = "Participants ages are represented at the time of enrollment",
              placement = "top",
              html = TRUE
            )
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shiny::fluidRow(
          shiny::column(
            width = 12, class = "col-lg-3",
            shiny::tags$div(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("Probands"),
                  height = "400px"
                )
              )
            ) |>
            bsplus::bs_embed_tooltip(
              title = "The family structure in relation to the participant with T21.<br />The participant may be shown in more than one category.<br />Only T21 are represented, and related controls are not shown.",
              placement = "top",
              html = TRUE
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-5",
            shiny::tags$div(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("SamplesAvailable"),
                  height = "400px"
                )
              )
            ) |>
            bsplus::bs_embed_tooltip(
              title = "Total number of participants by sample type",
              placement = "top",
              html = TRUE
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-4",
            shiny::tags$div(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("OmicsSamplesAvailable"),
                  height = "400px"
                )
              )
            ) |>
            bsplus::bs_embed_tooltip(
              title = "Analyses Available within the Human Trisome Project",
              placement = "top",
              html = TRUE
            )
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        shiny::fluidRow(
          shiny::column(
            width = 12, class = "col-lg-6",
            shiny::tags$div(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("RaceEthnicityChart"),
                  height = "600px"
                )
              )
            ) |>
            bsplus::bs_embed_tooltip(
              title = "Current demographic information for participants in the database",
              placement = "top",
              html = TRUE
            )
          ),
          shiny::column(
            width = 12, class = "col-lg-6",
            shinydashboardPlus::box(
              title = shiny::HTML(
                'State of Residence
                <span data-toggle="tooltip"
                data-placement="auto right"
                title=""
                class="fas fa-info-circle gtooltip info-tooltip"
                data-original-title="
                State where participants lived at time of enrollment.
                Enrollment centers on Colorado, with higher enrollment numbers from states hosting National Down Syndrome Congress
                Conferences when HTP enrollment occurred.">
                </span>'
              ),
              id = "map",
              height = "600px",
              width = NULL,
              closable = FALSE,
              solidHeader = FALSE,
              collapsible = FALSE,
              headerBorder = FALSE,
              shinycssloaders::withSpinner(
                leaflet::leafletOutput(
                  ns("ParticipantStates"),
                  height = "500px"
                )
              )
            )
          )
        ),
        shiny::tags$hr(style = "margin-top:5px;margin-bottom:10px;")
      )
    )
  )

}

#' Server-side logic / processing for TrisomExploreR cohort explorer
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic to be utilized by all sub-modules
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom gargoyle init
#' @importFrom gargoyle watch
#' @importFrom plotly renderPlotly
#' @importFrom leaflet renderLeaflet
#' @export
cohort_explorer_server <- function(id, r6, ...) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    gargoyle::init("get_cohort_data", session = session)

    cohort_explorer_inputs_server(id = "inputs", r6 = r6, ...)

    cohort_explorer_kpi_server(id = "kpi", r6 = r6)

    dataWithFilters <- shiny::eventReactive(
      c(gargoyle::watch("get_cohort_data")), {
      r6$getParticipantData()
      r6$ParticipantData
    })

    output$SexesOverview <- plotly::renderPlotly({
      dataWithFilters() |> r6$getSexesOverviewPlot()
    })

    output$AgeDistributionOverview <- plotly::renderPlotly({
      dataWithFilters() |> r6$getAgeDistributionOverviewPlot()
    })

    output$Probands <- plotly::renderPlotly({
      dataWithFilters() |> r6$getProbandsPlot()
    })

    output$SamplesAvailable <- plotly::renderPlotly({
      dataWithFilters() |>  r6$getSamplesAvailablePlot()
    })

    output$OmicsSamplesAvailable <- plotly::renderPlotly({
      dataWithFilters() |> r6$getOmicsSamplesAvailablePlot()
    })

    output$RaceEthnicityChart <- plotly::renderPlotly({
      dataWithFilters() |> r6$getRaceEthnicityPlot()
    })

    output$ParticipantStates <- leaflet::renderLeaflet({
      dataWithFilters() |> r6$getParticipantStatesPlot()
    })

  })

}
