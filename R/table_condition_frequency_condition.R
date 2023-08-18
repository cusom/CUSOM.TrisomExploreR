#' Create condition frequency table output for TrisomExploreR Clinical data analysis
#' @param id - string - id for this module namespace
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT dataTableOutput
#' @export
condition_frequency_condition_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h3("Choose (up to 5) Specific Conditions"),
    shinycssloaders::withSpinner(
      DT::dataTableOutput(
        ns("Table"),
        height = "420px"
      )
    ),
    shinycssloaders::withSpinner(
      DT::dataTableOutput(
        ns("Summary"),
        height = "auto"
      )
    )
  )
}

#' Server logic for condition frequency table output for TrisomExploreR Clinical data analysis
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @import dplyr
#' @import DT
#' @importFrom gargoyle watch trigger
#' @importFrom stats quantile
#' @importFrom grDevices colorRampPalette
#' @export
condition_frequency_condition_table_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    childConditions <- shiny::eventReactive(
      c(gargoyle::watch("get_child_conditions")), {
      r6$updateConditions()
      r6$updateSummaryConditionCounts()
      r6$conditionCounts
    }, ignoreInit = TRUE)

    childConditionSummary <- shiny::eventReactive(
      c(gargoyle::watch("get_child_conditions_summary")), {
      r6$conditionSummary
    }, ignoreInit = TRUE)

    output$Table <- DT::renderDataTable({

      shiny::validate(
        shiny::need(!is.null(childConditions()), "")
      )

      DT::datatable(
        data = childConditions(),
        rownames = FALSE,
        options = list(
          dom = "t",
          autowidth = TRUE,
          columnDefs = list(
            list(targets = c(0), visible = TRUE, width = "50%"),
            list(targets = c(1), visible = TRUE, width = "25%"),
            list(targets = c(2), visible = TRUE, width = "25%")
          ),
          scrollY = "400px",
          pageLength = 150,
          select = list(
            style = "multi",
            selector = "td:not(.notselectable)"
          )
        ),
        extensions = "Select",
        selection = "none",
        callback = DT::JS(
          paste0(
            "table.on('click', 'tbody tr', function() {
              limitDataTableSelections(table, 5,'", ns("Table_rows_selected"), "','",ns("SelectedConditions"), "');
            });"
          )
        )
      ) |>
      DT::formatStyle(
        columns = colnames(childConditions()),
        fontSize = "80%"
      ) |>
      DT::formatStyle(
        "Control %",
        backgroundColor = DT::styleInterval(
          stats::quantile(
            childConditions()$`Control %`,
            probs = seq(.05, .95, .05),
            na.rm = TRUE
          ),
          grDevices::colorRampPalette(c("#f2f2f3", "#BBBDC0"))(20)
        )
      ) |>
      DT::formatStyle(
        "Trisomy 21 %",
        backgroundColor = styleInterval(
          stats::quantile(
            childConditions()$`Trisomy 21 %`,
            probs = seq(.05, .95, .05),
            na.rm = TRUE
          ),
          grDevices::colorRampPalette(c("#E9F1F6", "#287BA5"))(20)
        )
      ) |>
      DT::formatRound(
        columns = c("Control %", "Trisomy 21 %")
      )

    }, server = FALSE)


    output$Summary <- DT::renderDataTable({

      shiny::validate(
        shiny::need(!is.null(childConditionSummary()), "")
      )

      DT::datatable(
        data = childConditionSummary(),
        rownames = FALSE,
        colnames = c("", "", ""),
        options = list(
          dom = "t",
          ordering = FALSE,
          autowidth = FALSE,
          columnDefs = list(
            list(targets = c(0), visible = TRUE, width = "50%"),
            list(targets = c(1), visible = TRUE, width = "25%"),
            list(targets = c(2), visible = TRUE, width = "25%")
          )
        )
      ) |>
      DT::formatStyle(
        columns = colnames(childConditionSummary()),
        fontWeight = "bold",
        fontSize = "100%"
      ) |>
      DT::formatPercentage(
        columns = colnames(childConditionSummary())[2:3],
        digits = 2
      ) |>
      DT::formatStyle(
        "Trisomy 21 %",
        color = "#287BA5"
      )

    })

    shiny::observeEvent(input$Table_rows_selected, {

      s <- input$Table_rows_selected

      r6$conditions <- r6$conditionCounts[s, 1] |> dplyr::pull()

      r6$updateSummaryConditionCounts()

      gargoyle::trigger("get_child_conditions_summary")
      gargoyle::trigger("get_condition_plot")

    })

  })


}
