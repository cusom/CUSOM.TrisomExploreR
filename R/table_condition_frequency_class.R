#' Create condition class frequency table output for TrisomExploreR Clinical data analysis
#' @param id - string - id for this module namespace
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT dataTableOutput
#' @export
condition_frequency_class_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML(
      '<h3>Choose (up to 3) Condition Classes
        <span data-toggle="tooltip"
          data-placement="auto right"
          title=""
          class="fas fa-info-circle gtooltip info-tooltip"
          data-original-title="
          Overall rates of reported clinical conditions have remained consistent, however it is important to
          note that not all participants in the database have clinical data abstracted.
          Condition class represents groups of conditions. When a Condition Class is selected,
          the male/female ratio of those with a diagnosis in the selected Condition Class is reported below.
          After selecting one Condition Class, the specific conditions that comprise the condition
          class appear in the right-hand column in alphabetical order. Some conditions, such as
          &lsquo;Congenital heart defect - any&rsquo; are comprised of other conditions listed out separately in the same group.
          If more than one specific condition is selected, only participants with all selected conditions appear in the reported total.">
        </span>
      </h3>'
    ),
    shinycssloaders::withSpinner(
      DT::dataTableOutput(
        ns("TopConditionsTable"),
        height = "420px"
      )
    ),
    shinycssloaders::withSpinner(
      DT::dataTableOutput(
        ns("TopConditionsSummary"),
        height = "auto"
      )
    )

  )
}

#' Server logic for condition class frequency table output for TrisomExploreR Clinical data analysis
#' @param id - string - id for this module namespace
#' @param r6 - R6 class defining server-side logic
#' @import dplyr
#' @import DT
#' @importFrom gargoyle trigger
#' @importFrom stats quantile
#' @importFrom grDevices colorRampPalette
#' @export
condition_frequency_class_table_server <- function(id, r6) {

  shiny::moduleServer(id, function(input, output, session) {

   ns <- session$ns

   output$TopConditionsTable <- DT::renderDataTable({

    shiny::validate(
      shiny::need(!is.null(r6$conditionClassCounts), "")
    )

    DT::datatable(
      data = r6$conditionClassCounts,
      rownames = FALSE,
      options = list(
        dom = "t",
        autowidth = TRUE,
        columnDefs = list(
          list(targets = c(0), visible = TRUE, width = "50%"),
          list(targets = c(1), visible = TRUE, width = "25%"),
          list(targets = c(2), visible = TRUE, width = "25%")
        ),
        scrollX = TRUE,
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

            setTimeout(function() {

              var selectedindexes = table.rows({selected:true}).indexes();
              var selectedindices = Array(selectedindexes.length);
              var unselectedindexes = table.rows({selected:false}).indexes();
              var unselectedindices = Array(unselectedindexes.length);

              for(var i = 0; i < selectedindices.length; ++i){
                selectedindices[i] = selectedindexes[i]+1;
              }

              for(var i = 0; i < unselectedindices.length; ++i){
                unselectedindices[i] = unselectedindexes[i];
              }

              if(selectedindexes.length <= 3) {

                table.$('td:first-child').each(function() {

                  $(this).removeClass('notselectable');
                  $(this).removeClass('selectable');

                });

                Shiny.setInputValue('",ns("TopConditionsTable_rows_selected"),"', selectedindices);

                if(selectedindexes.length == 0) {
                  Shiny.setInputValue('",ns("SelectedConditionClasses"),"',null,{priority:'event'});
                }

              }

              if(selectedindexes.length == 3) {

                table.$('td:first-child').each(function() {

                  if($.inArray($(this)[0]._DT_CellIndex.row,unselectedindices)!= -1) {

                    $(this).removeClass('notselectable');
                    $(this).removeClass('selectable');
                    $(this).addClass('notselectable');

                  }

                });
              }

              if(selectedindexes.length > 3 ) {

                table.$('td:first-child').each(function() {

                  if($.inArray($(this)[0]._DT_CellIndex.row,unselectedindices)!= -1) {

                    $(this).removeClass('selectable');
                    $(this).addClass('notselectable');

                  }

                });

              }

            }, 0);

          });
          "
        )
      )
    ) |>
    DT::formatStyle(
      columns = colnames(r6$conditionClassCounts),
      fontSize = "80%"
    ) |>
    DT::formatStyle(
      "Control %",
      backgroundColor = DT::styleInterval(
        stats::quantile(
          r6$conditionClassCounts$`Control %`,
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
          r6$conditionClassCounts$`Trisomy 21 %`,
          probs = seq(.05, .95, .05),
          na.rm = TRUE
        ),
        grDevices::colorRampPalette(c("#E9F1F6", "#287BA5"))(20)
      )
    ) |>
      DT::formatRound(
        columns = c("Control %","Trisomy 21 %")
      )

   }, server = FALSE)

   # Observe Table Row clicks -> update Condition Class Inputs
   shiny::observeEvent(input$TopConditionsTable_rows_selected, {

     s <- input$TopConditionsTable_rows_selected

     #SelectedConditionClasses <- r6$conditionClassCounts[s,1] |> pull()
     r6$conditionClasses <- r6$conditionClassCounts[s, 1] |> dplyr::pull()

     #update list of condition
     r6$updateSummaryConditionCounts()

     gargoyle::trigger("get_child_conditions")
     gargoyle::trigger("get_child_conditions_summary")

   })

   conditionClassSummary <- shiny::eventReactive(
    c(input$GetConditionClassSummary, input$TopConditionsTable_rows_selected), {
      r6$updateSummaryConditionClassCounts()
      r6$conditionClassSummary
   })

   # Condition Class Total Line ####
   output$TopConditionsSummary <- DT::renderDataTable({

    DT::datatable(
      data = conditionClassSummary(),
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
      columns = colnames(conditionClassSummary()),
      fontWeight = "bold",
      fontSize = "100%"
    ) |>
    DT::formatPercentage(
      columns = colnames(conditionClassSummary())[2:3],
      digits = 2
    ) |>
    DT::formatStyle(
      "Trisomy 21 %",
      color = "#287BA5"
    )

   })

  })

}
