#' @export
condition_frequency_condition_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3('Choose (up to 5) Specific Conditions'),
    shinycssloaders::withSpinner(
      DT::dataTableOutput(
        ns("ChildConditionsTable"),
        height = "420px"
      )
    ),
    shinycssloaders::withSpinner(
      DT::dataTableOutput(
        ns("ChildConditionsSummary"),
        height="auto"
      )
    )
  )
}

#' @export
condition_frequency_condition_table_server <- function(id, r6) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    childConditions <- eventReactive(c(gargoyle::watch("get_child_conditions")),{
      r6$updateConditions()
      r6$updateSummaryConditionCounts()
      r6$conditionCounts
    },ignoreInit = TRUE)

    childConditionSummary <- eventReactive(c(gargoyle::watch("get_child_conditions_summary")),{
      r6$conditionSummary
    },ignoreInit = TRUE)

    output$ChildConditionsTable <- DT::renderDataTable({

      validate(
        need(!is.null(childConditions()),"")
      )

      formattable::as.datatable(
        formattable::formattable(
          childConditions(),
          list(
            formattable::area(col = 2) ~ formattable::color_tile("#f2f2f3","#BBBDC0"),
            formattable::area(col = 3) ~ formattable::color_tile("#E9F1F6", "#287BA5")
          )
        ),
        callback = DT::JS(
          paste0(
          "table.on('click', 'tbody tr', function(){

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

              if(selectedindexes.length <= 5) {

                table.$('td:first-child').each(function() {

                  $(this).removeClass('notselectable');
                  $(this).removeClass('selectable');

                });

                Shiny.setInputValue('",ns("ChildConditionsTable_rows_selected"),"',selectedindices);

                if(selectedindexes.length == 0) {

                  Shiny.setInputValue('SelectedConditions',null,{priority:'event'});

                }

              }

              if(selectedindexes.length == 5) {

                table.$('td:first-child').each(function() {

                  if($.inArray($(this)[0]._DT_CellIndex.row,unselectedindices)!= -1) {

                    $(this).removeClass('notselectable');
                    $(this).removeClass('selectable');
                    $(this).addClass('notselectable');

                  }

                });

              }

              if(selectedindexes.length >5) {

                table.$('td:first-child').each(function() {

                  if($.inArray($(this)[0]._DT_CellIndex.row,unselectedindices)!= -1) {

                    $(this).removeClass('selectable');
                    $(this).addClass('notselectable');

                  }

                });

              }

            }, 0);

          });"
        )),
        rownames = FALSE,
        options = list(
          dom = 't',
          autowidth=TRUE,
          columnDefs = list(
            list(targets = c(0), visible = TRUE, width = '50%'),
            list(targets = c(1), visible = TRUE, width = '25%'),
            list(targets = c(2), visible = TRUE, width = '25%')
          ),
          scrollX = TRUE,
          scrollY = '400px',
          pageLength = 150
          ,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'font-size': '80%'});",
            "}"
          ),
          select = list(style = "multi", selector = "td:not(.notselectable)")
        ),
        extensions = "Select",
        selection = "none"
      )

    }, server = FALSE)


    output$ChildConditionsSummary <- DT::renderDataTable({

      validate(
        need(!is.null(childConditionSummary()),"")
      )

      formattable::as.datatable(
        formattable::formattable(
          childConditionSummary(),
          list(
            Condition = formattable::formatter("span", style = ~ formattable::style(font.weight = "bold")),
            `Control %` = formattable::formatter("span", x ~ scales::percent(x/100), style = ~ formattable::style(font.weight = "bold")),
            `Trisomy 21 %` = formattable::formatter("span", x ~ scales::percent(x/100), style = ~ formattable::style(font.weight = "bold",color="#287BA5"))
          )
        )
        ,rownames = FALSE
        ,colnames = c("", "", "")
        ,options = list(
          dom = 't',
          ordering=FALSE,
          columnDefs = list(
            list(targets=c(0), visible=TRUE, width='50%'),
            list(targets=c(1), visible=TRUE, width='25%'),
            list(targets=c(2), visible=TRUE, width='25%')

          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'font-size': '100%'});",
            "}"
          )
        )
      )

    })

    observeEvent(input$ChildConditionsTable_rows_selected,{

      s <- input$ChildConditionsTable_rows_selected

      r6$conditions <- r6$conditionCounts[s,1] |> dplyr::pull()

      r6$updateSummaryConditionCounts()

      gargoyle::trigger("get_child_conditions_summary")
      gargoyle::trigger("get_condition_plot")

    })

  })


}