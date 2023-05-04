#' @export
clinical_overview_ui <- function(id, ...) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12, class = "col-lg-12",
        shinydashboardPlus::box(
          id = NS(id, "Overview"),
          title = "",
          height = "auto",
          width = 12,
          closable = FALSE,
          solidHeader = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          shiny::tags$img(
            src = "www/LindaCrnicInstituteLogo.png",
            width = "auto",
            height = "115px",
            style = "margin: auto;display:block"
          ),
          tags$br(),
          tags$div(
            class = "overviewHeader",
            tags$h2("Overview")
          ),
          tags$div(
            class = "overviewBodyText",
            tags$p("
              Clinical data is presented in multiple formats for exploration. 
              Sample numbers and additional data are available on hover. 
              The data can be filtered by karyotype, sex, and age at enrollment. 
              When data is filtered, all graphs are reactive.    
            "
            ),
            tags$br(),
            tags$div(
              class = "overviewBodyText",
              tags$b("Cohort Overview"),
              tags$p(" tab presents 
                    1) basic demographics of participants, 
                    2) sample types available for participants currently in the database and 
                    3) number of samples with -omics analyses available."
              ),
              tags$p(
                tags$b("The Conditions Table"),
                tags$p(" displays the most commonly reported co-occurring conditions, shown as a percentage of affected individuals 
                    within typical people (controls) and people with trisomy 21. 
                    In the left-hand column, a Condition Class can be selected and sex ratio of those diagnosed is reported below. 
                    Once a Condition Class is selected, the Specific Conditions that comprise the Class are displayed in the right-hand column. "
                )
              ),
              tags$p(
                tags$b("Conditions Interactions"),
                tags$p(" dashboard displays the co-occurrence of selected conditions as an upset plot. 
                    Users can select various classes of conditions, such as autoimmune conditions, or specific conditions, such as celiac disease or alopecia areata. 
                    Note that a maximum of six co-occurring conditions can be selected at any time."
                )
              )
            ),
            tags$br(),
            tags$div(
              class = "overviewHeader",
              tags$h3("METHODS")
            ),
            tags$div(
              class = "overviewBodyText",
              tags$p("
                Informed consent is first obtained from the participant, or their legal guardian. 
                Clinical data is obtained from participant report and/or the medical record. 
                For samples analyzed by the Human Trisome Project on a de-identified basis, the appropriate data sharing agreements are in place.  
                Note that not all participants have clinical data available, or from both sources, as medical records are continually being abstracted and clinical data updated."
              ),
              tags$br()
            )
          )
        )
      )
    )
  )

}

#' @export
clinical_overview_server <- function(input,output,session, id) {

}
