#' Create overview page for proteome application
#' @param id - string - id for this module namespace
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom shinydashboardPlus box
#' @export
proteome_overview_ui <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12, class = "col-lg-12",
        shinydashboardPlus::box(
          id = ns("Overview"),
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
          shiny::tags$br(),
          shiny::tags$div(
            class = "overviewHeader",
            shiny::tags$h2("WHAT IS THE PROTEOME?")
          ),
          shiny::tags$div(
            class = "overviewBodyText",
            shiny::tags$p(
              shiny::tags$a(
                href = "https://www.nih.gov/news-events/nih-research-matters/revealing-human-proteome",
                target = "_blank",
                shiny::tags$b("Proteomics"),
                .noWS = c("outside")
              ),
              htmltools::HTML(" is the study of all proteins produced by a tissue or organism.
                  In biology, proteins are the functional molecules that result when a gene is expressed and that perform most of the biological processes needed for life.
                  Many factors can affect the expression and function of proteins, and abnormal protein expression or function can play a role in health and disease.
                  Therefore, by measuring the levels of many different proteins (i.e. the \"proteome\";),
                  we can understand differences in protein levels and explore how those differences may affect health conditions.
                  We used proteomics to compare proteins in plasma from blood between people with and without Down syndrome;
                  we also made this comparison in people with Down syndrome with and without various co-occurring health conditions."
              )
            ),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("OVERVIEW")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              htmltools::HTML("This dashboard contains data generated from plasma using two different platforms:
                  SOMAscan&copy; of over 3500 proteins and Meso Scale Discovery&copy; assays of 55 cytokines and chemokines. "
              ),
              shiny::tags$p(
                shiny::tags$b("Effect of trisomy 21"),
                shiny::tags$p("Explore the differences in protein levels between samples with trisomy 21 and those without (controls).
                  The data are searchable by protein name and can be filtered by platform, age at time of blood collection, and sex. 
                  Significant differences between karyotypes (p<0.05) are indicated within the selected parameters."
                )
              ),
              shiny::tags$p(
                shiny::tags$b("Effect of Comorbidity in Trisomy 21."),
                shiny::tags$p("Explore the differences in immune cytokines between trisomy 21 samples with and without common comorbidities associated with Down syndrome.
                  The data are searchable by gene name and can be filtered by comorbidity, age at time of blood collection, and sex.
                  Significant differences between those with and without the specified comorbidities (p<0.05) are indicated within the selected parameters."
                ),
                 shiny::tags$br(),
                 shiny::tags$p("Note that the conditions are displayed on the graph using \'OR\' logic.
                  For example, selecting \'Alopecia areata\' and \'Celiac disease\' will result in a graph showing participants that have either condition,
                  not only those with both conditions, compared with those with neither condition.")
              ),
              shiny::tags$p(
                shiny::tags$b("METHODS"),
                htmltools::HTML("SomaScan&copy; data was generated by SomaLogic&copy;. Meso Scale Discovery&copy;
                  data was generated using the Meso Scale Discovery V-PLEX 54-PLEX Human Cytokine Kit and a U-PLEX custom array.
                  All comparisons are reported using a Kolmogorov-Smirnov Test (K-S test) and have not been corrected for multiple comparisons.
                  For full methods and analysis, please see associated publications."
                )
              )
            ),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("DATASETS AND PUBLICATIONS")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              shiny::tags$p(
                shiny::tags$a(
                  href = "https://www.nature.com/articles/s41598-017-13858-3",
                  target = "_blank",
                  shiny::tags$b("Trisomy 21 causes changes in the circulating proteome indicative of chronic autoinflammation."),
                  .noWS = c("outside")
                ),
                htmltools::HTML("Sullivan KD, Evans D, Pandey A, Hraha TH, Smith KP, Markham N, Rachubinski AL, Wolter-Warmerdam K, Hickey F, Espinosa JM, Blumenthal T. Scientific Reports. 2017 Nov 1;7(1):14818. PMID: 29093484.")
              )
            )
          )
        )
      )
    )
  )

}

#' overview page for proteome application
#' @param id - string - id for this module namespace
#' @export
proteome_overview_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

  })

}
