#' @export
proteome_overview_ui <- function(id, ...) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12, class = "col-lg-12",
        shinydashboardPlus::box(
          id = NS(id,"Overview"),
          title = "",
          height = "auto",
          width = 12,
          closable = FALSE,
          solidHeader = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          tags$img(
            src = "www/LindaCrnicInstituteLogo.png",
            width = "auto",
            height = "115px",
            style = "margin: auto;display:block"
          ),
          tags$br(),
          tags$div(
            class = "overviewHeader",
            tags$h2("WHAT IS THE PROTEOME?")
          ),
          tags$div(
            class = "overviewBodyText",
            tags$p(
              tags$a(
                href = "https://www.nih.gov/news-events/nih-research-matters/revealing-human-proteome",
                target = "_blank",
                tags$b("Proteomics"),
                .noWS = c("outside")
              ),
              tags$html(" is the study of all proteins produced by a tissue or organism. 
                  In biology, proteins are the functional molecules that result when a gene is expressed and that perform most of the biological processes needed for life. 
                  Many factors can affect the expression and function of proteins, and abnormal protein expression or function can play a role in health and disease. 
                  Therefore, by measuring the levels of many different proteins (i.e. the &ldquo;proteome&rdquo;), 
                  we can understand differences in protein levels and explore how those differences may affect health conditions. 
                  We used proteomics to compare proteins in plasma from blood between people with and without Down syndrome; 
                  we also made this comparison in people with Down syndrome with and without various co-occurring health conditions."
              )
            ),
            tags$br(),
            tags$div(
              class = "overviewHeader",
              tags$h3("OVERVIEW")
            ),
            tags$div(
              class = "overviewBodyText",
              tags$html("This dashboard contains data generated from plasma using two different platforms: 
                  SOMAscan&copy; of over 3500 proteins and Meso Scale Discovery&copy; assays of 55 cytokines and chemokines. "
              ),
              tags$p(
                tags$b("Effect of trisomy 21"),
                tags$p("Explore the differences in protein levels between samples with trisomy 21 and those without (controls). 
                  The data are searchable by protein name and can be filtered by platform, age at time of blood collection, and sex. Significant differences between karyotypes (p<0.05) are indicated within the selected parameters."
                )
              ),
              tags$p(
                tags$b("Effect of Comorbidity in Trisomy 21."),
                tags$p("Explore the differences in immune cytokines between trisomy 21 samples with and without common comorbidities associated with Down syndrome. 
                  The data are searchable by gene name and can be filtered by comorbidity, age at time of blood collection, and sex. 
                  Significant differences between those with and without the specified comorbidities (p<0.05) are indicated within the selected parameters."
                ),
                 tags$br(), 
                 tags$p("Note that the conditions are displayed on the graph using 'OR' logic. 
                  For example, selecting 'Alopecia areata' and 'Celiac disease' will result in a graph showing participants that have either condition, 
                  not only those with both conditions, compared with those with neither condition.")
              ),
              tags$p(
                tags$b("METHODS"),
                tags$p("SomaScan&copy; data was generated by SomaLogic&copy;. Meso Scale Discovery&copy; 
                  data was generated using the Meso Scale Discovery V-PLEX 54-PLEX Human Cytokine Kit and a U-PLEX custom array. 
                  All comparisons are reported using a Kolmogorov-Smirnov Test (K-S test) and have not been corrected for multiple comparisons. 
                  For full methods and analysis, please see associated publications."
                )
              )
            ),
            tags$br(),
            tags$div(
              class = "overviewHeader",
              tags$h3("DATASETS AND PUBLICATIONS")
            ),
            tags$div(
              class = "overviewBodyText",
              tags$p(
                tags$a(
                  href = "https://www.nature.com/articles/s41598-017-13858-3",
                  target = "_blank",
                  tags$b("Trisomy 21 causes changes in the circulating proteome indicative of chronic autoinflammation."),
                  .noWS = c("outside")
                ),
                tags$html("Sullivan KD, Evans D, Pandey A, Hraha TH, Smith KP, Markham N, Rachubinski AL, Wolter-Warmerdam K, Hickey F, Espinosa JM, Blumenthal T. Scientific Reports. 2017 Nov 1;7(1):14818. PMID: 29093484.
                </div>  ")
              )
            )
          )
        )
      )
    )
  )

}

#' @export
proteome_overview_server <- function(input,output,session, id) {

}
