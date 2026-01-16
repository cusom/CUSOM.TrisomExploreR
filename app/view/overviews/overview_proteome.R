box::use(
  shiny[tags]
)

#' @export
ui <- function(id) {

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
              htmltools::HTML(" is the study of large numbers of proteins produced by a tissue or organism.
              In biology, proteins are the functional molecules that result when a gene is expressed and
              which perform most of the biological processes needed for life. Many factors can affect the
              expression and function of proteins, and abnormal protein expression or function can play a
              role in health and disease. Therefore, by measuring the levels of many different proteins
              (i.e. the \"proteome\"), we can identify differences in protein levels and explore how those
              differences may affect health conditions in people with Down syndrome. We used proteomics to
              compare proteins in plasma from blood samples between people with and without Down syndrome"
              )
            ),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("OVERVIEW")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              tags$p("This dashboard contains data generated from plasma using three different platforms:"),
              tags$ul(
                style = "padding-left:50px;",
                tags$li(
                  shiny::tags$p(
                    htmltools::HTML("Meso Scale Discovery&copy assays of cytokines, chemokines,
                    and other immune markers, as described in "),
                    shiny::tags$a(
                      href = "https://pubmed.ncbi.nlm.nih.gov/37379383/",
                      target = "_blank",
                      "Galbraith et al, 2023",
                      .noWS = c("outside")
                    ),
                    htmltools::HTML(" and "),
                    shiny::tags$a(
                      href = "https://pubmed.ncbi.nlm.nih.gov/31628327/",
                      target = "_blank",
                      "Powers et al, 2019.",
                      .noWS = c("outside")
                    )
                  )
                ),
                tags$li(
                  shiny::tags$p(
                    htmltools::HTML("SIMOA&copy measurements of selected neurodegeneration
                    and neuroinflammation markers, as described in "),
                    shiny::tags$a(
                      href = "https://pubmed.ncbi.nlm.nih.gov/36577365/",
                      target = "_blank",
                      "Araya et al, 2022. ",
                      .noWS = c("outside")
                    )
                  )
                ),
                tags$li(
                  shiny::tags$p(
                    htmltools::HTML("SOMAscan&copy measurements of thousands of proteins, as described in "),
                    shiny::tags$a(
                      href = "https://pubmed.ncbi.nlm.nih.gov/37379383/",
                      target = "_blank",
                      "Galbraith et al, 2023",
                      .noWS = c("outside")
                    ),
                    htmltools::HTML(" and "),
                    shiny::tags$a(
                      href = "https://pubmed.ncbi.nlm.nih.gov/29093484/",
                      target = "_blank",
                      "Sullivan et al, 2017.",
                      .noWS = c("outside")
                    )
                  )
                )
              ),
              tags$br(),
              shiny::tags$div(
                class = "overviewHeader",
                shiny::tags$h3("Effect of trisomy 21")
              ),
              shiny::tags$div(
                class = "overviewBodyText",
                shiny::tags$p(
                  "Explore the differences in protein levels between samples with trisomy 21
                  and those without (controls)."
                ),
                shiny::tags$p("
                  The data are searchable by protein name and can be
                  filtered by platform, age at time of blood collection, and sex."
                ),
                shiny::tags$p("
                  Significant differences
                  between karyotypes (q<0.1, FDR10) are indicated within the selected parameters."
                ),
                tags$br(),
                tags$p("Please click on the information icons for a step-by-step tutorial."),
              )
            ),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("DATASETS AND PUBLICATIONS")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              shiny::tags$div(
                shiny::tags$a(
                  href = "https://www.nature.com/articles/s41598-017-13858-3",
                  target = "_blank",
                  shiny::tags$b("Trisomy 21 causes changes in the circulating proteome
                  indicative of chronic autoinflammation."),
                  .noWS = c("outside")
                ),
                shiny::tags$br(),
                htmltools::HTML(
                  "Sullivan KD, Evans D, Pandey A, Hraha TH, Smith KP, Markham N,
                  Rachubinski AL, Wolter-Warmerdam K, Hickey F, Espinosa JM, Blumenthal T."
                ),
                shiny::tags$p(
                  "Scientific Reports. 2017 Nov 1;7(1):14818. PMID: 29093484."
                )
              ),
              shiny::tags$br(),
              shiny::tags$div(
                shiny::tags$a(
                  href = "https://pubmed.ncbi.nlm.nih.gov/31628327/",
                  target = "_blank",
                  shiny::tags$b("Trisomy 21 activates the kynurenine pathway via increased
                    dosage of interferon receptors"),
                  .noWS = c("outside")
                ),
                shiny::tags$br(),
                htmltools::HTML(
                  "Powers RK, Culp-Hill R, Ludwig MP, Smith KP, Waugh KA, Minter R, Tuttle KD,
                  Lewis HC, Rachubinski AL, Granrath RE, Carmona-Iragui M, Wilkerson RB, Kahn DE,
                  Joshi M, Lle&oacute; A, Blesa R, Fortea J, D'Alessandro A, Costello JC, Sullivan KD, Espinosa JM."
                ),
                shiny::tags$p(
                  "Nature Communications 2019 Oct 18;10(1):4766. doi: 10.1038/s41467-019-12739-9.PMID: 31628327"
                )
              ),
              tags$br(),
              shiny::tags$div(
                shiny::tags$a(
                  href = "https://pubmed.ncbi.nlm.nih.gov/36577365/",
                  target = "_blank",
                  shiny::tags$b("IGF1 deficiency integrates stunted growth and neurodegeneration in Down syndrome"),
                  .noWS = c("outside")
                ),
                shiny::tags$br(),
                htmltools::HTML(
                  "Araya P, Kinning KT, Coughlan C, Smith KP, Granrath RE, Enriquez-Estrada BA, Worek K,
                  Sullivan KD, Rachubinski AL, Wolter-Warmerdam K, Hickey F, Galbraith MD, Potter H, Espinosa JM."
                ),
                shiny::tags$p(
                  "Cell Reports 2022 Dec 27;41(13):111883. doi: 10.1016/j.celrep.2022.111883. PMID: 36577365"
                )
              ),
              shiny::tags$br(),
              shiny::tags$div(
                shiny::tags$a(
                  href = "https://pubmed.ncbi.nlm.nih.gov/37379383/",
                  target = "_blank",
                  shiny::tags$b("Multidimensional definition of the interferonopathy of Down syndrome and its
                  response to JAK inhibition")
                ),
                shiny::tags$br(),
                htmltools::HTML(
                  "Galbraith MD, Rachubinski AL, Smith KP, Araya P, Waugh KA, Enriquez-Estrada B, Worek K,
                  Granrath RE, Kinning KT, Paul Eduthan N, Ludwig MP, Hsieh EWY, Sullivan KD, Espinosa JM"
                ),
                shiny::tags$p(
                  "Sciences Advances 2023 Jun 28;9(26):eadg6218. doi: 10.1126/sciadv.adg6218.
                  Epub 2023 Jun 28. PMID: 37379383"
                )
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
server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

  })

}
