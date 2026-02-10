#' Create overview page for metabolome application
#' @param id - string - id for this module namespace
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom shinydashboardPlus box
#' @export
metabolome_overview_ui <- function(id, ...) {
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
            shiny::tags$h2("WHAT IS THE METABOLOME?")
          ),
          shiny::tags$div(
            class = "overviewBodyText",
            shiny::tags$p(
              shiny::tags$a(
                href = "https://en.wikipedia.org/wiki/Metabolomics",
                target = "_blank",
                shiny::tags$b("Metabolomics"),
                .noWS = c("outside")
              ),
              htmltools::HTML(" is the study of all small molecules, or 'metabolites',
                produced by a cell, tissue, or organism while performing typical biological processes,
                such as converting food into energy, or getting rid of waste products from cells.
                Together, these metabolites create a chemical fingerprint (i.e., the 'metabolome')
                that provides valuable information about how that cell, tissue, or organism is functioning.
                Therefore, by measuring all metabolites, we can explore how cells are actively functioning
                in response to both internal gene expression changes and external environmental factors.
                We used metabolomics to compare metabolites between people with and without Down syndrome."
              )
            ),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("OVERVIEW")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              htmltools::HTML("This dashboard presents metabolomic data generated from
                Plasma samples, as described in "),
              shiny::tags$a(
                href = "https://pubmed.ncbi.nlm.nih.gov/31628327/",
                target = "_blank",
                "Powers et al, 2019",
                .noWS = c("outside")
              ),
              htmltools::HTML(", and "),
              shiny::tags$a(
                href = "https://pubmed.ncbi.nlm.nih.gov/37379383/",
                target = "_blank",
                "Galbraith et al, 2023.",
                .noWS = c("outside")
              ),
              shiny::tags$br(),
              shiny::tags$br(),
              shiny::tags$div(
                class = "overviewHeader",
                shiny::tags$h3("Effect of trisomy 21")
              ),
              shiny::tags$div(
                class = "overviewBodyText",
                shiny::tags$p(
                  "Explore the differences in metabolite levels between samples with
                  trisomy 21 and those without (controls)."
                ),
                shiny::tags$p("
                  The data are searchable by metabolite name and can be filtered by study,
                  age at time of blood collection, and sex"
                ),
                shiny::tags$p("
                  Significant differences between karyotypes (q<0.1, FDR10)
                  are indicated within the selected parameters."
                ),
                tags$br(),
                tags$p("Please click on the information icons for a step-by-step tutorial."),
              )
            ),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("DATASETS AND PUBLICATIONS")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              shiny::tags$div(
                shiny::tags$a(
                  href = "https://www.nature.com/articles/s41467-019-12739-9",
                  target = "_blank",
                  shiny::tags$b("Trisomy 21 activates the kynurenine pathway via increased dosage
                  of interferon receptors"),
                  .noWS = c("outside")
                ),
                shiny::tags$br(),
                htmltools::HTML(
                  "Powers RK, Culp-Hill R, Ludwig MP, Smith KP, Waugh KA, Minter R, Tuttle KD, Lewis HC,
                  Rachubinski AL, Granrath RE, Carmona-Iragui M, Wilkerson RB, Kahn DE, Joshi M, Lle&oacute; A,
                  Blesa R, Fortea J, D'Alessandro A, Costello JC, Sullivan KD, Espinosa JM."
                ),
                shiny::tags$p(
                  "Nature Communications 2019 Oct 18;10(1):4766. doi: 10.1038/s41467-019-12739-9.PMID: 31628327"
                )
              ),
              tags$br(),
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

#' Create overview page for metabolome application
#' @param id - string - id for this module namespace
#' @export
metabolome_overview_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

  })

}
