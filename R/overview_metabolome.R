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
              shiny::tags$html(" is the study of all small molecules, or 'metabolites', produced by a cell, tissue, or organism while performing typical biological processes,
                    such as converting food into energy, or getting rid of waste products from cells. Together, these metabolites create a chemical
                    fingerprint (i.e. the 'metabolome') that provides valuable information about how that cell, tissue, or organism is functioning.
                    Therefore, by measuring all metabolites, we can explore how cells are actively functioning in response to both internal gene expression changes
                    and external environmental factors. We used metabolomics to compare metabolites between people with and without Down syndrome."
              )
            ),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("OVERVIEW")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              shiny::tags$p("This dashboard presents metabolomic data generated from plasma and can be displayed as either absolute or relative quantity.
                          The data are searchable by metabolite and can be filtered by age at time of blood collection and sex.
                          Significant differences between karyotypes (p<0.05) are indicated within the selected parameters."
              ),
              shiny::tags$p(
                shiny::tags$b("Effect of trisomy 21"),
                shiny::tags$p("Explore the differences in protein levels between samples with trisomy 21 and those without (controls).
                            The data are searchable by protein name and can be filtered by platform, age at time of blood collection, and sex.
                            Significant differences between karyotypes (p<0.05) are indicated within the selected parameters."
                )
              ),
              shiny::tags$p(
                shiny::tags$b("METHODS"),
                shiny::tags$p("Metabolomics data were generated via ultra-high-pressure liquid chromatography coupled to high-resolution mass spectrometry (UHPLC-HRMS) technology.
                          All comparisons are reported using a Student&rsquo;s T-test and have not been corrected for multiple comparisons.
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
                  href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5745140/",
                  target = "_blank",
                  shiny::tags$b("Red blood cell metabolism in Down syndrome: hints on metabolic derangements in aging."),
                  .noWS = c("outside")
                ),
                tags$html("Culp-Hill R, Zheng C, Reisz JA, Smith K, Rachubinski A, Nemkov T, Butcher E, Granrath R, Hansen KC, Espinosa JM, D'Alessandro A. Blood Advances. 2017 Dec 21;1(27):2776-2780. PMID: 29296929.")
              ),
              shiny::tags$br(),
              shiny::tags$p(
                shiny::tags$a(
                  href = "https://www.nature.com/articles/s41467-019-12739-9",
                  target = "_blank",
                  shiny::tags$b("Trisomy 21 activates the kynurenine pathway via increased dosage of interferon receptors."),
                  .noWS = c("outside")
                ),
                shiny::tags$html("Powers RK, Culp-Hill R, Ludwig MP, Smith KP, Waugh KA, Minter R, Tuttle KD, Lewis HC, Rachubinski AL, Granrath RE, Carmona-Iragui M, Wilkerson RB, Kahn DE, Joshi M, Lle&oacute; A, Blesa R,
                              Fortea J, D'Alessandro A, Costello JC, Sullivan KD, Espinosa JM. Nature Communications. 2019 Oct 18;10(1):4766. PMID: 31628327.")
              )
            )
          )
        )
      )
    )
  )

}

#' @export
metabolome_overview_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

  })

}
