#' @export
clinical_overview_ui <- function(id, ...) {
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
          shiny::tags$img(
            src = "www/LindaCrnicInstituteLogo.png",
            width = "auto",
            height = "115px",
            style = "margin: auto;display:block"
          ),
          tags$br(),
          tags$div(
            class = "overviewHeader",
            tags$h2("WHAT IS AN IMMUNE MAP?")
          ),
          tags$div(
            class = "overviewBodyText",
            tags$html("An immune map is a detailed characterization of the types and amounts of immune cells present in an organism.
                  Altogether, these immune cells drive the immune response against infectious agents such as bacteria and viruses.
                  Some immune cells also participate in the defense against tumors, and some can contribute to the appearance and severity
                  of autoimmune disorders. Therefore, an immune map provides valuable information about how the immune system is functioning.
                  By measuring a multitude of immune cell types in the blood, we can explore how the immune system is different in people
                  with Down syndrome versus typical people, and also reveal differences among those with Down syndrome affected by
                  different co-occurring health conditions or \'comorbidities\'."
            ),
            tags$br(),
            tags$div(
              class = "overviewHeader",
              tags$h3("OVERVIEW")
            ),
            tags$div(
              class = "overviewBodyText",
              tags$p("This dashboard contains data generated from circulating blood using two different platforms:
                  mass-cytometry and flow-cytometry."
              ),
              tags$p(
                tags$b("Effect of trisomy 21"),
                tags$p("Explore the differences in immune cell types between
                  samples with trisomy 21 (T21, Down syndrome) versus those from typical people (controls). The data are searchable
                  by cell type and can be filtered by experimental platform, age at time of blood collection, and sex. Significant
                  differences between trisomy 21 and controls (p<0.05) are indicated within the selected parameters."
                )
              ),
              tags$p(
                tags$b("Effect of Comorbidity"),
                tags$html("Explore the differences in immune cell types between samples obtained from people with trisomy 21
                  (T21, Down syndrome) <b><i>with</i></b> versus <b><i>without</i></b> selected comorbidities as defined
                  by mass-cytometry. The data are searchable by cell type and can be filtered by age at time of blood collection,
                  and sex. Significant differences between those <b><i>with</i></b> versus <b><i>without</i></b> selected
                  comorbidities  (p<0.05) are indicated within the selected parameters."
                )
              ),
              tags$p(
                tags$b("METHODS"),
                tags$p("Mass-cytometry and flow-cytometry were used to generate cell counts.
                  All comparisons are reported using a Student\'s T-test and have not been corrected for multiple comparisons.
                  For full methods and analysis, please see associated publications.
                  Data are represented from two measurements: frequency of a given cell type among total non-granulocytes of
                  hematopoietic origin (i.e. frequency among total CD45+CD66-) or frequency of a given cell type among its immune
                  cell lineage (e.g. frequency among total T cells)"
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
                  href = "https://doi.org/10.1016/j.celrep.2019.10.038",
                  target = "_blank",
                  tags$b("Mass-cytometry reveals global immune remodeling with multi-lineage hypersensitivity to Type I Interferon in Down syndrome."),
                  .noWS = c("outside")
                ),
                tags$html("Waugh, KA, Araya P, Pandey A, Jordan KR, Smith KP, Granrath RE, Khanal S, Butcher ET, Enriquez Estrada B,
                  Rachubinski AL, McWilliams JA, Minter R, Dimasi T, Colvin KL,
                  Baturin D, Pham AT, Galbraith MD, Bartsch KW, Yeager ME, Porter CC, Sullivan KD, Hsieh EW, Espinosa JM.
                  Cell Reports. 2019 Nov 12; 29(7):1893-1908.")
              ),
              tags$br(),
              tags$p(
                tags$a(
                  href = "https://www.pnas.org/content/early/2019/11/06/1908129116",
                  target = "_blank",
                  tags$b("Trisomy 21 dysregulates T cell lineages toward an autoimmunity-prone state associated with interferon hyperactivity."),
                  .noWS = c("outside")
                ),
                tags$html("Araya P, Waugh KA, Sullivan KD, Nunez NG, Roselli E, Smith KP, Granrath RE, Rachubinski AL, Butcher ET,
                  Minter R, Tuttle KD,
                  Bruno TC, Maccioni M, and Espinosa JM. PNAS. 2019 Nov 7; PMID: 31699819.")
              )
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
