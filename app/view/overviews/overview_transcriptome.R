#' Create overview page for transcriptome application
#' @param id - string - id for this module namespace
#' @param ... dots - additional arguments (if any) to be passed to sub-modules
#' @importFrom shinydashboardPlus box
#' @export
transcriptome_overview_ui <- function(id, ...) {
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
            shiny::tags$h2("WHAT IS THE TRANSCRIPTOME?")
          ),
          shiny::tags$div(
            class = "overviewBodyText",
            shiny::tags$p(
              shiny::tags$a(
                href = "https://www.genome.gov/about-genomics/fact-sheets/Transcriptome-Fact-Sheet",
                target = "_blank",
                shiny::tags$b("Transcriptomics"),
                .noWS = c("outside")
              ),
              htmltools::HTML(" is the study of ribonucleic acid (RNA), a molecule that is produced in a cell when a gene is turned on. 
                  While the DNA that encodes all genes is the same in every cell, different types of cells (e.g. a heart cell versus a liver cell) may turn certain genes 
                  on and off at different times. Other factors may also affect what genes are turned on and off, such as age, gender, and diseases or other health conditions. 
                  Therefore, by measuring the levels of all RNAs (i.e. the \"transcriptome\"), we can understand what genes are expressed in a variety of situations. 
                  We used transcriptomics to compare gene expression between people with and without Down syndrome and in various types of cells."
              )
            ),
            shiny::tags$br(),
            shiny::tags$div(
              class = "overviewHeader",
              shiny::tags$h3("OVERVIEW")
            ),
            shiny::tags$div(
              class = "overviewBodyText",
              shiny::tags$p("This dashboard presents transcriptome data generated from five sample types: fibroblasts, monocytes, T-cells, white blood cells, and whole blood. 
                  The data are searchable by gene name and can be filtered by age at time of blood collection and sex. Significant differences between karyotypes (p<0.05) 
                  are indicated within the selected parameters."
              ),          
              shiny::tags$p(
                shiny::tags$b("METHODS"),
                shiny::tags$p("RNA-seq was used to identify the quantity of RNA in each of the five sample types. All comparisons are reported using a Student's T-test and have 
                not been corrected for multiple comparisons. For full methods and analysis, please see associated publications."
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
                  href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5012864/",
                  target = "_blank",
                  shiny::tags$b("Trisomy 21 consistently activates the interferon response."),
                  .noWS = c("outside")
                ),
                htmltools::HTML("Sullivan KD, Lewis HC, Hill AA, Pandey A, Jackson LP, Cabral JM, Smith KP, Liggett LA, Gomez EB, Galbraith MD, DeGregori J, Espinosa JM. Elife. 2016 Jul 29;5. PMID: 27472900.")
              ),
              shiny::tags$br()
            )
          )
        )
      )
    )
  )

}

#' overview page for transcriptome application
#' @param id - string - id for this module namespace
#' @export
transcriptome_overview_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

  })

}