#' R6 Class to  manage precalculated Feature Analysis - DESeq2
#' @description
#' subclass of FeatureAnalysisManager - overrides several class methods
#' @field Age - numeric vector of chosen ages
#' @field Sex - character vecotr of chosen sex(s)
#' @export
PreCalcFeatureAnalysisManager <- R6::R6Class(
  "PreCalcFeatureAnalysisManager",
  inherit = FeatureAnalysisManager,
  private = list(),
  public = list(
    Age = c(0, 99),
    Sex = c("Male", "Female"),

    #' @description
    #' Create a new instance of a PreCalcFeatureAnalysisManager
    #' @param applicationName string - name of application
    #' @param id string - namespace for this instance
    #' @param namespace_config list - configurations for this namespace
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){
      super$initialize(applicationName, id, namespace_config, remoteDB, localDB)
    },

    #' @description
    #' helper function to get Karyotype input options based on namespace
    #' @param karyotypes string vector of karyotypes for input widget
    getKaryotypeChoices = function(karyotypes) {

      if (self$analysisVariable == "Karyotype") {
        return(
          tibble::tibble(
            choiceNames = "Trisomy 21 vs. Control",
            choiceValues = glue::glue_collapse(karyotypes, sep = ";")
          )
        )
      }

      if (self$analysisVariable == "Age") {
        return(
          self$localDB$getQuery(
          "SELECT * FROM analyteKaryotypeCounts"
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            sort = dplyr::case_when(
              Karyotype == "Trisomy 21" ~ 1,
              TRUE ~ 99
            ),
            choiceNames = glue::glue("{Karyotype} (n={n})"),
            choiceValues = Karyotype
          ) |>
          dplyr::bind_rows(
            tibble::tibble(
              Karyotype = glue::glue_collapse(karyotypes, sep = ","),
              n = NA,
              sort = 999,
              choiceNames =
                glue::glue(
                  '<div>{glue::glue_collapse(karyotypes,sep = " vs. ")}
                    <span
                      data-toggle="tooltip"
                      data-placement="auto right"
                      title=""
                      class="fas fa-info-circle gtooltip info-tooltip"
                      data-original-title="Test for differences in trajectories between Trisomy 21 & Controls">
                    </span>
                  </div>'
                ),
              choiceValues = glue::glue_collapse(karyotypes, sep = ";")
            )
          ) |>
          dplyr::arrange(sort)
        )
      }

    },

    #' @description
    #' Set Volcano Summary Data along with other volcano plot properties
    getVolcanoSummaryData = function() {

      self$VolcanoSummaryData <- self$localDB$getQuery(
        "SELECT g.AnalyteID, g.Analyte, s.FoldChange, s.pvalue, s.padj
          FROM PrecalculatedDESeq2 s
          INNER JOIN Genes g ON AnalyteID = s.Geneid
          WHERE [namespace] = ({namespace})
          AND samples = ({karyotypes})
          AND selected_parameters = ({covariates})",
          tibble::tibble(
            namespace = self$namespace,
            karyotypes = glue::glue_collapse(self$Karyotype, ";"),
            covariates = ifelse(is.null(self$Covariates), "none", glue::glue_collapse(self$Covariates, ";"))
          )
        ) |>
        dplyr::rename(
          "p.value.original" = pvalue,
          "p.value" = padj
        ) |>
        dplyr::mutate(
          log2FoldChange = log2(FoldChange),
          `-log10pvalue` = -log10(p.value),
          `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
          formattedPValue = unlist(
            purrr::pmap(
              .l = list(p.value, p.value.adjustment.method),
              CUSOMShinyHelpers::formatPValue
            )
          ),
          text = glue::glue("Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}"),
          "lmFormula" = "<a href='https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html' target='_blank'>DESeq2 model</a>",
          ivs = ""
        )

        self$VolcanoPlotTitle <- glue::glue("Effect of {self$analysisVariableLabel} on all {self$analytesLabel}")
        self$VolcanoSummaryMaxFoldChange <- max(abs(self$VolcanoSummaryData$log2FoldChange))
        self$VolcanoSummaryDataXAxisLabel <- "log<sub>2</sub>(Fold Change)"
        self$VolcanoSummaryDataYAxisLabel <- glue::glue(
          "-log<sub>10</sub>({ifelse(self$Adjusted,\"q-value \",\"p-value \")})"
        )

    },

    #' @description
    #' get sample level data for selected analyte(s)
    getAnalyteData = function() {

      self$AnalytePlotMethod <- getAnalytePlotMethod(self$analysisType, length(self$Analyte))
      self$AnalytePlotTitle <- getAnalytePlotTitle(
        self$analysisVariable,
        self$AnalytePlotMethod,
        self$Analyte, self$Karyotype
      )
      self$groupBaselineLabel <- stringr::str_split(
        gsub("<.*?>", "", self$groupBaselineLabel),
        " ",
        simplify = TRUE
      )[1]

      if (length(self$Analyte) == 1) {

        self$AnalyteData <- self$remoteDB$getQuery(
          "[shiny].[GetDataByStudyAnalyteAgeSexKaryotype] ?,?,?,?,?,?",
          tibble::tibble(
            "StudyName" = self$Study,
            "Analyte" = self$Analyte,
            "MinAge" = min(self$Age),
            "MaxAge" = max(self$Age),
            "Sex" = glue::glue_collapse(self$Sex, ";"),
            "Karyotype" = unlist(stringr::str_split(self$Karyotype, pattern = ";"))
          )
        ) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})"),
          highlightGroup = dplyr::case_when(
            1 == 1 ~ NA
          )
        ) |>
        dplyr::filter(
          log2MeasuredValue != Inf,
          log2MeasuredValue != -Inf
        )

        if (self$AnalytePlotMethod == "boxplot") {

          self$AnalyteData <- self$AnalyteData |>
            dplyr::rowwise() |>
            CUSOMShinyHelpers::addGroupCount(
              group = !!rlang::sym(self$analysisVariable),
              addLineBreak = FALSE
            ) |>
            dplyr::select(-n) |>
            dplyr::ungroup() |>
            dplyr::mutate(text = glue::glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}"))

          self$groupBaselineLabel <- self$AnalyteData |>
            dplyr::select(!!rlang::sym(self$analysisVariable)) |>
            dplyr::distinct() |>
            dplyr::filter(grepl(self$groupBaselineLabel, !!rlang::sym(self$analysisVariable))) |>
            dplyr::pull()

        }

      } else {

        self$AnalytePlotMethod <- "heatmap"

        self$AnalyteData <- self$VolcanoSummaryData |>
          dplyr::filter(Analyte %in% self$Analyte) |>
          dplyr::select(Analyte, log2FoldChange, text) |>
          dplyr::arrange(-log2FoldChange) |>
          dplyr::mutate(Analyte = forcats::fct_inorder(Analyte), "Analysis" = "T21vD21")

      }
    },

    #' @description
    #' Get GSEA data
    #' ### NO NEED TO PENALIZE SINCE THE MODEL IS DESEQ**
    #'
    getGSEAData = function() {

      ranks <- self$VolcanoSummaryData |>
        dplyr::rowwise() |>
        dplyr::mutate(ParsedComparisonAnalyte = CUSOMShinyHelpers::parseDelimitedString(Analyte, 1)) |>
        dplyr::ungroup() |>
        # dplyr::mutate(
        #   ID = ParsedComparisonAnalyte,
        #   t = (`-log10pvalue` * CorrelationValue)
        # ) |>
        # dplyr::select(ID,t) |>
        dplyr::select(ID = ParsedComparisonAnalyte, t = log2FoldChange) |>
        dplyr::filter(!is.na(t)) |>
        dplyr::arrange(-abs(t)) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        tibble::deframe()

      gsea <- CUSOMShinyHelpers::runfGSEA(geneset = TrisomExploreR:::GSEA_hallmarks, ranks = ranks) |>
        dplyr::mutate(
          Leading.edge.genes = purrr::map_chr(leadingEdge, toString),
          Leading.edge.genes = gsub(" ", "", Leading.edge.genes)
        ) |>
        dplyr::select(
          "Gene.set" = pathway,
          "Size" = size,
          ES,
          NES, "p.value" = pval,
          "q.value" = padj,
          Leading.edge.genes
        ) |>
        dplyr::mutate(
          Gene.set = stringr::str_to_title(trimws(gsub("_", " ", gsub("HALLMARK", "", Gene.set))))
        )

      self$GSEAData <- list(
        "ranks" = ranks,
        "hallmarks" = GSEA_hallmarks,
        "gsea" = gsea
      )
    }

  )
)
