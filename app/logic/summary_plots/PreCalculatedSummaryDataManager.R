
box::use(
  R6[R6Class],
  glue[glue, glue_collapse],
  dplyr[filter, select, rename, mutate],
  purrr[pmap]
)

box::use(
  app/logic/summary_plots/CategoricalSummaryDataManager[FeatureAnalysis_CategoricalSummaryDataManager],
  app/logic/shared/statistical_analysis[formatPValue]
)

#' @export
PreCalculatedFeatureAnalysis_SummaryDataManager <- R6::R6Class(
  "PreCalculatedFeatureAnalysis_SummaryDataManager",
  inherit = FeatureAnalysis_CategoricalSummaryDataManager,
  private = list(),
  public = list(
    initialize = function(analysis_config, StatTest, Covariates, AdjustmentMethod) {
      super$initialize(analysis_config, StatTest, Covariates, AdjustmentMethod)
    },
    setVolcanoSummaryData = function(.data) {
      self$VolcanoSummaryData <- .data |>
        select("AnalyteID" = Geneid, "Analyte" = Gene_name, FoldChange, pvalue, padj) |>
        rename(
          "p.value.original" = pvalue,
          "p.value" = padj
        ) |>
        mutate(
          log2FoldChange = log2(FoldChange),
          `-log10pvalue` = -log10(p.value),
          `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
          formattedPValue = unlist(
            pmap(
              .l = list(p.value, p.value.adjustment.method),
              formatPValue
            )
          ),
          text = glue("
            Gene: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}"
          ),
          lmFormula =
            "<a href='https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html'
                      target='_blank'>DESeq2 model</a>",
          ivs = ""
        )
      return(self$VolcanoSummaryData)
    }
  )
)
