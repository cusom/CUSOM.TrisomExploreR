#' @export
PreCalcFeatureAnalysisManager <- R6::R6Class(
  "PreCalcFeatureAnalysisManager",
  inherit = FeatureAnalysisManager,
  private = list(),
  public = list(
    Age = c(0,99),
    Sex = c("Male","Female"),
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){
      super$initialize(applicationName, id, namespace_config, remoteDB, localDB)
    },

    getKaryotypeChoices = function(karyotypes, localDBLocation) {

      if(self$analysisVariable == "Karyotype") {
        return(
          tibble::tibble(
            choiceNames = 'Trisomy 21 vs. Control',
            choiceValues = glue::glue_collapse(karyotypes, sep = ";")
          )
        )
      }

      if(self$analysisVariable == "Age") {

        karyotypeInputs <- self$localDB$getQuery(
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
              Karyotype = glue::glue_collapse(karyotypes, sep=","),
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
                      data-original-title="Test for differences in age trajectories between Trisomy 21 & Controls">
                    </span>
                  </div>'
                ),
              choiceValues = glue::glue_collapse(karyotypes,sep=";")
            )
          ) |>
          dplyr::arrange(sort)

        return(karyotypeInputs)

      }

    },

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
            karyotypes = glue::glue_collapse(self$Karyotype,';'),
            covariates = ifelse(is.null(self$Covariates),'none',glue::glue_collapse(self$Covariates,';'))
          )
        ) |>
        dplyr::rename(
          "p.value.original" = pvalue,
          "p.value" = padj
        ) |>
        dplyr::mutate(
          log2FoldChange= log2(FoldChange),
          `-log10pvalue` = -log10(p.value),
          `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
          formattedPValue = unlist(purrr::pmap(.l = list(p.value,p.value.adjustment.method), CUSOMShinyHelpers::formatPValue)),
          text = glue::glue('Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}'),
          "lmFormula" = "<a href='https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html' target='_blank'>DESeq2 model</a>",
          ivs = ""
        )

        self$VolcanoPlotTitle <- glue::glue("Effect of {self$analysisVariableLabel} on all {self$analytesLabel}")
        self$VolcanoSummaryMaxFoldChange <- max(abs(self$VolcanoSummaryData$log2FoldChange))
        self$VolcanoSummaryDataXAxisLabel <- "log<sub>2</sub>(Fold Change)"
        self$VolcanoSummaryDataYAxisLabel <- glue::glue("-log<sub>10</sub>({ifelse(self$Adjusted,\"q-value \",\"p-value \")})")

    },

    getAnalyteData = function() {

      self$AnalytePlotMethod <- getAnalytePlotMethod(self$analysisType, length(self$Analyte))
      self$AnalytePlotTitle <- getAnalytePlotTitle(self$analysisVariable, self$AnalytePlotMethod, self$Analyte, self$Karyotype)
      self$groupBaselineLabel <- stringr::str_split(gsub("<.*?>", "", self$groupBaselineLabel),' ', simplify = TRUE)[1]

      if(length(self$Analyte) == 1) {

        self$AnalyteData <- self$remoteDB$getQuery(
          "[shiny].[GetDataByStudyAnalyteAgeSexKaryotype] ?,?,?,?,?,?",
          tibble::tibble(
            "StudyName" = self$Study,
            "Analyte" = self$Analyte,
            "MinAge" = min(self$Age),
            "MaxAge" = max(self$Age),
            "Sex" = glue::glue_collapse(self$Sex,";"),
            "Karyotype" = unlist(stringr::str_split(self$Karyotype, pattern = ';'))
          )
        ) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})"),
          highlightGroup = dplyr::case_when(
            1 == 1 ~ NA
            # LabID %in% r6$GroupA ~ "A",
            # LabID %in% r6$GroupB ~ "B"
          )
        ) |>
        dplyr::filter(
          log2MeasuredValue != Inf,
          log2MeasuredValue != -Inf
        )

        if(self$AnalytePlotMethod == "boxplot") {

          self$AnalyteData <- self$AnalyteData |>
            dplyr::rowwise() |>
            CUSOMShinyHelpers::addGroupCount(
              group = !!rlang::sym(self$analysisVariable),
              addLineBreak = FALSE
            ) |>
            dplyr::select(-n) |>
            dplyr::ungroup() |>
            dplyr::mutate( text = glue::glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}") )

          self$groupBaselineLabel <- self$AnalyteData |>
            dplyr::select(!!rlang::sym(self$analysisVariable)) |>
            dplyr::distinct() |>
            dplyr::filter(grepl(self$groupBaselineLabel,!!rlang::sym(self$analysisVariable))) |>
            dplyr::pull()

        }

      }

      else {

        self$AnalytePlotMethod <- "heatmap"

        self$AnalyteData <- self$VolcanoSummaryData |>
          dplyr::filter(Analyte %in% self$Analyte) |>
          dplyr::select(Analyte, log2FoldChange, text) |>
          dplyr::arrange(-log2FoldChange) |>
          dplyr::mutate(Analyte = forcats::fct_inorder(Analyte), "Analysis" = "T21vD21")

      }
    },

    getGSEAData = function() {

      ranks <- self$VolcanoSummaryData |>
        dplyr::rowwise() |>
        dplyr::mutate(ParsedComparisonAnalyte = CUSOMShinyHelpers::parseDelimitedString(Analyte,1)) |>
        dplyr::ungroup() |>
        ### NO NEED TO PENALIZE SINCE THE MODEL IS DESEQ
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

      gsea <- CUSOMShinyHelpers::runfGSEA(geneset = GSEA_hallmarks, ranks = ranks) |>
        dplyr::mutate(
          Leading.edge.genes = purrr::map_chr(leadingEdge, toString),
          Leading.edge.genes = gsub(' ','',Leading.edge.genes)
        ) |>
        dplyr::select("Gene.set" = pathway, "Size" = size, ES, NES, "p.value" = pval, "q.value" = padj, Leading.edge.genes) |>
        dplyr::mutate(Gene.set = stringr::str_to_title(trimws(gsub('_',' ',gsub('HALLMARK','',Gene.set)))))

      self$GSEAData <- list(
        "ranks" = ranks,
        "hallmarks" = GSEA_hallmarks,
        "gsea" = gsea
      )
    },

    getGSEAPlot = function(.data, ns) {

      data <- .data$gsea |>
        dplyr::mutate(
          `-log10qvalue` = -log(q.value),
          text = glue::glue("Gene Set: {Gene.set}<br />NES: {NES} <br />-log<sub>10</sub>(q-value): {`-log10qvalue`}")
        ) |>
        dplyr::top_n(25, wt = abs(NES))

      limit <- data |>
        dplyr::pull(NES) |>
        abs() |>
        max() |>
        plyr::round_any(1, f = ceiling) * 1.15

      p <- data |>
        dplyr::arrange(`-log10qvalue`) |>
        plotly::plot_ly(
          type = "bar",
          x = ~ `-log10qvalue`,
          y = ~ reorder(Gene.set,NES),
          hoverinfo = "text",
          hovertext = ~ text,
          customdata = ~ Leading.edge.genes,
          marker = list(
            color = ~ NES,
            autocolorscale = FALSE,
            colorscale = "RdBlu",
            cauto  = FALSE,
            cmax = limit,
            cmid = 0,
            cmin = -limit,
            colorbar = list(
              title = 'NES',
              tickmode = "auto",
              len = 0.5,
              yanchor = "middle",
              y = 0.5
            )
          )
        ) |>
        plotly::layout(
          title = list(
            text = "GSEA: Top 25 Hallmark gene sets <br />T21 vs. Control"
          ),
          showlegend = FALSE,
          xaxis = list(
            title = list(
              text = "-log<sub>10</sub>(q-value)"
            ),
            showlines = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            title = list(
              text = ""
            ),
            showlines = FALSE
          ),
          margin = list(
            t = 65
          ),
          shapes = list(
            list(
              type = "line",
              xref = "x",
              yref = "paper",
              axref = "paper",
              ayref = "y",
              y0 = 0,
              y1 = 1,
              x0 = -log(0.1),
              x1 = -log(0.1),
              line = list(
                color = "black",
                dash = "dot"
              )
            )
          )
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - GSEA Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      p$x$source <- ns("GSEAPlot")

      return(p)

    },

    getGSEAPathwayData = function(pathName) {
      
      gseaParam <- 0

      stats <- self$GSEAData$ranks

      pathwayNammed <- self$GSEAData$gsea |>
        dplyr::filter(Gene.set == pathName) |>
        dplyr::select(Leading.edge.genes) |>
        dplyr::mutate(id = dplyr::row_number()) |>
        tidyr::separate_rows(Leading.edge.genes, sep = ",") |>
        dplyr::select(-id) |>
        purrr::simplify()

      rnk <- rank(-stats)
      ord <- order(rnk)
      statsAdj <- stats[ord]
      statsAdj <- sign(statsAdj) * (abs(statsAdj)^gseaParam)
      statsAdj <- statsAdj/max(abs(statsAdj))

      pathway <- unname(as.vector(na.omit(match(pathwayNammed, names(statsAdj)))))
      pathway <- sort(pathway)

      gseaRes <- fgsea::calcGseaStat(
        statsAdj,
        selectedStats = pathway,
        returnAllExtremes = TRUE
      )

      bottoms <- gseaRes$bottoms
      tops <- gseaRes$tops
      n <- length(statsAdj)
      xs <- as.vector(rbind(pathway - 1, pathway))
      ys <- as.vector(rbind(bottoms, tops))

      GSEAScores <- tibble::tibble(x = c(0, xs, n + 1), y = c(0, ys, 0)) |>
        dplyr::inner_join(
          tibble::tibble(x = pathway, Gene = pathwayNammed),
          by = "x"
        ) |>
        dplyr::rename("Rank" = x, "ES" = y) |>
        dplyr::relocate("Gene")

      self$GSEAPathwayData <- self$VolcanoSummaryData |>
        dplyr::filter(AnalyteID %in% self$Analyte) |>
        dplyr::select("Gene" = Analyte,log2FoldChange, `-log10pvalue`) |>
        dplyr::left_join( GSEAScores, by = "Gene" ) |>
        dplyr::group_by(Gene) |>
        dplyr::arrange(-log2FoldChange) |>
        dplyr::mutate(r = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::filter(r == 1) |>
        dplyr::select(-c(ES,r)) |>
        dplyr::mutate(
          log2FoldChange = format(log2FoldChange,scientific = TRUE),
          `-log10pvalue` = format(`-log10pvalue`,scientific = TRUE),
          Analyte = Gene,
          Gene = glue::glue("<a href=\"https://www.genecards.org/Search/Keyword?queryString={Gene}\" target=\"_blank\">{Gene}</a>")
        ) |>
        dplyr::relocate(Gene) |>
        dplyr::rename("log<sub>2</sub>(Fold Change)" = log2FoldChange,"-log<sub>10</sub>(q-value)" = `-log10pvalue`) |>
        dplyr::arrange(Rank)

      self$GSEAAnalytes <- self$GSEAPathwayData |>
        dplyr::select(Analyte) |>
        dplyr::summarise(text = toString(Analyte)) |>
        dplyr::mutate(text = gsub(', ','|',text)) |>
        dplyr::pull()

      self$GSEAGenesetName <- glue::glue("HALLMARK_{gsub(' ','_',stringr::str_to_upper(self$GSEATraceName))}")

    }, 
    getGSEAEnrichmentPlot = function(.data, ns) {

      p <- CUSOMShinyHelpers::plotGSEAEnrichment(
        pathName = self$GSEATraceName,
        stats = .data$ranks,
        res = .data$gsea,
        title = glue::glue("T21 vs. Control:\n{self$GSEATraceName}")
      ) |>
      plotly::layout(
        margin = list(
          autoexpand = TRUE,
          l = 10,
          r = 30,
          t = 75
        )
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "svg",
          filename = glue::glue('{self$applicationName} - {self$Study} GSEA Enrichment Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
          width = NULL,
          height = NULL
        ),
        modeBarButtons = list(
          list("zoom2d"),
          list("zoomIn2d"),
          list("zoomOut2d"),
          list("resetScale2d"),
          list("toImage")
        )
      )

      p$x$source <- ns("GSEAEnrichmentPlot")

      p

    }

  )
)
