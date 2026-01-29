box::use(
  app/logic/shared/statistical_analysis[getStatTestByKeyGroup, formatPValue, runfGSEA, calculate_GSEA_scores],
  app/logic/summary_plots/volcano_plot_helpers[getVolcanoPlot, getVolcanoAnnotations, addSignificanceGroup],
  app/logic/shared/string_helper_functions[parse_delimited_string],
)

#' @export
GSEAManager <- R6::R6Class(
  "GSEAManager",
  private = list(
    ..event_data = tibble::tibble()
  ),
  active = list(
    event_data = function(value) {
      if (missing(value)) {
        return(private$..event_data)
      } else {
        private$..event_data <- value
      }
    },
    event_data_keys = function(value) {
      return(
        tibble::tibble("Gene" = self$event_data$customdata) |>
          tidyr::separate_rows(Gene, sep = ",") |>
          dplyr::inner_join(
            self$VolcanoSummaryData() |>
              dplyr::select(Analyte) |>
              dplyr::mutate(
                "Gene" = purrr::pmap_chr(
                  list(Analyte, 1),
                  parse_delimited_string
                )
              ),
            by = "Gene"
          ) |>
          dplyr::select(Analyte) |>
          dplyr::summarise(text = toString(Analyte)) |>
          dplyr::mutate(text = gsub(", ", "|", text)) |>
          dplyr::pull()
      )
    },
    Analyte = function(value) {
      return(
        stringr::str_split(self$event_data_keys, "\\|", simplify = TRUE)
      )
    },
    GSEATraceName = function(value) {
      return(
        self$event_data$y
      )
    },
    GSEAAnalytes = function(value) {
      if (missing(value)) {
        return (
            self$GSEAPathwayData |>
              dplyr::select(Analyte) |>
              dplyr::summarise(text = toString(Analyte)) |>
              dplyr::mutate(text = gsub(", ", "|", text)) |>
              dplyr::pull()
        )
      } else {
        stop("GSEAAnalytes is a read-only active binding")
      }
    },
    GSEAGenesetName = function(value) {
      if (missing(value)) {
        return(glue::glue("HALLMARK_{gsub(' ','_',stringr::str_to_upper(self$GSEATraceName))}"))
      } else {
        stop("GSEAGenesetName is a read-only active binding")
      }
    }
  ),
  
  public = list(

    Study = NULL,
    VolcanoSummaryData = NULL,
    GSEA_hallmarks = NULL,


    GSEAData = NULL,
    GSEAPathwayData = NULL,

    #' @description
    #' Create a new instance of a FeatureAnalysisManager
    #' @param applicationName string - name of application
    #' @param id string - namespace for this instance
    #' @param namespace_config list - configurations for this namespace
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(Study, VolcanoSummaryData) {

      self$Study <- Study
      self$VolcanoSummaryData <- VolcanoSummaryData
      self$GSEA_hallmarks <- readRDS("app/logic/app_resources/data/GSEA_hallmarks.rds")

    },

    #' @description
    #' helper function to hide / show, disable / enable GSEA Pathway analysis based on conditions
    #' If study is one of SOMA / RNASeq, show
    #' If volcano summary plot has been rendered, enable button
    addGSEAInputClass = function() {
      hide <- "hide"
      disabled <- "disabled"
      if (!is.null(self$Study())) {
        if (grepl("SOMA", self$Study(), ignore.case = TRUE) | grepl("RNA", self$Study(), ignore.case = TRUE)) {
          hide <- "show"
        }
        if (!is.null(self$VolcanoSummaryData())) {
          disabled <- "enabled"
        }
      }
      return(
        glue::glue("shinyjs-{hide} shinyjs-{disabled}")
      )
    },

    #' @description
    #' Get GSEA data
    #' ** INCLUDES penalized calculation for ranks = `-log10pvalue` * CorrelationValue
    #'
    getGSEAData = function() {

      ranks <- self$VolcanoSummaryData() |>
        dplyr::select(Analyte, `-log10pvalue`, log2FoldChange) |>
        dplyr::mutate(
            ParsedComparisonAnalyte = purrr::pmap_chr(
                list(Analyte, 1),
                parse_delimited_string
            ),
            ID = ParsedComparisonAnalyte,
            t = (`-log10pvalue` * log2FoldChange)
        ) |>
        dplyr::select(ID, t) |>
        dplyr::filter(!is.na(t)) |>
        dplyr::arrange(-abs(t)) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        tibble::deframe()

      gsea <- runfGSEA(geneset = self$GSEA_hallmarks, ranks = ranks) |>
        dplyr::mutate(
            Leading.edge.genes = purrr::map_chr(leadingEdge, toString),
            Leading.edge.genes = gsub(" ", "", Leading.edge.genes)
        ) |>
        dplyr::select(
            "Gene.set" = pathway,
            "Size" = size,
            ES,
            NES,
            "p.value" = pval,
            "q.value" = padj,
            Leading.edge.genes
        ) |>
        dplyr::mutate(
            Gene.set = stringr::str_to_title(trimws(gsub("_", " ", gsub("HALLMARK", "", Gene.set))))
        )

      self$GSEAData <- list(
        "ranks" = ranks,
        "hallmarks" = self$GSEA_hallmarks,
        "gsea" = gsea
      )
    },

    #' @description
    #' Get GSEA plot - top 25
    #' @param .data - data for plot
    #' @param ns - namespace to apply to plot object
    getGSEAPlot = function(.data, ns, top_n = 25) {

      data <- .data$gsea |>
        dplyr::mutate(
          `-log10qvalue` = -log(q.value),
          text = glue::glue(
            "Gene Set: {Gene.set}
            NES: {NES} 
            -log<sub>10</sub>(q-value): {`-log10qvalue`}"
          )
        ) |>
        dplyr::top_n(top_n, wt = abs(NES))

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
          y = ~ stats::reorder(Gene.set, NES),
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
              title = "NES",
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
            filename = glue::glue("{self$applicationName} - GSEA Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
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

    getSelectedGSEAPathwayData = function(event_data) {
    
      keys <- tibble::tibble("Gene" = event_data$customdata) |>
        tidyr::separate_rows(Gene, sep = ",") |>
        dplyr::inner_join(
          self$VolcanoSummaryData() |>
            dplyr::select(Analyte) |>
            dplyr::mutate(
              "Gene" = purrr::pmap_chr(
                list(Analyte, 1),
                parse_delimited_string
              )
            ),
          by = "Gene"
        ) |>
        dplyr::select(Analyte) |>
        dplyr::summarise(text = toString(Analyte)) |>
        dplyr::mutate(text = gsub(", ", "|", text)) |>
        dplyr::pull()

      self$Analyte <- stringr::str_split(keys, "\\|", simplify = TRUE)

      self$GSEATraceName <- event_data$y

      return(invisible(self))

    },

    #' @description
    #' Set selected GSEA pathway data
    #' @param path_name - string - selected pathway name
    getGSEAPathwayData = function(path_name) {

      pathway_data <- self$GSEAData$gsea |>
        dplyr::filter(Gene.set == path_name)

      gsea_scores <- calculate_GSEA_scores(
        self$GSEAData$ranks,
        pathway_data
      )
      
      self$GSEAPathwayData <- self$VolcanoSummaryData() |>
        dplyr::filter(Analyte %in% self$Analyte) |>
        dplyr::select(Analyte, log2FoldChange, `-log10pvalue`) |>
        dplyr::mutate(
            Gene = purrr::pmap_chr(
                list(Analyte, 1),
                parse_delimited_string
            )
        ) |>
        dplyr::left_join(gsea_scores, by = "Gene") |>
        dplyr::group_by(Gene) |>
        dplyr::arrange(-log2FoldChange) |>
        dplyr::mutate(r = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::filter(r == 1) |>
        dplyr::select(-c(ES, r)) |>
        dplyr::mutate(
            log2FoldChange = format(log2FoldChange, scientific = TRUE),
            `-log10pvalue` = format(`-log10pvalue`, scientific = TRUE),
            Gene = glue::glue(
                "<a href=\"https://www.genecards.org/Search/Keyword?queryString={Gene}\" target=\"_blank\">{Gene}</a>"
            )
        ) |>
        dplyr::relocate(Gene) |>
        dplyr::rename("log<sub>2</sub>(Fold Change)" = log2FoldChange, "-log<sub>10</sub>(q-value)" = `-log10pvalue`) |>
        dplyr::arrange(Rank)
  
      return(invisible(self$GSEAPathwayData))

    },

    #' @description
    #' Get selected GSEA pathway enrichment plot
    #' @param .data - data used for plot
    #' @param ns - namespace to apply to plot object
    getGSEAEnrichmentPlot = function(.data, ns) {

      p <- GSEA_enrichment_plot(
        path_name = self$GSEATraceName,
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
          filename = glue::glue(
            "{self$applicationName} - {self$Study()} GSEA Enrichment Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
          ),
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

GSEA_enrichment_plot <- function(
  path_name,
  stats,
  res,
  title = "",
  gsea_param = 0
) {

  ticks_size <- 0.4

  pathway_nammed <- res |>
    dplyr::filter(Gene.set == path_name) |>
    dplyr::select(Leading.edge.genes) |>
    dplyr::pull() |>
    (\(x) {stringr::str_split(x, ",") })() |>
    purrr::simplify()

  label <- res |>
    dplyr::filter(Gene.set == path_name) |>
    dplyr::select(NES, q.value) |>
    dplyr::mutate(
      NES = round(NES, 2),
      q.value = formatPValue(q.value, adjustmentMethod = "q"),
      text = as.character(glue::glue("NES = {NES}\n {q.value}"))
    ) |>
    dplyr::pull(text)

  x_label <- length(stats) * 0.99
  y_label <- ((res |> dplyr::filter(Gene.set == path_name))$ES) * 0.95
  rnk <- rank(-stats) # rank highest values first
  ord <- order(rnk) # get correct order
  stats_adj <- stats[ord] # ensure ranked list is ordered correctly
  # gets sign and multiplies by absolute value ^ gsea param
  stats_adj <- sign(stats_adj) * (abs(stats_adj)^gsea_param)
  stats_adj <- stats_adj / max(abs(stats_adj))
  # New; get Zero crossing point
  zero_cross <- stats_adj[stats_adj > 0] |> length()
  pathway <- unname(as.vector(stats::na.omit(match(pathway_nammed, names(stats_adj)))))
  pathway <- sort(pathway)

  gsea_res <- fgsea::calcGseaStat(
    stats_adj,
    selectedStats = pathway,
    returnAllExtremes = TRUE
  )
  bottoms <- gsea_res$bottoms
  tops <- gsea_res$tops
  n <- length(stats_adj)
  xs <- as.vector(rbind(pathway - 1, pathway))
  ys <- as.vector(rbind(bottoms, tops))

  diff <- (max(tops) - min(bottoms)) / 8
  x <- y <- NULL

  es_score <- tibble::tibble(
    x = c(0, xs, n + 1),
    y = c(0, ys, 0)
  ) |>
    dplyr::left_join(
      tibble::tibble(
        x = pathway,
        names = pathway_nammed
      )
      , by = "x"
    ) |>
    dplyr::mutate(
      text = ifelse(
        is.na(names),
        "",
        glue::glue("Gene: {names}\n Rank: {x}\n Enrichment Score: {y}")
      )
    )

  p <- es_score |>
    plotly::plot_ly(
      type = "scatter",
      mode = "lines",
      name = "ES score",
      x = ~ x,
      y = ~ y,
      text = ~ text,
      hoverinfo = "text",
      line = list(
        color = "green",
        width = 2
      )
    )

  gene_ticks <- tibble::tibble(
    x = pathway,
    y = -diff / 2,
    xend = pathway,
    yend = diff / 2,
    names = pathway_nammed
  ) |>
  dplyr::mutate(
    text = glue::glue("Gene: {names}\n Rank: {x}")
  )

  p <- p |>
    plotly::add_segments(
      data = gene_ticks,
      name = "hits",
      x = ~ x,
      y =  ~ y, 
      xend = ~ xend,
      yend = ~ yend,
      text = ~ text,
      hoverinfo = "text",
      line = list(
        color = "black",
        size = ticks_size
      )
    )

  max_x <- max(es_score$x)
  max_y <- max(es_score$y)

  p <- p |>
    plotly::layout(
      showlegend = FALSE,
      title = list(
        text = title,
        x = 0,
        xref = "paper",
        font = list(
          color = "Black",
          family = "Arial",
          size = 22
        )
      ),
      xaxis = list(
        title = list(
          text = "Rank"
        ),
        range = list(-500, max_x * 1.05),
        showgrid = FALSE,
        zeroline = TRUE,
        showline = TRUE
      ),
      yaxis = list(
        title = list(
          text = "Enrichment score"
        ),
        showgrid = FALSE,
        zeroline = TRUE,
        showline = TRUE
      ),
      margin = list(
        t = 75
      ),
      shapes = list(
        list(
          type = "line",
          layer = "below",
          xref = "paper",
          yref = "y",
          axref = "y",
          ayref = "y",
          y0 = max(tops),
          y1 = max(tops),
          x0 = 0,
          x1 = 1,
          text = "",
          hovertext = "test",
          line = list(
            color = "red",
            dash = "dot",
            width = 1
          )
        ),
        list(
          type = "line",
          layer = "below",
          xref = "paper",
          yref = "y",
          axref = "y",
          ayref = "y",
          y0 = min(bottoms),
          y1 = min(bottoms),
          x0 = 0,
          x1 = 1,
          text = "",
          line = list(
            color = "red",
            dash = "dot",
            width = 1
          )
        ),
        list(
          type = "line",
          layer = "below",
          xref = "x",
          yref = "paper",
          axref = "y",
          ayref = "y",
          y0 = 0,
          y1 = 1,
          x0 = zero_cross,
          x1 = zero_cross,
          text = "",
          line = list(
            color = "grey50",
            dash = "dash",
            width = 1
          )
        )
      ),
      annotations = list(
        list(
          x = zero_cross + (n / 8),
          y = 0.025,
          text = paste0("Zero cross at ", zero_cross),
          xref = "x",
          yref = "y",
          showarrow = FALSE,
          font = list(
            color = "Black",
            family = "Arial",
            size = 12
          )
        ),
        list(
          x = x_label,
          y = y_label,
          text = label,
          xref = "x",
          yref = "y",
          showarrow = FALSE,
          font = list(
            color = "Black",
            family = "Arial",
            size = 12
          )
        )
      )
    )
  p

}