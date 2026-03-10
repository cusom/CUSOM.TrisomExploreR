box::use(
    R6[R6Class],
    dplyr[mutate, top_n, pull, arrange, inner_join, select, summarise, filter,
            group_by, ungroup, relocate, rename, left_join,  row_number],
    glue[glue],
    plyr[round_any],
    plotly[plot_ly, layout, config],
    stats[reorder],
    tibble[tibble],
    tidyr[separate_rows],
    purrr[pmap_chr],
    stringr[str_split, str_to_upper]
)

box::use(
    app/logic/shared/string_utils[parse_delimited_string],
    app/logic/shared/statistical_analysis[formatPValue, runfGSEA, calculate_GSEA_scores],
    app/logic/shared/gsea_plots[GSEA_enrichment_plot]
)

#' @export
GSEAPlotStrategy <- R6Class(
    "GSEAPlotStrategy",
    private = list(
        ..event_data = tibble()
    ),
    active = list(
        ranks = function(value) {
            return(self$source_data$ranks)
        },
        hallmarks = function(value) {
            return(self$source_data$hallmarks)
        },
        gsea = function(value) {
            return(self$source_data$gsea)
        },
        top_gene_sets = function(value) {
            return(
                self$gsea |>
                    mutate(
                        `-log10qvalue` = -log(q.value),
                        text = glue(
                            "Gene Set: {Gene.set}
                            NES: {NES}
                            -log<sub>10</sub>(q-value): {`-log10qvalue`}"
                        )
                    ) |>
                    top_n(25, wt = abs(NES))
            )
        },
        gsea_limit = function(value) {
            return(
                self$gsea |>
                    pull(NES) |>
                    abs() |>
                    max() |>
                    round_any(1, f = ceiling) * 1.15
            )
        },
        event_data = function(value) {
            if (missing(value)) {
                return(private$..event_data)
            } else {
                private$..event_data <- value
            }
        },
        event_data_keys = function(value) {
            return(
                tibble("Gene" = self$event_data$customdata) |>
                    separate_rows(Gene, sep = ",") |>
                    inner_join(
                        self$summary_data |>
                            select(Analyte) |>
                            mutate(
                                "Gene" = pmap_chr(
                                    list(Analyte, 1),
                                    parse_delimited_string
                                )
                            ),
                        by = "Gene"
                    ) |>
                    select(Analyte) |>
                    summarise(text = toString(Analyte)) |>
                    mutate(text = gsub(", ", "|", text)) |>
                    pull()
            )
        },
        analytes = function(value) {
            return(
                str_split(self$event_data_keys, "\\|", simplify = TRUE)
            )
        },
        gsea_trace_name = function(value) {
            return(
                self$event_data$y
            )
        },
        gsea_analytes = function(value) {
            if (missing(value)) {
                return(
                    self$gsea_pathway_data |>
                        select(Analyte) |>
                        summarise(text = toString(Analyte)) |>
                        mutate(text = gsub(", ", "|", text)) |>
                        pull()
                )
            }
        },
        gsea_geneset_name = function(value) {
            if (missing(value)) {
                return(glue("HALLMARK_{gsub(' ','_',str_to_upper(self$gsea_trace_name))}"))
            } else {
                stop("GSEAGenesetName is a read-only active binding")
            }
        }
    ),
    public = list(
        source_data = NULL,
        study = NULL,
        summary_data = NULL,
        gsea_pathway_data = NULL,
        initialize = function(study, summary_data) {
            self$study <- study
            self$summary_data <- summary_data
        },
        set_source_data = function(.data) {
            self$source_data <- .data
            return(invisible(self$source_data))
        },
        render_gsea_plot = function(.data) {
            self$set_source_data(.data)

            self$top_gene_sets |>
                arrange(`-log10qvalue`) |>
                plot_ly(
                    type = "bar",
                    x = ~ `-log10qvalue`,
                    y = ~ reorder(Gene.set, NES),
                    hoverinfo = "text",
                    hovertext = ~ text,
                    customdata = ~ Leading.edge.genes,
                    marker = list(
                        color = ~ NES,
                        autocolorscale = FALSE,
                        colorscale = "RdBlu",
                        cauto  = FALSE,
                        cmax = self$gsea_limit,
                        cmid = 0,
                        cmin = -self$gsea_limit,
                        colorbar = list(
                            title = "NES",
                            tickmode = "auto",
                            len = 0.5,
                            yanchor = "middle",
                            y = 0.5
                        )
                    )
                ) |>
                layout(
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
                config(
                    displayModeBar = TRUE,
                    displaylogo = FALSE,
                    toImageButtonOptions = list(
                        format = "svg",
                        filename = glue("{self$applicationName} - GSEA Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
                        width = NULL,
                        height = NULL
                    ),
                    modeBarButtons = list(
                        list("toImage")
                    )
                )

        },
        set_GSEA_pathway_data = function(path_name) {
            pathway_data <- self$gsea |>
                filter(Gene.set == path_name)

            gsea_scores <- calculate_GSEA_scores(
                self$ranks,
                pathway_data
            )

            self$gsea_pathway_data <- self$summary_data |>
                filter(Analyte %in% self$analytes) |>
                select(Analyte, log2FoldChange, `-log10pvalue`) |>
                mutate(
                    Gene = pmap_chr(
                        list(Analyte, 1),
                        parse_delimited_string
                    )
                ) |>
                left_join(gsea_scores, by = "Gene") |>
                group_by(Gene) |>
                arrange(-log2FoldChange) |>
                mutate(r = row_number()) |>
                ungroup() |>
                filter(r == 1) |>
                select(-c(ES, r)) |>
                mutate(
                    log2FoldChange = format(log2FoldChange, scientific = TRUE),
                    `-log10pvalue` = format(`-log10pvalue`, scientific = TRUE),
                    Gene = glue(
                        "<a href=\"https://www.genecards.org/Search/Keyword?queryString={Gene}\" target=\"_blank\">
                            {Gene}
                        </a>"
                    )
                ) |>
                relocate(Gene) |>
                rename(
                    "log<sub>2</sub>(Fold Change)" = log2FoldChange,
                    "-log<sub>10</sub>(q-value)" = `-log10pvalue`
                ) |>
                arrange(Rank)

            return(invisible(self$gsea_pathway_data))
        },
        render_enrichment_plot = function() {

            GSEA_enrichment_plot(
                path_name = self$gsea_trace_name,
                stats = self$ranks,
                res = self$gsea,
                title = glue("T21 vs. Control:\n{self$gsea_trace_name}")
            ) |>
            layout(
                margin = list(
                autoexpand = TRUE,
                l = 10,
                r = 30,
                t = 75
                )
            ) |>
            config(
                displayModeBar = TRUE,
                displaylogo = FALSE,
                toImageButtonOptions = list(
                    format = "svg",
                    filename = glue(
                        "{self$applicationName} - {self$study} GSEA \\
                        Enrichment Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
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
        }
    )
)
