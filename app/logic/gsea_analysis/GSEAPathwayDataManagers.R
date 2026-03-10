box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble, deframe],
    dplyr[select, filter, mutate, distinct, arrange],
    purrr[pmap_chr, map_chr],
    stringr[str_to_title],
    rlang[sym],
)

box::use(
    app/logic/shared/statistical_analysis[runfGSEA],
    app/logic/shared/string_utils[parse_delimited_string]
)


GSEADataSourceBase <- R6Class(
    "GSEADataSourceBase",
    private = list(
        app_config = NULL,
        analysis_config = NULL,
        remote_db = NULL
    ),
    active = list(
        gsea_data = function(value) {
            return(
                list(
                    "ranks" = self$gsea_ranks,
                    "hallmarks" = self$gsea_hallmarks,
                    "gsea" = self$gsea
                )
            )
        }
    ),
    public = list(
        study = NULL,
        summary_data = NULL,
        gsea_hallmarks = NULL,
        gsea_ranks = NULL, 
        gsea = NULL,
        gsea_pathway_data = NULL,
        initialize = function(study, summary_data) {
            self$study <- study
            self$summary_data <- summary_data
            self$gsea_hallmarks <- readRDS("app/logic/app_resources/data/GSEA_hallmarks.rds")
        },
        get_data = function() {
            self$set_gsea_ranks()
            self$set_gsea()
            return(invisible(self$gsea_data))
        },
        set_gsea_ranks = function() {
            self$gsea_ranks <- self$summary_data |>
                select(Analyte, `-log10pvalue`, log2FoldChange) |>
                mutate(
                    ParsedComparisonAnalyte = pmap_chr(
                        list(Analyte, 1),
                        parse_delimited_string
                    ),
                    ID = ParsedComparisonAnalyte,
                    t = (`-log10pvalue` * log2FoldChange)
                ) |>
                select(ID, t) |>
                filter(!is.na(t)) |>
                arrange(-abs(t)) |>
                distinct(ID, .keep_all = TRUE) |>
                deframe()
        },
        set_gsea = function() {
            self$gsea <- runfGSEA(geneset = self$gsea_hallmarks, ranks = self$gsea_ranks) |>
                mutate(
                    Leading.edge.genes = map_chr(leadingEdge, toString),
                    Leading.edge.genes = gsub(" ", "", Leading.edge.genes)
                ) |>
                select(
                    "Gene.set" = pathway,
                    "Size" = size,
                    ES,
                    NES,
                    "p.value" = pval,
                    "q.value" = padj,
                    Leading.edge.genes
                ) |>
                mutate(
                    Gene.set = str_to_title(trimws(gsub("_", " ", gsub("HALLMARK", "", Gene.set))))
                )
        }
    )
)

#' @export
GSEADataSourceSOMA <- R6Class(
    "GSEADataSourceSOMA",
    inherit = GSEADataSourceBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(study, summary_data) {
            super$initialize(study, summary_data)
        }
    )
)

#' @export
GSEADataSourceRNA <- R6Class(
    "GSEADataSourceRNA",
    inherit = GSEADataSourceBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(study, summary_data) {
            super$initialize(study, summary_data)
        }
    )
)
