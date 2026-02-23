
box::use(
    dplyr[filter, select, pull, mutate, left_join],
    stringr[str_split],
    purrr[simplify],
    tibble[tibble],
    glue[glue],
    plotly[plot_ly, add_segments, layout],
    fgsea[calcGseaStat],
    stats[na.omit]
)

box::use(
    app/logic/shared/statistical_analysis[formatPValue]
)


#' @export
GSEA_enrichment_plot <- function(
    path_name,
    stats,
    res,
    title = "",
    gsea_param = 0
) {

    ticks_size <- 0.4

    pathway_nammed <- res |>
        filter(Gene.set == path_name) |>
        select(Leading.edge.genes) |>
        pull() |>
        (\(x) {str_split(x, ",") })() |>
        simplify()

    label <- res |>
        filter(Gene.set == path_name) |>
        select(NES, q.value) |>
        mutate(
            NES = round(NES, 2),
            q.value = formatPValue(q.value, adjustmentMethod = "q"),
            text = as.character(glue("NES = {NES}\n {q.value}"))
        ) |>
        pull(text)

    x_label <- length(stats) * 0.99
    y_label <- ((res |> filter(Gene.set == path_name))$ES) * 0.95
    rnk <- rank(-stats) # rank highest values first
    ord <- order(rnk) # get correct order
    stats_adj <- stats[ord] # ensure ranked list is ordered correctly
    # gets sign and multiplies by absolute value ^ gsea param
    stats_adj <- sign(stats_adj) * (abs(stats_adj)^gsea_param)
    stats_adj <- stats_adj / max(abs(stats_adj))
    # New; get Zero crossing point
    zero_cross <- stats_adj[stats_adj > 0] |> length()
    pathway <- unname(as.vector(na.omit(match(pathway_nammed, names(stats_adj)))))
    pathway <- sort(pathway)

    gsea_res <- calcGseaStat(
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

    es_score <- tibble(
        x = c(0, xs, n + 1),
        y = c(0, ys, 0)
    ) |>
        left_join(
            tibble(
                x = pathway,
                names = pathway_nammed
            )
            , by = "x"
        ) |>
        mutate(
            text = ifelse(
                is.na(names),
                "",
                glue("Gene: {names}\n Rank: {x}\n Enrichment Score: {y}")
            )
        )

    p <- es_score |>
        plot_ly(
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

    gene_ticks <- tibble(
        x = pathway,
        y = -diff / 2,
        xend = pathway,
        yend = diff / 2,
        names = pathway_nammed
    ) |>
    mutate(
        text = glue("Gene: {names}\n Rank: {x}")
    )

    p <- p |>
        add_segments(
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
        layout(
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