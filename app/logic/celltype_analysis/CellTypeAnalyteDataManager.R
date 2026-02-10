box::use(
    plotly[plot_ly, layout, config],
    glue[glue],
    htmltools[HTML],
    rlang[enquo],
    dplyr[select, mutate, distinct, pull, first]
)

box::use(
    app/logic/shared/plot_utils[getStatAnnotationAnchorLines, getGroupedStatAnnotations]
)

#' @export
get_cell_type_plot <- function(
    .data,
    all_cell_types,
    gene,
    stat_test,
    covariates,
    adjustment_method,
    app_name,
    ns
    ) {

    p <- .data |>
        getGroupedBoxplot(
            key = LabID,
            group = CellType,
            legendGroup = Karyotype,
            value = log2MeasuredValue,
            valueLabel = log2Measurement,
            text = text,
            customData = LabID,
            showJitter = TRUE
        ) |>
        layout(
            showlegend = TRUE,
            legend = list(
                itemclick = "toggleothers",
                title = list(
                text = "<b>Karyotype</b>",
                font = list(
                    family = "Arial",
                    color = "rgb(58, 62, 65)",
                    size = 16
                )
                ),
                font = list(
                family = "Arial",
                color = "rgb(58, 62, 65)",
                size = 16
                )
            ),
            title = list(
                text = HTML(glue("Effect of trisomy 21 on {gene} mRNA expression")),
                font = list(
                    family = "Arial",
                    color = "rgb(58, 62, 65)",
                    size = 24
                ),
                pad = list(
                    t = 10,
                    b = 50
                ),
                y = 1
            ),
            xaxis = list(
                categoryorder = "array",
                categoryarray = all_cell_types,
                fixedrange = TRUE
            ),
            yaxis = list(
                title = list(
                    text = glue("{.data$log2Measurement[1]}"),
                    font = list(
                        family = "Arial",
                        color = "rgb(58, 62, 65)",
                        size = 18
                    )
                ),
                tickfont = list(
                    family = "Arial",
                    color = "rgb(58, 62, 65)",
                    size = 14
                ),
                fixedrange = TRUE
            )
        )

    lines <- .data |>
        getStatAnnotationAnchorLines(
            group = CellType,
            group_members = all_cell_types,
            significance_variable = p.value,
            group_is_significant = p.value.is.significant,
            include_insignificant_values = TRUE
        )

    if (length(lines) > 0) {

        significance_annotations <- getGroupedStatAnnotations(
            AnnotationAnchorLines = lines,
            statTest = stat_test,
            covariates = covariates,
            adjustmentMethod = adjustment_method,
            formatInsignificantValues = TRUE
        )

        p <- p |>
            layout(
                shapes = lines,
                annotations = significance_annotations
            )
    }

    p <- p |>
        config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
                format = "svg",
                filename = glue(
                    "{app_name} - {gene} Cell Type Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
                ),
                width = NULL,
                height = NULL
            ),
            modeBarButtons = list(
                list("toImage")
            )
        )

    p$x$source <- ns("plot")

    return(p)

}

getGroupedBoxplot <- function(
    .data,
    key,
    group,
    legendGroup,
    value,
    valueLabel,
    text,
    customData,
    showJitter = TRUE
) {

    .key <- enquo(key)
    .group <- enquo(group)
    .legendGroup <- enquo(legendGroup)
    .value <- enquo(value)
    .valueLabel <- enquo(valueLabel)
    .text <- enquo(text)
    .customData <- enquo(customData)

    if (nrow(.data) > 0) {

        y_lab <- .data |> pull(!!.valueLabel) |> first()
        key <- .data |> pull(!!.key)
        x <- .data |> pull(!!.group)
        y <- .data |> pull(!!.value)
        lg <- .data |> pull(!!.legendGroup)
        text <- .data |> pull(!!.text)
        cd <- .data |> pull(!!.customData)
        j <- ifelse(showJitter, "all", "none")

        p <- plot_ly(
            x = x,
            y = y,
            legendgroup = lg,
            color = lg,
            type = "box",
            text = text,
            hoverinfo = "text",
            key = key,
            customdata = cd,
            colors = c("#BBBDC0", "#287BA5"),
            boxpoints = j,
            pointpos = 0
        ) |>
        layout(
            showlegend = TRUE,
            boxmode = "group",
            title = list(
                x = 0.05,
                font = list(
                    family = "Noto Serif', serif",
                    size = 24,
                    color = "rgb(0, 79, 128)"
                )
            ),
            xaxis = list(
                title = "",
                titlefont =  list(
                    family = "Arial",
                    color = "rgb(58, 62, 65)",
                    size = 18
                ),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = TRUE,
                showticklabels = TRUE
            ),
            yaxis = list(
                title = y_lab,
                titlefont = list(
                    family = "Arial",
                    color = "rgb(58, 62, 65)",
                    size = 18
                ),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = TRUE,
                showticklabels = TRUE
            ),
            margin = list(
                autoexpand = TRUE,
                l = 100,
                r = 100,
                t = 100,
                b = 50
            )
        )

        return(p)

    } else {
        return(NULL)
    }

}