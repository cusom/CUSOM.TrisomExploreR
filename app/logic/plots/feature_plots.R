box::use(
  dplyr[mutate, add_tally, filter, select, distinct, pull],
)

#' @export
getBoxPlotWithHighlightGroup <- function(
  .data,
  key,
  group,
  groupBaselineLabel,
  value,
  valueLabel,
  text,
  highlightGroup,
  colors = c("#BBBDC0", "#287BA5"),
  highlightColors = c("orange", "red"),
  plotName
  ) {

  .key <- rlang::enquo(key)
  .group <- rlang::enquo(group)
  .value <- rlang::enquo(value)
  .valueLabel <- rlang::enquo(valueLabel)
  .text <- rlang::enquo(text)
  .highlightGroup <- rlang::enquo(highlightGroup)
  baselineColor <- colors[1]
  comparisonColor <- colors[2]

  if (nrow(.data) > 0) {

    .data <- .data |>
      dplyr::mutate(
        key = !!.key,
        group = !!.group,
        value = !!.value,
        text = !!.text
      ) |>
      dplyr::add_tally() |>
      dplyr::mutate(
        x = stats::rnorm(
          n,
          mean = ifelse(!!.group == groupBaselineLabel, -1, 1
        ),
        sd = 0.15)
      )

    yVariableLabel <- .data |>
      dplyr::distinct(!!.valueLabel) |>
      dplyr::pull()

    baseline <- .data |>
      dplyr::filter(!!.group == groupBaselineLabel)

    comparison <- .data |>
      dplyr::filter(!!.group != groupBaselineLabel)

    highlightGroups <- .data |>
      dplyr::select(!!.highlightGroup) |>
      tidyr::drop_na() |>
      dplyr::distinct() |>
      dplyr::pull()

    highlight_A_baseline <- .data |>
      dplyr::filter(
        !!.group == groupBaselineLabel,
        !!.highlightGroup == highlightGroups[1]
      )

    highlight_A_comparison <- .data |>
      dplyr::filter(
        !!.group != groupBaselineLabel,
        !!.highlightGroup == highlightGroups[1]
      )

    highlight_B_baseline <- .data |>
      dplyr::filter(
        !!.group == groupBaselineLabel,
        !!.highlightGroup != highlightGroups[1]
      )

    highlight_B_comparison <- .data |>
      dplyr::filter(
        !!.group != groupBaselineLabel,
        !!.highlightGroup != highlightGroups[1]
      )

    p1 <- plotly::plot_ly(
      type = "box",
      colors = baselineColor
      ) |>
      plotly::add_boxplot(
        y = baseline$value,
        x = -1,
        type = "box",
        boxpoints = FALSE,
        name = baseline$group,
        color = baseline$group,
        legendgroup = "baseline"
      ) |>
      plotly::add_markers(
        y = baseline$value,
        text = baseline$text,
        hoverinfo = "text",
        key = baseline$key,
        x = baseline$x,
        marker = list(
          color = baselineColor,
          size = 8
        ), 
        showlegend = FALSE,
        legendgroup = "baseline"
      )

    if (nrow(highlight_A_baseline) > 0) {
      p1 <- p1 |>
        plotly::add_markers(
          y = highlight_A_baseline$value,
          text = highlight_A_baseline$text,
          hoverinfo = "text",
          x = highlight_A_baseline$x,
          marker = list(
            color = highlightColors[1],
            size = 8
          ),
          showlegend = TRUE,
          name = "Group A",
          legendgroup = "groupA"
        )
    }

    if (nrow(highlight_B_baseline) > 0) {
      p1 <- p1 |>
        plotly::add_markers(
          y = highlight_B_baseline$value,
          text = highlight_B_baseline$text,
          hoverinfo = "text", 
          x = highlight_B_baseline$x,
          marker = list(
            color = highlightColors[2],
            size = 8
          ),
          showlegend = TRUE,
          name = "Group B",
          legendgroup = "groupB"
        )
    }

    p1 <- p1 |>
      plotly::layout(
        showlegend = TRUE,
        #legendgroup = "baseline",
        title = "",
        xaxis = list(
          title = list(
            text = "",
            standoff = 0,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = FALSE,
          fixedrange = TRUE
        ),
        yaxis = list(
          title = list(
            text = yVariableLabel,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          fixedrange = TRUE
        )
      )

    p2 <- plotly::plot_ly(
      type = "box", 
      colors = comparisonColor
      ) |>
      plotly::add_boxplot(
        y = comparison$value,
        x = 1,
        type = "box",
        boxpoints = FALSE,
        name = comparison$group,
        color = comparison$group,
        legendgroup = "comparison"
      ) |>
      plotly::add_markers(
        y = comparison$value,
        text = comparison$text,
        hoverinfo = "text",
        key = comparison$key,
        x = comparison$x,
        marker = list(
          color = comparisonColor,
          size = 8
        ),
        showlegend = FALSE,
        legendgroup = "comparison"
      )

    if (nrow(highlight_A_comparison) > 0) {
      showLegend <- ifelse(nrow(highlight_A_baseline) > 0, FALSE, TRUE)
      p2 <- p2 |>
        plotly::add_markers(
          y = highlight_A_comparison$value,
          text = highlight_A_comparison$text,
          hoverinfo = "text",
          x = highlight_A_comparison$x,
          marker = list(
            color = highlightColors[1],
            size = 8
          ),
          showlegend = showLegend,
          name = "Group A",
          legendgroup = "groupA"
        )
    }

    if (nrow(highlight_B_comparison) > 0) {
      showLegend <- ifelse(nrow(highlight_B_baseline) > 0, FALSE, TRUE)
      p2 <- p2 |>
        plotly::add_markers(
          y = highlight_B_comparison$value,
          text = highlight_B_comparison$text,
          hoverinfo = "text",
          x = highlight_B_comparison$x,
          marker = list(
            color = highlightColors[2],
            size = 8
          ),
          showlegend = showLegend,
          name = "Group B",
          legendgroup = "groupB"
        )
    }

    p2 <- p2 |>
      plotly::layout(
        showlegend = TRUE,
        #legendgroup = "comparison",
        title = "",
        xaxis = list(
          title = list(
            text = "",
            standoff = 0,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = FALSE,
          fixedrange = TRUE
        ),
        yaxis = list(
          title = list(
            text = yVariableLabel,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          fixedrange = TRUE
        )
      )

    p <- plotly::subplot(
        p1,
        p2,
        shareX = TRUE,
        shareY = TRUE,
        margin = 0.0
      ) |>
      plotly::layout(
        showlegend = TRUE,
        legend = list(
          title = list(
            text = "",
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          font = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 14
          )
        ),
        title = list(
          font = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 18
          ),
          pad = list(
            t = 10,
            l = 5
          ),
          x = 0,
          xanchor = "left",
          xref = "container",
          y = 1
        ),
        margin = list(
          autoexpand = TRUE,
          l = 25,
          r = 15,
          t = 20,
          b = 20
        )
      )

    p$x$source <- paste0(plotName, "BoxPlot")

    return(p)

  } else {

    return(NULL)

  }

}

#' @export
getScatterPlotByGroup <- function(
  .data,
  key,
  x,
  y,
  group,
  groupBaselineLabel,
  text,
  addFitLines = TRUE,
  plotName
) {

  key <- rlang::enquo(key)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  text <- rlang::enquo(text)

  if (nrow(.data) > 0) {

    groups <- .data |>
      dplyr::select(!!group) |>
      dplyr::distinct() |>
      dplyr::pull()

    xRange <- c(
      round(min(.data[[rlang::quo_name(x)]])),
      round(max(.data[[rlang::quo_name(x)]])) + 1
      )

    data1 <- .data |>
      dplyr::filter(!!group == groupBaselineLabel) |>
      dplyr::mutate(
        `:=`(name, groupBaselineLabel),
        color = "#BBBDC0"
      )

    data2 <- .data |>
      dplyr::filter(!!group != groupBaselineLabel) |>
      dplyr::mutate(
        `:=`(name, groups[which(groups !=  groupBaselineLabel)]),
        color = "#287BA5"
      )

    p <- plotly::plot_ly(
      type = "scatter",
      mode = "markers"
    )

    p <- p |>
      plotly::add_trace(
        data = data1,
        type = "scatter",
        mode = "markers",
        x = x,
        y = y,
        text = ~ text,
        hoverinfo = "text",
        name = ~ name,
        legendgroup = ~ name,
        marker = list(
          color = ~ color
        )
      ) |>
      plotly::add_trace(
        data = data2,
        type = "scatter",
        mode = "markers",
        x = x,
        y = y,
        text = ~ text,
        hoverinfo = "text",
        name = ~ name,
        legendgroup = ~ name,
        marker = list(
            color = ~ color
        )
      )

    if (addFitLines) {

      lmformula <- paste(rlang::quo_name(y), " ~ ", rlang::quo_name(x))

      fit1 <- .data |>
        dplyr::select(!!x, !!y) |>
        tidyr::nest(
          data = c(!!x, !!y)
        ) |>
        dplyr::mutate(
          fit = purrr::map(
            data,
            ~stats::lm(lmformula, data = .x)$fit
          )
        ) |>
        tidyr::unnest()

      fit1CI <- .data |>
        dplyr::select(!!x, !!y) |>
        tidyr::nest(data = c(!!x, !!y)) |>
        dplyr::mutate(
          fit = purrr::map(
            data,
            ~broom::augment(
              stats::lm(lmformula, data = .x),
              se_fit = TRUE
            )
          )
        ) |>
        tidyr::unnest(fit) |>
        dplyr::mutate(
            ymin = .fitted - 1.96 * .se.fit,
            ymax = .fitted + 1.96 * .se.fit
        ) |>
        dplyr::select(ymin, ymax)

        if (length(groups) == 2) {

          fit2 <- .data |>
            dplyr::filter(!!group == groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fit = purrr::map(
                data,
                ~stats::lm(lmformula, data = .x)$fit
              )
            ) |>
            tidyr::unnest() |>
            dplyr::mutate(
              `:=`(name, groupBaselineLabel),
              color = "rgb(81, 81, 81)"
            )

          fit2CI <- .data |>
            dplyr::filter(!!group == groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fit = purrr::map(
                data,
                ~broom::augment(
                  stats::lm(lmformula, data = .x),
                  se_fit = TRUE
                )
              )
            ) |>
            tidyr::unnest(fit) |>
            dplyr::mutate(
              ymin = .fitted - 1.96 * .se.fit,
              ymax = .fitted + 1.96 * .se.fit
            ) |>
            dplyr::select(ymin, ymax)

          fit3 <- .data |>
            dplyr::filter(!!group != groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fit = purrr::map(
                data,
                ~stats::lm(lmformula, data = .x)$fit
              )
            ) |>
            tidyr::unnest() |>
            dplyr::mutate(
              `:=`(name, groups[which(groups !=  groupBaselineLabel)]),
              color = "rgb(48, 128, 255)"
            )

          fit3CI <- .data |>
            dplyr::filter(!!group != groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fitted = purrr::map(
                data,
                ~broom::augment(
                  stats::lm(lmformula, data = .x),
                  se_fit = TRUE
                )
              )
            ) |>
            tidyr::unnest(fitted) |>
            dplyr::mutate(
              ymin = .fitted - 1.96 * .se.fit,
              ymax = .fitted + 1.96 * .se.fit
            ) |>
            dplyr::select(ymin, ymax)

            p <- p |>
              plotly::add_trace(
                data = fit2,
                type = "scatter",
                x = x,
                y = ~ fit,
                mode = "lines",
                name = ~ name,
                legendgroup = ~ name,
                showlegend = FALSE,
                line = list(
                  color = ~ color,
                  width = 2
                )
              ) |>
              plotly::add_ribbons(
                x = x,
                ymin = fit2CI$ymin,
                ymax = fit2CI$ymax,
                line = list(
                  color = ~ color
                ),
                fillcolor = ~ color,
                name = "",
                legendgroup = ~ name,
                showlegend = FALSE,
                opacity = 0.3
              ) |>
              plotly::add_trace(
                data = fit3,
                type = "scatter",
                x = x,
                y = ~ fit,
                mode = "lines",
                name = ~ name,
                legendgroup = ~ name,
                showlegend = FALSE,
                line = list(
                  color = ~ color,
                  width = 2
                )
              ) |>
              plotly::add_ribbons(
                x = x,
                ymin = fit3CI$ymin,
                ymax = fit3CI$ymax,
                line = list(
                  color = ~ color
                ),
                fillcolor = ~ color,
                name = "",
                legendgroup = ~ name,
                showlegend = FALSE,
                opacity = 0.3
              )
        } else {

          p <- p |>
            plotly::add_trace(
              data = fit1,
              type = "scatter",
              x = x,
              y = ~ fit,
              mode = "lines",
              name = groups,
              legendgroup = ~ groups,
              showlegend = FALSE,
              line = list(
                color = ifelse(
                  groups == groupBaselineLabel,
                  "rgb(81, 81, 81)",
                  "rgb(48, 128, 255)"
                ),
                width = 2
                )
              ) |>
            plotly::add_ribbons(
              x = x,
              ymin = fit1CI$ymin,
              ymax = fit1CI$ymax,
              line = list(
                color = ifelse(
                  groups == groupBaselineLabel,
                  "rgb(81, 81, 81)", 
                  "rgb(48, 128, 255)"
                )
              ),
              fillcolor = ifelse(
                groups == groupBaselineLabel,
                "rgb(81, 81, 81)", 
                "rgb(48, 128, 255)"
              ),
              name = "",
              legendgroup = ~ groups,
              showlegend = FALSE,
              opacity = 0.3
            )
        }

      }

    p <- p |>
      plotly::layout(
        showlegend = TRUE,
        legend = list(
          title = list(
            text = "",
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          font = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 14
          )
        ),
        title = list(
          font = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 18
          ),
          pad = list(
            t = 10,
            l = 5
          ),
          x = 0,
          xanchor = "left",
          xref = "container",
          y = 1
        ),
        xaxis = list(
          title = list(
            standoff = 10,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          range = xRange,
          fixedrange = FALSE
        ),
        yaxis = list(
          title = list(
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          fixedrange = FALSE
        ),
        margin = list(
          autoexpand = TRUE,
          l = 10,
          r = 30,
          t = 30
        )
      )

    p$x$source <- paste0(plotName, "ScatterPlot")

    return(p)

  } else {

    return(NULL)

  }

}