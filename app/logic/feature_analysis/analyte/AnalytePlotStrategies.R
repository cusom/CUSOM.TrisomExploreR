box::use(
    R6[R6Class],
    dplyr[select, filter, distinct, mutate, case_when, rowwise, ungroup, pull,
        row_number, arrange],
    stringr[str_split, str_split_1],
    glue[glue],
    purrr[pmap],
    forcats[fct_inorder],
    plyr[round_any],
    plotly[layout, config, colorbar, ggplotly],
    htmlwidgets[onRender],
    rlang[sym],
    heatmaply[heatmaply],
    RColorBrewer[brewer.pal],
    circlize[colorRamp2],
    htmltools[HTML]
)

box::use(
    app/logic/shared/statistical_analysis[formatPValue],
    app/logic/shared/string_utils[parse_delimited_string],
    app/logic/shared/analyte_plots[getBoxPlotWithHighlightGroup, getScatterPlotByGroup,
        getScatterPlotWithSmoothing, getDensityColors],
)

PlotStrategyBase <- R6Class(
    "PlotStrategyBase",
    private = list(
        remote_db = NULL
    ),
    active = list(
        GroupVariableCount = function(value) {
            if (missing(value)) {
                return(
                    length(
                        str_split(self$analysisVariableLabel, pattern = ";", simplify = TRUE)
                    )
                )
            }
        },
        Karyotype = function(value) {
            return(
                self$analyte_data |>
                    distinct(Karyotype) |>
                    pull()
            )
        },
        AnalytePlotTitle =  function(value) {
            if (missing(value)) {
                stop("Abstract: must implement")
            }
        },
        AnalytePlotXAxisLabel = function(value) {
            if (missing(value)) {
                if (self$namespace == "Comorbidity") {
                    return(
                        glue("Has Any {self$analysisVariableLabel}")
                    )
                } else {
                    return(
                        self$analysisVariableLabel
                    )
                }
            }
        },
        measurement_label = function(value) {
            return(
                as.character(
                    self$analyte_data[1, "Measurement"]
                )
            )
        },
        formattedGroupBaselineLabel = function(value) {
            if (missing(value)) {
                return(
                    self$analyte_data |>
                        select(!!sym(self$analysisVariable)) |>
                        distinct() |>
                        filter(grepl(self$groupBaselineLabel, !!sym(self$analysisVariable))) |>
                        pull()
                )
            }
        }
    ),
    public = list(
        study = NULL,
        study_data = NULL,
        analyte = NULL,
        summary_data = NULL,
        applicationName = NULL,
        namespace = NULL,
        analysisVariable = NULL,
        analysisVariableLabel = NULL,
        analysisType = NULL,
        is_precalculated = NULL,
        experimentIDs = NULL,
        groupBaselineLabel = NULL,
        analyte_data = NULL,
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            self$study <- study
            self$study_data <- study_data
            self$analyte <- analyte
            self$summary_data <- summary_data

            private$remote_db <- app_config$remote_db
            self$applicationName <- analysis_config$ApplicationName

            self$namespace <- analysis_config$Namespace
            self$analysisVariable <- analysis_config$AnalysisVariableName
            self$analysisVariableLabel <- analysis_config$AnalysisVariableLabel
            self$analysisType <- analysis_config$AnalysisType
            self$is_precalculated <- analysis_config$UsesPreCalculatedData

            self$experimentIDs <- str_split_1(analysis_config$ExperimentIDs, "\\|")
            self$groupBaselineLabel <- analysis_config$AnalysisVariableBaselineLabel
        },
        set_analyte_data = function(.data) {
            self$analyte_data <- .data
            return(invisible(self$analyte_data))
        },
        render = function(.data) {
            stop("Abstract: must implement")
        }
    )
)

#' @export
BoxPlotStrategy <- R6Class(
    "BoxPlotStrategy",
    inherit = PlotStrategyBase,
    private = list(),
    active = list(
        AnalytePlotTitle =  function(value) {
            if (missing(value)) {
                return(glue("Effect of {self$analysisVariableLabel} on {self$analyte}"))
            }
        },
        AnalytePlotStatAnnotation = function(value) {
            return(
                self$summary_data |>
                    filter(Analyte == self$analyte) |>
                    ungroup() |>
                    select(p.value, p.value.adjustment.method) |>
                    mutate(
                        formatted.p.value = unlist(
                            pmap(
                                .l = list(p.value, p.value.adjustment.method),
                                formatPValue
                            )
                        )
                    ) |>
                    select(formatted.p.value)
            )
        }
    ),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            super$initialize(analysis_config, app_config, study, study_data, analyte, summary_data)
        },
        render = function(.data) {
            self$set_analyte_data(.data) |>
                getBoxPlotWithHighlightGroup(
                    key = LabID,
                    group = !!sym(self$analysisVariable),
                    groupBaselineLabel = self$formattedGroupBaselineLabel,
                    value = log2MeasuredValue,
                    valueLabel = log2Measurement,
                    text = text,
                    highlightGroup = highlightGroup
                ) |>
                layout(
                    showlegend = TRUE,
                    legend = list(
                        orientation = "h",
                        itemclick = "toggleothers",
                        itemsizing = "constant",
                        itemwidth = 30,
                        valign = "middle",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.10,
                        title = list(
                        text = ""
                        )
                    ),
                    title = list(
                        text = self$AnalytePlotTitle
                    ),
                    annotations = list(
                        list(
                            x = 0.5,
                            y = -0.07,
                            text = glue("<b>{self$AnalytePlotXAxisLabel}</b>"),
                            xref = "paper",
                            yref = "paper",
                            axref = "x",
                            ayref = "y",
                            showarrow = FALSE,
                            ax = 0,
                            ay = 0,
                            font = list(
                                family = "Arial",
                                color = "rgb(58, 62, 65)",
                                size = 14
                            )
                        ),
                        list(
                            x = 0.5,
                            y = 1.025,
                            text = glue("{self$AnalytePlotStatAnnotation}"),
                            xref = "paper",
                            yref = "paper",
                            axref = "x",
                            ayref = "y",
                            ax = 0,
                            ay = 0,
                            font = list(
                                family = "Arial",
                                color = "rgb(58, 62, 65)",
                                size = 12
                            )
                        ),
                        list(
                            x = 0.5,
                            y = 1,
                            xref = "x domain",
                            yref = "paper",
                            axref = "x domain",
                            ax = 1.5,
                            ay = 1,
                            showarrow = TRUE,
                            arrowcolor = "black",
                            arrowhead = 0,
                            arrowwidth = 0.9
                        )
                    ),
                    margin = list(t = 75)
                ) |>
                config(
                    displayModeBar = TRUE,
                    displaylogo = FALSE,
                    toImageButtonOptions = list(
                        format = "svg",
                        filename = glue(
                            "{self$applicationName} - {self$analyte} Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
                        ),
                        width = NULL,
                        height = NULL
                    ),
                    modeBarButtons = list(
                        list("select2d"),
                        list("lasso2d"),
                        list("toImage")
                    )
                ) |>
                onRender(
                    'function(el) {
                        el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
                    }'
                )
        }
    )
)

#' @export
ScatterPlotStrategy <- R6Class(
    "ScatterPlotStrategy",
    inherit = PlotStrategyBase,
    private = list(),
    active = list(
        AnalytePlotTitle =  function(value) {
            if (missing(value)) {
                if (self$GroupVariableCount == 1) {
                        return(
                            glue(
                                "Effect of {self$analysisVariableLabel} in {self$Karyotype} on {self$analyte}"
                            )
                        )
                    } else {
                        return(
                            glue(
                                "Comparison of {self$analysisVariableLabel} \\
                                trajectories between karyotype for {self$analyte}"
                            )
                        )
                    }
            }
        }
    ),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            super$initialize(analysis_config, app_config, study, study_data, analyte, summary_data)
        },
        render = function(.data) {
            self$set_analyte_data(.data) |>
                getScatterPlotByGroup(
                    key = LabID,
                    x = !!sym(self$analysisVariable),
                    y = log2MeasuredValue,
                    group = Karyotype,
                    groupBaselineLabel = "Control",
                    text = text,
                    addFitLines = TRUE
                ) |>
                layout(
                    legend = list(
                        orientation = "h",
                        itemclick = "toggleothers",
                        itemsizing = "constant",
                        itemwidth = 30,
                        valign = "middle",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.10,
                        title = list(
                        text = ""
                        )
                    ),
                    title = list(
                        text = self$AnalytePlotTitle
                    ),
                    xaxis = list(
                        title = list(
                            text = self$analysisVariable
                        )
                    ),
                    yaxis = list(
                        title = list(
                            text = glue("{.data$log2Measurement[1]}")
                        )
                    ),
                    annotations = list(
                        list(
                            x = 0.5,
                            y = 1.025,
                            text = glue("{self$AnalytePlotStatAnnotation}"),
                            xref = "paper",
                            yref = "paper",
                            axref = "x",
                            ayref = "y",
                            ax = 0,
                            ay = 0,
                            font = list(
                                family = "Arial",
                                color = "rgb(58, 62, 65)",
                                size = 12
                            )
                        )
                    ),
                    margin = list(t = 75)
                ) |>
                config(
                    displayModeBar = TRUE,
                    displaylogo = FALSE,
                    toImageButtonOptions = list(
                        format = "svg",
                        filename = glue(
                            "{self$applicationName} - {self$analyte} Analyte Plot \\
                                {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
                        ),
                        width = NULL,
                        height = NULL
                    ),
                    modeBarButtons = list(
                        list("toImage")
                    )
                ) |>
                onRender(
                    'function(el) {
                        el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
                    }'
                )

        }
    )
)

#' @export
HeatmapPlotStrategy <- R6Class(
    "HeatmapPlotStrategy",
    inherit = PlotStrategyBase,
    private = list(),
    active = list(
        long_data = function(value) {
            if (missing(value)) {
                return(
                    self$analyte_data |>
                        select(name = Analyte, variable = Analysis, value = log2FoldChange)
                )
            }
        },
        heatmap_data = function(value) {
            if (missing(value)) {
                return(
                    self$long_data |>
                        select(name = Analyte, variable = Analysis, value = log2FoldChange) |>
                        select("Analyte" = name, z = value) |>
                        arrange(z) |>
                        mutate(r = row_number())
                )
            }
        },
        data_limit = function(value) {
            return(
                self$analyte_data |>
                    pull(log2FoldChange) |>
                    abs() |>
                    max() |>
                    round_any(0.01, f = ceiling)
            )
        },
        AnalytePlotStatAnnotation = function(value) {
            return("")
        }
    ),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            super$initialize(analysis_config, app_config, study, study_data, analyte, summary_data)
        },
        render = function(.data) {

            self$set_analyte_data(.data)

            heatmaply(
                long_data = self$long_data,
                dendrogram = "none",
                xlab = "",
                ylab = "",
                key = ~ name,
                showticklabels = c(FALSE, TRUE),
                main = HTML(glue("Fold Change with {self$analysisVariableLabel}")),
                margins = c(60, 100, 40, 20),
                subplot_widths = 0.65,
                yaxis_width = 10,
                grid_color = "white",
                grid_width = 0.001,
                titleX = TRUE,
                limits = c(-self$data_limit, self$data_limit),
                col = brewer.pal(11, "RdBu") |> rev(),
                scale_fill_gradient_fun = colorRamp2(
                    seq(-self$data_limit, self$data_limit, length.out = 11),
                    brewer.pal(11, "RdBu") |> rev()
                ),
                key.title = "log<sub>2</sub>(Fold Change)",
                branches_lwd = 0.1,
                fontsize_row = 10,
                fontsize_col = 1,
                heatmap_layers = theme(axis.line = element_blank()),
                plot_method = "plotly",
                colorbar_len = 0.5,
                colorbar_yanchor = "middle",
                colorbar_ypos = 0.5,
                custom_hovertext = as.matrix(
                    self$analyte_data$text
                )
            ) |>
            colorbar(
                tick0 = -self$data_limit,
                dtick = self$data_limit
            ) |>
            layout(
                title = list(
                    text = HTML(glue("Fold Change with {self$analysisVariableLabel}")),
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
                    list(fixedrange = TRUE)
                )
            ) |>
            config(
                displayModeBar = TRUE,
                displaylogo = FALSE,
                toImageButtonOptions = list(
                    format = "svg",
                    filename = glue("{self$applicationName} - Heatmap {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
                    width = NULL,
                    height = NULL
                ),
                modeBarButtons = list(
                    list("toImage")
                )
            ) |>
            onRender('
                function(el) {
                    el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
                }'
            )
        }
    )
)

#' @export
ScatterPlotWithSmoothingStrategy <- R6Class(
    "ScatterPlotWithSmoothingStrategy",
    inherit = PlotStrategyBase,
    private = list(),
    active = list(
        ComparisonAnalyteLabel = function(value) {
            return(self$analyte_data[1, "yLabel"])
        },
        ComparisonMeasurement = function(value) {
            return(self$analyte_data[1, "Measurement.y"])
        },
        QueryAnalyteLabel = function(value) {
            return(
                self$analyte_data |>
                    distinct(QueryAnalyte) |>
                    pull()
            )
        },
        QueryMeasurement = function(value) {
            return(self$analyte_data[1, "Measurement.x"])
        },
        CorrelationAnalytePlotTitle = function(value) {
            return(
                glue(
                    "{parse_delimited_string(self$analyte, 1)} \\
                    vs. {parse_delimited_string(self$QueryAnalyteLabel, 1)}"
                )
            )
        },
        AnalytePlotStatAnnotation = function(value) {
            return(
                self$summary_data |>
                    filter(Analyte == self$analyte) |>
                    ungroup() |>
                    select(p.value, p.value.adjustment.method) |>
                    mutate(
                        formatted.p.value = unlist(
                            pmap(
                                .l = list(p.value, p.value.adjustment.method),
                                formatPValue
                            )
                        )
                    ) |>
                    select(formatted.p.value)
            )
        }
    ),
    public = list(
        initialize = function(analysis_config, app_config, study, study_data, analyte, summary_data) {
            super$initialize(analysis_config, app_config, study, study_data, analyte, summary_data)
        },
        render = function(.data) {
            self$set_analyte_data(.data) |>
                mutate(Density = getDensityColors(x, y, transform = TRUE)) |>
                arrange(Density) |>
                ungroup() |>
                mutate(
                    text = glue(
                        "
                        {xLabel} log<sub>2</sub>({self$ComparisonMeasurement}):{round(log2x,2)}
                        {yLabel} log<sub>2</sub>({self$QueryMeasurement}):{round(log2y,2)}
                        Density: {Density}
                        "
                    )
                ) |>
                getScatterPlotWithSmoothing(
                    xVar = log2x,
                    yVar = log2y,
                    colorVar = Density,
                    textVar =  text,
                    smoothingMethod = "lm"
                ) |>
                ggplotly(tooltip = "text") |>
                layout(
                    showlegend = TRUE,
                    legend = list(
                        orientation = "h",
                        itemclick = "toggleothers",
                        itemsizing = "constant",
                        itemwidth = 30,
                        valign = "middle",
                        xanchor = "center",
                        x = 100,
                        y = 0.1,
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
                        text = HTML(self$CorrelationAnalytePlotTitle),
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
                        text = glue("{self$QueryAnalyteLabel} log<sub>2</sub>({self$QueryMeasurement})"),
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
                        fixedrange = TRUE
                    ),
                    yaxis = list(
                        title = list(
                        text = glue("{self$ComparisonAnalyteLabel} log<sub>2</sub>({self$ComparisonMeasurement})"),
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
                        fixedrange = TRUE
                    ),
                    annotations = list(
                        list(
                            x = 0.5,
                            y = 1.025,
                            text = glue("{self$AnalytePlotStatAnnotation}"),
                            xref = "paper",
                            yref = "paper",
                            axref = "x",
                            ayref = "y",
                            ax = 0,
                            ay = 0,
                            font = list(
                                family = "Arial",
                                color = "rgb(58, 62, 65)",
                                size = 12
                            )
                        )
                    ),
                    margin = list(t = 75)
                ) |>
                config(
                    displayModeBar = TRUE,
                    displaylogo = FALSE,
                    toImageButtonOptions = list(
                        format = "svg",
                        filename = glue('{self$applicationName} - Analyte Plot {format(Sys.time(),"%Y%m%d_%H%M%S") }'),
                        width = NULL,
                        height = NULL
                    ),
                    modeBarButtons = list(
                        list("toImage")
                    )
                ) |>
                onRender(
                    'function(el) {
                        el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
                    }'
                )
            }
    )
)
