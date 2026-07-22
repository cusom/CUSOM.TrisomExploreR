box::use(
    R6[R6Class],
    glue[glue],
    dplyr[select, mutate, distinct, n, filter, pull, summarise, case_when],
    rlang[sym],
    plotly[layout, config],
    htmlwidgets[onRender],
)

box::use(
    app/logic/shared/summary_plots[getVolcanoPlot, getVolcanoAnnotations, addSignificanceGroup,
        getCorrelationVolcanoAnnotations],
    app/logic/shared/column_detection_utils[resolve_column_name]
)

#' @export
VolcanoPlotStrategy <- R6Class(
    "VolcanoPlotStrategy",
    private = list(
        app_config = NULL,
        analysis_config = NULL,
        remote_db = NULL,
        fold_change_var_override = NULL,
        detect_fold_change_var = function(data, preferred = NULL) {
            return(
                resolve_column_name(
                    data = data,
                    preferred = preferred,
                    exact_candidates = c("log2FoldChange", "FoldChange", "CorrelationValue", "Correlation", "Mean_difference"),
                    pattern_candidates = c(
                        "(^|_)log2.*fold.?change($|_)",
                        "(^|_)fold.?change($|_)",
                        "(^|_)correlation(value)?($|_)",
                        "(^|_)mean[_\\.]?diff($|_)"
                    ),
                    output_name = "fold_change_var",
                    data_label = "plot-data"
                )
            )
        },
        detect_significance_var = function(data, preferred = NULL) {
            return(
                resolve_column_name(
                    data = data,
                    preferred = preferred,
                    exact_candidates = c("-log10pvalue", "rho", "p.value", "pvalue", "qvalue", "FDR", "padj"),
                    pattern_candidates = c(
                        "(^|_)-?log10.*(p|q)\\.?value($|_)",
                        "(^|_)(adj|adjusted)?_?p\\.?value($|_)",
                        "(^|_)q\\.?value($|_)",
                        "(^|_)(fdr|padj)($|_)",
                        "(^|_)rho($|_)"
                    ),
                    output_name = "significance_var",
                    data_label = "plot-data"
                )
            )
        }
    ),
    active = list(
        fold_change_var = function(value) {
            if (missing(value)) {
                return(
                    private$detect_fold_change_var(
                        data = self$plot_data,
                        preferred = private$fold_change_var_override
                    )
                )
            }
        },
        significance_var = function(value) {
            if (missing(value)) {
                return(
                    private$detect_significance_var(
                        data = self$plot_data
                    )
                )
            }
        },
        adjusted = function(value) {
            if (missing(value)) {
                return(self$adjustment_method != "none")
            }
        },
        analysis_variable_label = function(value) {
            return(
                private$analysis_config$AnalysisVariableLabel
            )
        },
        volcanoPlotExpectedTraceCount = function(value) {
            if (missing(value)) {
                return(
                    self$plot_data |>
                        distinct(significanceGroup, shape) |>
                        nrow()
                    )
            }
        },
        VolcanoPlotTitle = function(value) {
            if (missing(value)) {
                return(
                    glue("Effect of {self$analysis_variable_label} on all {self$analytes_label}")
                )
            }
        },
        VolcanoSummaryMaxFoldChange = function(value) {
            if (missing(value)) {
                return(max(abs(self$plot_data$log2FoldChange)))
            }
        },
        VolcanoSummaryDataXAxisLabel = function(value) {
            if (missing(value)) {
              return(
                case_when(
                  grepl("log2", self$fold_change_var) ~ gsub("log2", "log<sub>2</sub>", self$fold_change_var),
                  TRUE ~ self$fold_change_var
                )
              )
            }
        },
        VolcanoSummaryDataYAxisLabel = function(value) {
            if (missing(value)) {
                return(
                    glue("-log<sub>10</sub>({ifelse(self$adjusted,\"q-value \",\"p-value \")})")
                )
            }
        },
        volcanoTopAnnotationLabel = function(value) {
            return(private$analysis_config$AnalysisVolcanoPlotTopAnnotation)
        }
    ),
    public = list(
        study = NULL,
        study_data = NULL,
        stat_test = NULL,
        covariates = NULL,
        adjustment_method = NULL,
        analytes_label = "Analytes",
        plot_data = NULL,
        analyte = NULL,
        plot_event_data = NULL,
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            private$remote_db <- app_config$remote_db
            self$study <- study
            self$study_data <- study_data
            self$stat_test <- stat_test
            self$covariates <- covariates
            self$adjustment_method <- adjustment_method
        },
        set_plot_data = function(plot_data) {
            self$plot_data <- plot_data
        },
        get_volcano_annotations = function() {
            return(
                self$plot_data |>
                    getVolcanoAnnotations(
                        foldChangeVar = !!sym(self$fold_change_var),
                        significanceVariable = !!sym(self$significance_var),
                        selected = selectedPoint,
                        arrowLabelTextVar = self$analyte,
                        upRegulatedText = self$volcanoTopAnnotationLabel,
                        includeThresholdLabel = FALSE
                    )
            )
        },
        add_significance_group = function(plot_data, annotations) {
            self$plot_data <- plot_data |>
                addSignificanceGroup(
                    foldChangeVar = !!sym(self$fold_change_var),
                    significanceVariable = !!sym(self$significance_var),
                    adjustedInd = annotations$parameters$adjustedInd,
                    significanceThreshold = annotations$parameters$significanceThresholdTransformed,
                    originalSignificanceThreshold = annotations$parameters$significanceThreshold
                )
            return(invisible(self$plot_data))
        },
        render = function(plot_data) {
            self$set_plot_data(plot_data)
            a <- self$get_volcano_annotations()
            self$add_significance_group(self$plot_data, a) |>
                getVolcanoPlot(
                    foldChangeVariable = !!sym(self$fold_change_var),
                    significanceVariable = !!sym(self$significance_var),
                    significanceGroup = significanceGroup,
                    text = text,
                    key = Analyte,
                    color = color,
                    shape = shape,
                    plotName = ""
                ) |>
                layout(
                    showlegend = TRUE,
                    legend = list(
                        orientation = "h",
                        itemclick = "toggleothers",
                        itemsizing = "constant",
                        valign = "middle",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.12
                    ),
                    title = list(
                        text = self$VolcanoPlotTitle
                    ),
                    xaxis = list(
                        title = list(
                        text = self$VolcanoSummaryDataXAxisLabel
                        )
                    ),
                    yaxis = list(
                        title = list(
                        text = self$VolcanoSummaryDataYAxisLabel
                        )
                    ),
                    annotations = c(a$annotations, a$arrow),
                    margin = list(t = 75)
                ) |>
                config(
                    displayModeBar = TRUE,
                    displaylogo = FALSE,
                    toImageButtonOptions = list(
                        format = "svg",
                        filename = glue(
                            "{self$applicationName} - {self$study} Volcano Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
                        ),
                        width = NULL,
                        height = NULL
                    ),
                    modeBarButtons = list(
                        list("select2d"),
                        list("lasso2d"),
                        list("zoom2d"),
                        list("zoomIn2d"),
                        list("zoomOut2d"),
                        list("resetScale2d"),
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
CorrelatesVolcanoPlotStrategy <- R6Class(
    "CorrelatesVolcanoPlotStrategy",
    inherit = VolcanoPlotStrategy,
    active = list(
        analysis_variable_label = function(value) {
            return(
                self$plot_data |>
                    distinct(QueryAnalyte) |>
                    pull()
            )
        },
        VolcanoSummaryDataXAxisLabel = function(value) {
            if (missing(value)) {
                return(self$significance_var_label)
            }
        },
        volcanoMultiSelectText = function(value) {
            if (missing(value)) {
                if (length(self$analyte) == 1) {
                    return("")
                } else {
                    return(
                        self$plot_data |>
                            filter(Analyte %in% self$analyte) |>
                            summarise(
                                count = n(),
                                minFC = round(min(!!sym(self$fold_change_var)), 4),
                                maxFC = round(max(!!sym(self$fold_change_var)), 4),
                                minP = min(p.value),
                                maxP = max(p.value)
                            ) |>
                            mutate(
                                text = glue(
                                    "<center>{count} points selected. Min {self$significance_var_label}: \\
                                    {minFC}, Max {self$significance_var_label} {maxFC}</center>"
                                )
                            ) |>
                            select(text) |>
                            pull()
                    )
                }
            }
        }
    ),
    public = list(
        get_volcano_annotations = function() {
            return(
                self$plot_data |>
                    getCorrelationVolcanoAnnotations(
                        foldChangeVar = !!sym(self$fold_change_var),
                        significanceVariable = !!sym(self$significance_var),
                        selected = selectedPoint,
                        arrowLabelTextVar = Analyte,
                        titleText = glue("Correlation with {self$query_analyte_label}:"),
                        includeThresholdLabel = FALSE
                    )
            )
        }
    )
)
