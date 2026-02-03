box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, mutate, group_by, summarise, ungroup, rename_with, distinct, n,
        filter, pull, arrange, dense_rank, row_number],
    purrr[pmap],
    stringr[str_split_1],
    rlang[sym],
    plotly[layout, config],
    htmlwidgets[onRender],
    shinyjs[runjs],
)

box::use(
    app/logic/shared/statistical_analysis[formatPValue, addGroupCount],
    app/logic/shared/summary_plots[getVolcanoPlot, getVolcanoAnnotations, addSignificanceGroup,
        getCorrelationVolcanoAnnotations]
)

#' @export
VolcanoPlotStrategy <- R6Class(
    "VolcanoPlotStrategy",
    private = list(
        app_config = NULL,
        analysis_config = NULL,
        remote_db = NULL
    ),
    active = list(
        adjusted = function(value) {
            if (missing(value)) {
                return(self$adjustment_method != "none")
            }
        },
        analysis_variable_label = function(value) {
            return(
                private$analysis_config$AnalysisVariableName
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
                return("log<sub>2</sub>(Fold Change)")
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
                                minFC = round(min(FoldChange), 4),
                                maxFC = round(max(FoldChange), 4),
                                minP = min(p.value),
                                maxP = max(p.value)
                            ) |>
                            mutate(
                                text = glue(
                                    "<center>{count} points selected. Min Fold Change: \\
                                    {minFC}, Max Fold Change: {maxFC}</center>"
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
        study = NULL,
        study_data = NULL,
        stat_test = NULL,
        covariates = NULL,
        adjustment_method = NULL,
        fold_change_var = "log2FoldChange",
        significance_var = "-log10pvalue",
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
        },
        set_plot_event_data = function() {
            self$plot_event_data <- self$plot_data |>
                arrange(desc(significanceGroup)) |>
                select(
                    significanceGroup,
                    shape,
                    key = Analyte,
                    x = !!self$fold_change_var,
                    y = !!self$significance_var
                ) |>
                mutate(
                    group = glue("{significanceGroup}-{shape}"),
                    t = dense_rank(group),
                    curveNumber = t - 1
                ) |>
                group_by(group) |>
                mutate(
                    r = row_number(),
                    pointNumber = r - 1
                ) |>
                ungroup() |>
                filter(key == self$analyte) |>
                select(curveNumber, pointNumber, x, y, key)
            return(invisible(self$plot_event_data))
        },
        annotate_volcano_point = function(plot_name) {
            if (all(self$analyte != "")) {
                if (length(self$analyte) == 1) {
                    self$set_plot_event_data()

                    keys <- glue_collapse(self$analyte, sep = "|")

                    runjs(
                        glue(
                            'App.annotatePointByKey(
                                "{plot_name}",
                                {self$plot_event_data$curveNumber},
                                {self$plot_event_data$pointNumber},
                                "{keys}",
                                5
                            );'
                        )
                    )
                } else {
                    keys <- ""
                    runjs(
                        glue(
                            'App.annotatePointByKey(
                                "{plot_name}",
                                -1,
                                -1,
                                "{keys}",
                                5
                            );'
                        )
                    )
                    keys <- glue_collapse(self$analyte, sep = "|")
                    runjs(glue('App.updateSelectedKeys("{plot_name}","{keys}");'))
                }

                # runjs(
                #     paste0("
                #         Shiny.setInputValue(
                #         '", ns("analyteSearchResults"), "',
                #         {
                #             query: '", self$analyte, "',
                #             total: ", self$analyte, "
                #         },
                #         { priority: 'event' }
                #         );"
                #     )
                # )

            } else {
                keys <- ""
                runjs(glue('App.annotatePointByKey("{plot_name}","{keys}",5);'))
            }

        }
    )
)

#' @export
CorrelatesVolcanoPlotStrategy <- R6Class(
    "CorrelatesVolcanoPlotStrategy",
    inherit = VolcanoPlotStrategy,
    private = list(),
    active = list(
        query_analyte_label = function(value) {
            return(
                self$plot_data |>
                    distinct(QueryAnalyte) |>
                    pull()
            )
        }
    ),
    public = list(
        fold_change_var = "CorrelationValue",
        initialize = function(analysis_config, app_config, study, study_data,
            stat_test, covariates, adjustment_method) {
                super$initialize(
                    analysis_config, app_config, study, study_data,
                    stat_test, covariates, adjustment_method
                )
        },
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
