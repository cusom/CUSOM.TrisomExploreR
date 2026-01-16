box::use(
  app/logic/statistics/statistical_analysis[getStatTestByKeyGroup, getLinearModelWithInteraction, formatPValue, addGroupCount],
  app/logic/plots/feature_plots[getBoxPlotWithHighlightGroup, getScatterPlotByGroup],
)

#' @export
FeatureAnalysis_FeatureDataManager <- R6::R6Class(
  "FeatureAnalysis_FeatureDataManager",
  private = list(),
  active = list(
    AnalytePlotMethod = function(value) {
      if (missing(value)) {
        if (length(self$Analyte()) > 1) {
          return("heatmap")
        } else if (self$analysisType == "Categorical") {
          return("boxplot")
        } else if (self$analysisType == "Continuous") {
          return("scatterplot")
        } else {
          return("unknown")
        }
      }
    },
    GroupVariableCount = function(value) {
      if (missing(value)) {
        return(length(stringr::str_split(self$analysisVariableLabel, pattern = ";", simplify = TRUE)))
      }
    },
    AnalytePlotTitle =  function(value) {
      if (missing(value)) {
        if (self$AnalytePlotMethod == "boxplot") {
          return(glue::glue("Effect of {self$analysisVariableLabel} on {self$Analyte()}"))
        } else if (self$AnalytePlotMethod == "scatterplot" && self$GroupVariableCount == 1) {
          return(glue::glue("Effect of {self$analysisVariableLabel} in {group_variable} on {self$Analyte()}"))
        } else if (self$AnalytePlotMethod == "scatterplot" && self$GroupVariableCount > 1) {
          glue::glue("Comparison of {self$analysisVariableLabel} trajectories between karyotype for {self$Analyte()}")
        }
      }
    },
    AnalytePlotXAxisLabel = function(value) {  #function(namespace, analysis_variable) {self$namespace, self$analysisVariableLabel
      if (missing(value)) {
        if (self$namespace == "Comorbidity") {
          return(
            glue::glue("Has Any {self$analysisVariableLabel}")
          )
        } else {
          return(
            self$analysisVariableLabel
          )
        }
      }
    },
    AnalytePlotStatAnnotation = function(value) {
      if (missing(value)) {
        if (length(self$Analyte()) == 1) {
          return(
            self$SummaryData() |>
              dplyr::filter(Analyte == self$Analyte()) |>
              dplyr::ungroup() |>
              dplyr::select(p.value, p.value.adjustment.method) |>
              dplyr::mutate(
                formatted.p.value = unlist(
                  purrr::pmap(
                    .l = list(p.value, p.value.adjustment.method),
                    formatPValue
                  )
                )
              ) |>
              dplyr::select(formatted.p.value)
          )
        }
      }
    },
    formattedGroupBaselineLabel = function(value) {
      if (missing(value)) {
        return(
          self$AnalyteData |>
              dplyr::select(!!rlang::sym(self$analysisVariable)) |>
              dplyr::distinct() |>
              dplyr::filter(grepl(self$groupBaselineLabel, !!rlang::sym(self$analysisVariable))) |>
              dplyr::pull()
          #stringr::str_split(gsub("<.*?>", "", self$groupBaselineLabel), " ", simplify = TRUE)[1]
        )
      }
    }
  ),
  public = list(
    applicationName = NULL,
    namespace = NULL,


    input_config = NULL,
    analysisVariable = "",
    analysisVariableLabel = "",
    analysisType = "",
    experimentIDs = "",
    analytesLabel = "Analytes",
    groupBaselineLabel = "",
    FoldChangeVar = "log2FoldChange",
    SignificanceVariable = "-log10pvalue",

    Study = NULL,
    Platform = NULL,
    CellType = NULL,
    Karyotype = NULL,
    Conditions = NULL,
    Sex = NULL,
    Age = NULL,
    FilterLowCount = NULL,
    StatTest = NULL,
    Covariates = NULL,
    AdjustmentMethod = NULL,
    Adjusted = FALSE,
    SignificanceLabel = "p-value",

    StudyData = NULL,
    SummaryData = NULL,
    

    Analyte = "",
    AnalyteSearchName = "",
    AnalyteData = NULL,
    #AnalytePlotMethod = "boxplot",
    #AnalytePlotTitle = "",

    #AnalytePlotXAxisLabel = "",

    HeatmapData = NULL,


    #' @description
    #' Create a new instance of a FeatureAnalysisManager
    #' @param applicationName string - name of application
    #' @param id string - namespace for this instance
    #' @param namespace_config list - configurations for this namespace

    #' @param localDB R6 class - query manager for local database queries
    initialize = function(analysis_config, study_data, feature, summary_data) {
  
      #self$input_config <- input_config()
      self$StudyData <- study_data
      self$Analyte <- feature
      self$SummaryData <- summary_data
    
      namespace_config <- analysis_config #|>
        #dplyr::filter(tolower(Namespace) == tolower(id))

      self$applicationName <- namespace_config$ApplicationName

      self$namespace <- namespace_config$Namespace
      self$analysisVariable <- namespace_config$AnalysisVariableName
      self$analysisVariableLabel <- namespace_config$AnalysisVariableLabel
      self$analysisType <- namespace_config$AnalysisType
  
      self$experimentIDs <- stringr::str_split_1(namespace_config$ExperimentIDs, "\\|")
      self$groupBaselineLabel <- namespace_config$AnalysisVariableBaselineLabel

    },

    #' @description
    #' get sample level data for selected analyte(s)
    getAnalyteData = function() {
      
      if (length(self$Analyte()) == 1) {

        self$AnalyteData <- self$StudyData() |>
          dplyr::filter(
            Analyte %in% self$Analyte(),
            log2MeasuredValue != Inf,
            log2MeasuredValue != -Inf
          ) |>
          dplyr::mutate(
            log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
            log2Measurement = glue::glue("log<sub>2</sub>({Measurement})"),
            highlightGroup = dplyr::case_when(
              1 == 1 ~ NA
            )
          )

        if (self$AnalytePlotMethod == "boxplot") {

          self$AnalyteData <- self$AnalyteData |>
            dplyr::rowwise() |>
            addGroupCount(group = !!rlang::sym(self$analysisVariable), addLineBreak = FALSE) |>
            dplyr::select(-n) |>
            dplyr::ungroup() |>
            dplyr::mutate(text = glue::glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}"))

        }

      } else {


        self$AnalyteData <- self$SummaryData() |>
          dplyr::filter(Analyte %in% self$Analyte()) |>
          dplyr::select(Analyte, log2FoldChange, text) |>
          dplyr::arrange(-log2FoldChange) |>
          dplyr::mutate(Analyte = forcats::fct_inorder(Analyte), "Analysis" = "T21vD21")

      }

      return(invisible(self$AnalyteData))
    },

    #' @description
    #' Get analyte plot
    #' @param .data tibble - data for analyte plot
    #' @param ns - namespace to apply to plot object
    getAnalytePlot = function(.data, ns) {

      if (self$AnalytePlotMethod == "boxplot") {
  
        p <- .data |>
          getBoxPlotWithHighlightGroup(
            key = LabID,
            group = !!rlang::sym(self$analysisVariable),
            groupBaselineLabel = self$formattedGroupBaselineLabel,
            value = log2MeasuredValue,
            valueLabel = log2Measurement,
            text = text,
            highlightGroup = highlightGroup,
            plotName = glue::glue("{self$namespace}Analyte")
          ) |>
          plotly::layout(
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
                text = glue::glue("<b>{self$AnalytePlotXAxisLabel}</b>"),
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
                text = glue::glue("{self$AnalytePlotStatAnnotation}"),
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
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue::glue(
                "{self$applicationName} - {self$Analyte()} Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
              ),
              width = NULL,
              height = NULL
            ),
            modeBarButtons = list(
              ## list(plotlyCustomIcons$AnalytePlotTutorial),
              list("select2d"),
              list("lasso2d"),
              list("toImage")
              # list(appConfig$plotlyCustomIcons$BoxplotCompareGroup),
              # list(appConfig$plotlyCustomIcons$BoxplotClear)
            )
          ) |>
          htmlwidgets::onRender(
          'function(el) {
              el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
            }'
          )

        p$x$source <- ns("BoxPlot")

        return(p)

      }

      if (self$AnalytePlotMethod == "scatterplot") {
        
        p <- .data |>
          dplyr::mutate(
            text = glue::glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
          ) |>
          getScatterPlotByGroup(
            key = LabID,
            x = !!rlang::sym(self$analysisVariable),
            y = log2MeasuredValue,
            group = Karyotype,
            groupBaselineLabel = "Control",
            text = text,
            addFitLines = TRUE,
            plotName = "ScatterPlot"
          ) |>
          plotly::layout(
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
                text = glue::glue("{.data$log2Measurement[1]}")
              )
            ),
            annotations = list(
              list(
                x = 0.5,
                y = 1.025,
                text = glue::glue("{self$AnalytePlotStatAnnotation}"),
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
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue::glue(
                "{self$applicationName} - {self$Analyte()} Analyte Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
              ),
              width = NULL,
              height = NULL
            ),
            modeBarButtons = list(
              #list(plotlyCustomIcons$AnalytePlotTutorial),
              list("toImage")
            )
          ) |>
          htmlwidgets::onRender(
          'function(el) {
              el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
            }'
          )

        p$x$source <- ns("ScatterPlot")

        return(p)

      }

      if (self$AnalytePlotMethod == "heatmap") {
     
        limit <- .data |>
          dplyr::pull(log2FoldChange) |>
          abs() |>
          max() |>
          plyr::round_any(0.01, f = ceiling)

        long_data <- .data |>
          dplyr::select(name = Analyte, variable = Analysis, value = log2FoldChange)

        self$HeatmapData <- long_data |>
          dplyr::select("Analyte" = name, z = value) |>
          dplyr::arrange(z) |>
          dplyr::mutate(r = dplyr::row_number())

        p <- heatmaply::heatmaply(
          long_data = long_data,
          dendrogram = "none",
          xlab = "",
          ylab = "",
          key = ~ name,
          showticklabels = c(FALSE, TRUE),
          main = shiny::HTML(glue::glue("Fold Change with {self$analysisVariableLabel}")),
          margins = c(60, 100, 40, 20),
          subplot_widths = 0.65,
          yaxis_width = 10,
          grid_color = "white",
          grid_width = 0.001,
          titleX = TRUE,
          limits = c(-limit, limit),
          col = RColorBrewer::brewer.pal(11, "RdBu") |> rev(),
          scale_fill_gradient_fun = circlize::colorRamp2(
            seq(-limit, limit, length.out = 11),
            RColorBrewer::brewer.pal(11, "RdBu") |> rev()
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
            .data$text
          )
        ) |>
          plotly::colorbar(
            tick0 = -limit,
            dtick = limit
          ) |>
          plotly::layout(
            title = list(
              text = shiny::HTML(glue::glue("Fold Change with {self$analysisVariableLabel}")),
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
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue::glue("{self$applicationName} - Heatmap {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
              width = NULL,
              height = NULL
            ),
            modeBarButtons = list(
              list("toImage")
            )
          ) |>
          htmlwidgets::onRender('
            function(el) {
              el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
            }'
          )

        p$x$source <- ns("HeatmapPlot")

        return(p)

      }

    },

    #' @description
    #' Get formatted analyte plot data for data table
    #' @param .data - analyte sample level data
    getFormattedAnalyteSummaryData =  function(.data) {

      if (self$AnalytePlotMethod == "boxplot") {

        dataframe <- self$AnalyteData

        measurement <- as.character(dataframe[1, "Measurement"])

        return(
          dataframe |>
            dplyr::mutate("Study" = self$getStudyLabel()) |>
            dplyr::select(Study, Analyte, LabID, Karyotype, Sex, MeasuredValue) |>
            dplyr::rename(`:=`(!!measurement, MeasuredValue)) |>
            dplyr::arrange(Analyte)
        )

      } else {

        adjusted <- self$AdjustmentMethod != "none"
        p_val_label <- ifelse(adjusted, "q-value", "p-value")
        log_10_p_val_label <- ifelse(adjusted, "-log<sub>10</sub>(q-value)", "-log<sub>10</sub>(p-value)")

        old_names <- c("FoldChange", "p.value.original", "p.value.adjustment.method",
          "log2FoldChange", "p.value", "-log10pvalue", "lmFormula"
        )
        new_names <- c("Fold Change", "p-value (original)", "adjustment method",
          "log<sub>2</sub>(Fold Change)", p_val_label, log_10_p_val_label, "Model"
        )

        return(
          self$AnalyteData |>
            dplyr::select(Analyte) |>
            dplyr::inner_join(self$VolcanoSummaryData, by = "Analyte") |>
            dplyr::rename_with(~ new_names, all_of(old_names)) |>
            dplyr::select(-c(formattedPValue, text, ivs))
        )

      }

    }

  )
)
