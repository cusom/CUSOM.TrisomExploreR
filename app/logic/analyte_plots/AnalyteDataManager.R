box::use(
  dplyr[select, filter, mutate, distinct, pull, arrange,
    case_when, rowwise, ungroup, row_number],
  glue[glue],
  stringr[str_split],
  purrr[pmap],
  forcats[fct_inorder],
  plyr[round_any],
  plotly[colorbar, layout, config],
  htmlwidgets[onRender],
  rlang[sym],
  heatmaply[heatmaply],
  RColorBrewer[brewer.pal],
  circlize[colorRamp2],
)


box::use(
  app/logic/shared/statistical_analysis[getStatTestByKeyGroup, getLinearModelWithInteraction, formatPValue, addGroupCount],
  app/logic/analyte_plots/analyte_plots[getBoxPlotWithHighlightGroup, getScatterPlotByGroup],
)

#' @export
FeatureAnalysisAnalyteDataManager <- R6::R6Class(
  "FeatureAnalysisAnalyteDataManager",
  private = list(),
  active = list(
    AnalysisMode = function(value) {
      return(
        if (length(self$Analyte()) == 1) "single" else "multi"
      )
    },
    AnalytePlotMethod = function(value) {
      if (missing(value)) {
        stop("Abstract: must implement")
      }
    },
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
        self$AnalyteData |>
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
    AnalytePlotStatAnnotation = function(value) {
      if (missing(value)) {
        if (self$AnalysisMode == "single") {
          return(
            self$SummaryData() |>
              filter(Analyte == self$Analyte()) |>
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
      }
    },
    measurement_label = function(value) {
      return(
        as.character(
          self$AnalyteData[1, "Measurement"]
        )
      )
    },
    formattedGroupBaselineLabel = function(value) {
      if (missing(value)) {
        return(
          self$AnalyteData |>
              select(!!sym(self$analysisVariable)) |>
              distinct() |>
              filter(grepl(self$groupBaselineLabel, !!sym(self$analysisVariable))) |>
              pull()
        )
      }
    }
  ),
  public = list(
    applicationName = NULL,
    namespace = NULL,
    remote_db = NULL,
    analysisVariable = "",
    analysisVariableLabel = "",
    analysisType = "",
    experimentIDs = "",
    analytesLabel = "Analytes",
    groupBaselineLabel = "",
    FoldChangeVar = "log2FoldChange",
    SignificanceVariable = "-log10pvalue",

    FilterLowCount = NULL,
    StatTest = NULL,
    Covariates = NULL,
    AdjustmentMethod = NULL,
    Adjusted = FALSE,
    SignificanceLabel = "p-value",
    study = NULL,
    StudyData = NULL,
    SummaryData = NULL,
    Analyte = "",
    AnalyteSearchName = "",
    AnalyteData = NULL,
    HeatmapData = NULL,
    initialize = function(app_config, analysis_config, study, study_data, analyte, summary_data) {

      self$study <- study
      self$StudyData <- study_data
      self$Analyte <- analyte
      self$SummaryData <- summary_data

      self$remote_db <- app_config$remote_db
      namespace_config <- analysis_config
      self$applicationName <- namespace_config$ApplicationName

      self$namespace <- namespace_config$Namespace
      self$analysisVariable <- namespace_config$AnalysisVariableName
      self$analysisVariableLabel <- namespace_config$AnalysisVariableLabel
      self$analysisType <- namespace_config$AnalysisType

      self$experimentIDs <- stringr::str_split_1(namespace_config$ExperimentIDs, "\\|")
      self$groupBaselineLabel <- namespace_config$AnalysisVariableBaselineLabel

    },

    getAnalyteData = function() {

      if (self$AnalysisMode == "single") {
        return(self$set_single_analyte_data())
      } else {
        return(self$set_multi_analyte_data())
      }

    },

    set_single_analyte_data = function() {
      stop("Abstract: must implement")
    },
    set_multi_analyte_data = function() {
      self$AnalyteData <- self$SummaryData() |>
        filter(Analyte %in% self$Analyte()) |>
        select(Analyte, log2FoldChange, text) |>
        arrange(-log2FoldChange) |>
        mutate(Analyte = fct_inorder(Analyte), "Analysis" = "T21vD21")
      return(invisible(self$getAnalyteData))
    },

    getAnalytePlot = function(.data, ns) {
      if (self$AnalysisMode == "single") {
        self$plot_single(.data, ns)
      } else {
        self$plot_multi(.data, ns)
      }
    },

    plot_single = function(.data, ns) {
      stop("Abstract: must implement")
    },

    plot_multi = function(.data, ns) {

      limit <- .data |>
        pull(log2FoldChange) |>
        abs() |>
        max() |>
        round_any(0.01, f = ceiling)

      long_data <- .data |>
        select(name = Analyte, variable = Analysis, value = log2FoldChange)

      self$HeatmapData <- long_data |>
        select("Analyte" = name, z = value) |>
        arrange(z) |>
        mutate(r = row_number())

      p <- heatmaply(
        long_data = long_data,
        dendrogram = "none",
        xlab = "",
        ylab = "",
        key = ~ name,
        showticklabels = c(FALSE, TRUE),
        main = shiny::HTML(glue("Fold Change with {self$analysisVariableLabel}")),
        margins = c(60, 100, 40, 20),
        subplot_widths = 0.65,
        yaxis_width = 10,
        grid_color = "white",
        grid_width = 0.001,
        titleX = TRUE,
        limits = c(-limit, limit),
        col = brewer.pal(11, "RdBu") |> rev(),
        scale_fill_gradient_fun = colorRamp2(
          seq(-limit, limit, length.out = 11),
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
          .data$text
        )
      ) |>
        colorbar(
          tick0 = -limit,
          dtick = limit
        ) |>
        layout(
          title = list(
            text = shiny::HTML(glue("Fold Change with {self$analysisVariableLabel}")),
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

      p$x$source <- ns("HeatmapPlot")

      return(p)

    },

    get_table_data = function() {
      if (self$AnalysisMode == "single") {
        self$table_single()
      } else {
        self$table_multi()
      }
    },

    table_single = function() {
      return(
        self$AnalyteData |>
          dplyr::mutate("Study" = self$study()) |>
          dplyr::select(Study, Analyte, LabID, !!rlang::sym(self$analysisVariable), MeasuredValue) |>
          dplyr::rename(`:=`(!!self$measurement_label, MeasuredValue)) |>
          dplyr::arrange(Analyte)
      )
    },

    table_multi = function() {

      p_val_label <- ifelse(self$Adjusted, "q-value", "p-value")
      log_10_p_val_label <- ifelse(self$Adjusted, "-log<sub>10</sub>(q-value)", "-log<sub>10</sub>(p-value)")

      old_names <- c("FoldChange", "p.value.original", "p.value.adjustment.method",
                    "log2FoldChange", "p.value", "-log10pvalue", "lmFormula"
      )
      new_names <- c("Fold Change", "p-value (original)", "adjustment method",
                    "log<sub>2</sub>(Fold Change)", p_val_label, log_10_p_val_label, "Model"
      )
      return(
        self$AnalyteData |>
          dplyr::select(Analyte) |>
          dplyr::inner_join(self$SummaryData(), by = "Analyte") |>
          dplyr::rename_with(~ new_names, all_of(old_names)) |>
          dplyr::select(-c(formattedPValue, text, ivs))
      )
    }
  )
)
