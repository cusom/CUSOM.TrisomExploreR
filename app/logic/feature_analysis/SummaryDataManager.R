

box::use(
  app/logic/statistics/statistical_analysis[formatPValue, addGroupCount],
  app/logic/plots/volcano_plot_helpers[getVolcanoPlot, getVolcanoAnnotations, addSignificanceGroup]
)

box::use(
  R6[R6Class],
  glue[glue, glue_collapse],
  tibble[tibble],
  dplyr[select, mutate, group_by, summarise, ungroup, rename_with, distinct, n,
        pull, arrange, dense_rank, row_number],
  purrr[pmap],
  stringr[str_split_1],
  rlang[sym],
  plotly[layout, config],
  htmlwidgets[onRender],
  shinyjs[runjs]
)
#' @export
FeatureAnalysis_SummaryDataManager <- R6Class(
  "FeatureAnalysis_SummaryDataManager",
  private = list(),
  active = list(
    Adjusted = function(value) {
      if (missing(value)) {
        return(self$AdjustmentMethod() != "none")
      }
    },
    VolcanoPlotTitle = function(value) {
      if (missing(value)) {
        return(glue("Effect of {self$analysisVariableLabel} on all {self$analytesLabel}"))
      }
    },
    VolcanoSummaryMaxFoldChange = function(value) {
      if (missing(value)) {
        return(max(abs(self$VolcanoSummaryData$log2FoldChange)))
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
          glue("-log<sub>10</sub>({ifelse(self$Adjusted,\"q-value \",\"p-value \")})")
        )
      }
    },
    volcanoMultiSelectText = function(value) {
      if (missing(value)) {
        if (length(self$Analyte) == 1) {
          return("")
        } else {
          return(
            self$VolcanoSummaryData |>
              dplyr::filter(Analyte %in% self$Analyte) |>
              summarise(
                count = n(),
                minFC = round(min(FoldChange), 4),
                maxFC = round(max(FoldChange), 4),
                minP = min(p.value),
                maxP = max(p.value)
              ) |>
              mutate(
                text = glue(
                  "<center>{count} points selected. Min Fold Change: {minFC}, Max Fold Change: {maxFC}</center>"
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
    StatisticalParameters = NULL,
    Covariates = NULL,
    AdjustmentMethod = NULL,

    SignificanceLabel = "p-value",

    StudyData = NULL,

    VolcanoSummaryData = NULL,

    volcanoTopAnnotationLabel = "",
    volcanoPlotExpectedTraceCount = 3,
    volcanoSourceData = NULL,
    volcanoEventData = tibble(
      curveNumber = -1,
      pointNumber = -1,
      x = -1,
      y = -1,
      key = ""
    ),
    VolcanoSummaryDataFoldChangeFilter = NULL,
    #volcanoMultiSelectText = "",

    Analyte = "",
    initialize = function(analysis_config, StatTest, Covariates, AdjustmentMethod) {
  
      namespace_config <- analysis_config
    
      self$applicationName <- namespace_config$ApplicationName
    
      self$StatTest <- StatTest
      self$Covariates <- Covariates
      self$AdjustmentMethod <- AdjustmentMethod

      self$namespace <- namespace_config$Namespace
      self$analysisVariable <- namespace_config$AnalysisVariableName
      self$analysisVariableLabel <- namespace_config$AnalysisVariableLabel
      self$analysisType <- namespace_config$AnalysisType
  
      self$experimentIDs <- str_split_1(namespace_config$ExperimentIDs, "\\|")
      self$groupBaselineLabel <- namespace_config$AnalysisVariableBaselineLabel
      self$volcanoTopAnnotationLabel <- namespace_config$AnalysisVolcanoPlotTopAnnotation

    },

    setStudyData = function(.data) {
      self$StudyData <- .data
      return(invisible(self$StudyData))
    },

    #' @description
    #' Set Volcano Summary Data along with other volcano plot properties
    #'
    setVolcanoSummaryData = function(.data) {

      if (is.null(.data)) {
        .data <- self$StudyData
      }

      return(self$VolcanoSummaryData)

    },

    getVolcanoSummaryData = function(.data) {
      if (is.null(.data)) {
        .data <- self$StudyData
      }
      self$setVolcanoSummaryData(.data) |>
        mutate(
          formattedPValue = unlist(
            pmap(
              .l = list(p.value, p.value.adjustment.method),
              formatPValue
            )
          ),
          text = glue(
            "Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}"
          )
        ) |>
        ungroup() |>
        mutate(
          log2FoldChange = log2(FoldChange),
          `-log10pvalue` = -log10(p.value),
          `p.value.adjustment.method` = self$AdjustmentMethod(),
          formattedPValue = unlist(
            pmap(
              .l = list(p.value, p.value.adjustment.method),
              formatPValue
            )
          ),
          text = glue("Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}")
        )
    },

    #' @description
    #' get user-friendly formatted VolcanoSummaryData
    #' @param .data tibble of volcano summary data to format
    getFormattedVolcanoSummaryData =  function(.data) {

      adjusted <- self$AdjustmentMethod != "none"
      p_val_label <- ifelse(adjusted, "q-value", "p-value")
      log_10_p_val_label <- ifelse(adjusted, "-log<sub>10</sub>(q-value)", "-log<sub>10</sub>(p-value)")

      old_names <- c("log2FoldChange", "p.value.adjustment.method", "p.value.original",
        "FoldChange", "p.value", "-log10pvalue", "lmFormula"
      )
      new_names <- c("log<sub>2</sub>(Fold Change)", "adjustment method", "p-value (original)",
        "Fold Change", p_val_label, log_10_p_val_label, "model"
      )

      return(
        .data |>
          rename_with(~ new_names, all_of(old_names)) |>
          select(-c(pvalueCutoff, formattedPValue, text, ivs))
      )

    },

    #' @description
    #' Get volcano plot
    #' @param .data tibble - data for volcano plot
    #' @param ns - namespace to apply to plot object
    #'
    getVolcanoPlot = function(.data, ns) {

      self$volcanoSourceData <- .data |>
        mutate(
          shape = "circle",
          selectedPoint = 0
        )

      a <- self$volcanoSourceData |>
        getVolcanoAnnotations(
          foldChangeVar = !!sym(self$FoldChangeVar),
          significanceVariable = !!sym(self$SignificanceVariable),
          selected = selectedPoint,
          arrowLabelTextVar = self$Analyte,
          upRegulatedText = self$volcanoTopAnnotationLabel,
          includeThresholdLabel = FALSE
        )

      self$volcanoSourceData <- self$volcanoSourceData |>
        addSignificanceGroup(
          foldChangeVar = !!sym(self$FoldChangeVar),
          significanceVariable = !!sym(self$SignificanceVariable),
          adjustedInd = a$parameters$adjustedInd,
          significanceThreshold = a$parameters$significanceThresholdTransformed,
          originalSignificanceThreshold = a$parameters$significanceThreshold
        )

      self$volcanoPlotExpectedTraceCount <- self$volcanoSourceData |>
        distinct(significanceGroup, shape) |>
        nrow()

      p <- self$volcanoSourceData |>
        getVolcanoPlot(
          foldChangeVariable = !!sym(self$FoldChangeVar),
          significanceVariable = !!sym(self$SignificanceVariable),
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
              "{self$applicationName} - {self$Study} Volcano Plot {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
            ),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            #list(plotlyCustomIcons$VolcanoPlotTutorial),
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

      p$x$source <- ns("VolcanoPlot")

      p

    },

    # #' @description
    # #' helper function to update various attributes for the chosen analyte
    # updateAnalyteAttributes = function() {
      

    #   if (length(self$Analyte) == 1) {
        
        

    #     self$volcanoMultiSelectText <- ""

    #   }

    #   if (length(self$Analyte) > 1) {
    #     self$volcanoMultiSelectText <- self$VolcanoSummaryData |>
    #       dplyr::filter(Analyte %in% self$Analyte) |>
    #       summarise(
    #         count = n(),
    #         minFC = round(min(FoldChange), 4),
    #         maxFC = round(max(FoldChange), 4),
    #         minP = min(p.value),
    #         maxP = max(p.value)
    #       ) |>
    #       mutate(
    #         text = glue(
    #           "<center>{count} points selected. Min Fold Change: {minFC}, Max Fold Change: {maxFC}</center>"
    #         )
    #       ) |>
    #       select(text) |>
    #       pull()
    #   }

    #   return(invisible(self))

    # },

    #' @description
    #' helper function to add annotation to volcano plot based on chosen analyte
    #' @param plot_name string - name of target volcano plot
    #' @param ns namespace to properly derive fully-qualified plot name
    annotate_volcano_point = function(plot_name, ns) {

      plot_name <- ns(plot_name)

      if (all(self$Analyte != "")) {
        if (length(self$Analyte) == 1) {
          self$volcanoEventData <- self$volcanoSourceData |>
            arrange(desc(significanceGroup)) |>
            select(
              significanceGroup,
              shape,
              key = Analyte,
              x = !!self$FoldChangeVar,
              y = !!self$SignificanceVariable
            ) |>
            mutate(
              group = glue("{significanceGroup}-{shape}"),
              t =dense_rank(group),
              curveNumber = t - 1
            ) |>
            group_by(group) |>
            mutate(
              r = row_number(),
              pointNumber = r - 1
            ) |>
            ungroup() |>
            dplyr::filter(key == self$Analyte) |>
            select(curveNumber, pointNumber, x, y, key)

          self$volcanoEventData <- self$volcanoEventData |>
            dplyr::filter(key == self$Analyte)

          keys <- glue_collapse(self$Analyte, sep = "|")

          runjs(
            glue(
              'App.annotatePointByKey(
                "{plot_name}",
                {self$volcanoEventData$curveNumber},
                {self$volcanoEventData$pointNumber},
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
          keys <- glue_collapse(self$Analyte, sep = "|")
          runjs(glue('App.updateSelectedKeys("{plot_name}","{keys}");'))
        }

        runjs(
          paste0("
            Shiny.setInputValue(
              '", ns("analyteSearchResults"), "',
              {
                query: '", self$Analyte, "',
                total: ", self$Analyte, "
              },
              { priority: 'event' }
            );"
          )
        )

      } else {
        keys <- ""

        runjs(glue('App.annotatePointByKey("{plot_name}","{keys}",5);'))
      }

    }

    # # #' @description
    # # #' small helper function to get correct value for volcanoMultiSelectText
    # getVolcanoMultiSelectText = function() {
    #   if (length(self$Analyte) > 1 & self$volcanoMultiSelectText == "") {
    #     self$updateAnalyteAttributes()
    #   } else if (length(self$Analyte) == 1)  {
    #     self$volcanoMultiSelectText <- ""
    #   }
    #   return(self$volcanoMultiSelectText)
    # }

  )
)
