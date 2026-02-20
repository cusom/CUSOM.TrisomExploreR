

box::use(
  app/logic/feature_analysis/summary/SummaryDataManagers[CorrelatesSummaryDataSource],
  app/logic/shared/statistical_analysis[formatPValue, addGroupCount],
  app/logic/shared/summary_plots[getVolcanoPlot, getCorrelationVolcanoAnnotations, addSignificanceGroup],
  app/logic/shared/string_utils[parse_delimited_string]
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
CorrelatesSummaryDataManager <- R6Class(
  "CorrelatesSummaryDataManager",
  inherit = CorrelatesSummaryDataSource,
  private = list(),
  active = list(
    Adjusted = function(value) {
      return(TRUE)
    },
    QueryAnalyteLabel = function(value) {
      return(
        self$CorrelationSourceData |>
          distinct(QueryAnalyte) |>
          pull()
      )
    },
    CompareExperiment = function(value) {
      return(
        self$CorrelationSourceData |>
          distinct(ComparisonExperimentID) |>
          pull()
      )
    },
    CorrelationMeasureName = function(value) {
      return(unique(self$CorrelationSourceData$CorrelationMeasureName))
    },
    VolcanoDataMaxFinite = function(value) {
      return(
        self$CorrelationSourceData |>
          dplyr::filter(p.value > 0) |>
          dplyr::pull(p.value) |>
          min() |>
          (\(x) {
            -log10(x)
          })()
      )
    },
    VolcanoPlotTitle = function(value) {
      return(
        glue::glue(
          "Correlation between \\
            {parse_delimited_string(self$QueryAnalyteLabel,1)} \\
            and {self$CompareExperiment}"
        )
      )
    },
    VolcanoSummaryMaxFoldChange = function(value) {
      return(max(abs(self$VolcanoSummaryData$CorrelationValue)))
    },
    VolcanoSummaryDataXAxisLabel = function(value) {
      return(self$CorrelationMeasureName)
    }
  ),
  public = list(
    CorrelationSourceData = NULL,
    Analyte = NULL,
    volcanoEventData = NULL,
    FoldChangeVar = "CorrelationValue",

    initialize = function(analysis_config) {

      super$initialize(
        analysis_config,
        StatTest = "spearman",
        Covariates = c(0),
        AdjustmentMethod = "BH"
      )

    },

    setStudyData = function(.data) {

      self$CorrelationSourceData <- .data

      return(self$CorrelationSourceData)
    },

    getVolcanoSummaryData = function(.data) {
      if (is.null(.data)) {
        self$setStudyData(.data)
      }

      self$VolcanoSummaryData <- self$CorrelationSourceData |>
        dplyr::mutate(
          shape = ifelse(p.value == 0, "triangle-up", "circle"),
          p.value = ifelse(p.value == 0, 10^-(self$VolcanoDataMaxFinite * 1.05), p.value),
          `-log10pvalue` = -log10(p.value)
        ) |>
        dplyr::group_by(Analyte) |>
        dplyr::mutate(rank = dplyr::row_number(-abs(CorrelationValue))) |>
        dplyr::filter(rank == 1) |>
        dplyr::select(-rank) |>
        dplyr::mutate(
          "p.value.adjustment.method" = self$AdjustmentMethod,
          formattedPValue = unlist(
            purrr::pmap(
              .l = list(p.value, p.value.adjustment.method),
              formatPValue
              )
            ),
          text = glue::glue(
            "Analyte: {Analyte} <br />{self$CorrelationMeasureName}:{round(CorrelationValue,2)} <br />{formattedPValue}"
            )
        ) |>
        dplyr::ungroup()

      return(invisible(self$VolcanoSummaryData))
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

    ## call specific function and use differnt text
    get_volcano_annotations = function(.data) {
      return(
        .data |>
          getCorrelationVolcanoAnnotations(
            foldChangeVar = !!sym(self$FoldChangeVar),
            significanceVariable = !!sym(self$SignificanceVariable),
            selected = selectedPoint,
            arrowLabelTextVar = Analyte,
            titleText = glue::glue("Correlation with {self$QueryAnalyteLabel}:"),
            includeThresholdLabel = FALSE
          )
      )
    }

  )
)
