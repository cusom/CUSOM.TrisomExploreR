#' R6 Class to manage Condition Correlates Analysis
#' @description
#' Enables analysis of co-occuring conditions by chosen feature
#'
#' @field applicationName - string - name of application
#' @field namespace - string - namespace for this instance
#' @field remoteDB - R6 class to manage remote database queries
#' @field localDB - R6 class to manage local database queries
#' @field analysisVariable - string - target feature for analysis in this instance-namespace
#' @field analysisVariableLabel - string - label for analysis variable
#' @field FoldChangeVar - string - name of variable indicating fold change or difference (log2FoldChange)
#' @field SignificanceVariable - string - name of variable indiciating significance value (p-value)
#' @field Study - string - selected study
#' @field Platform - string vector - Platform values chosen for analysis
#' @field Experiment - string - chosen experimentID
#' @field QueryAnalyte - string - chosen Query Analyte
#' @field Analyte - string - chosen Comparison Analyte
#' @field MinComorbitityMembership - numeric - minimum number of records with matching condition(s) to include in analysis
#' @field Sex - string vector - Sex values chosen for analysis
#' @field Age - numeric vector - Age values chosen fo analysis
#' @field StatTest - string - name of statistical test to apply for analysis (Linear Model, etc.)
#' @field Covariates - string vector - names of features to include as covariates in Linear Model analysis
#' @field AdjustmentMethod - string - name of multiple hypothesis correction method to apply to statistical output
#' @field AnalytePlotStatAnnotation - string - stat annotation to show above analyte plot
#' @field AnalyteData - tibble - sample level data for chosen analyte(s)
#' @field ParticipantComorbidities - tibble - Participant Comorbidity data
#' @field ComorbiditySummary - tibble -
#' @field Comorbidities - character vector - chosen comborbidities
#' @field VolcanoSummaryData - tibble - Fold Change summary data used for volcano plot
#' @field VolcanoSummaryDataXAxisLabel - string - volcano plot x-axis
#' @field VolcanoSummaryDataYAxisLabel - string - volcano plot y-axis
#' @field VolcanoSummaryMaxFoldChange - numeric - maxiumum abs. value of fold change
#' @field VolcanoPlotTitle - string - title to show above volcano plot
#' @field volcanoTopAnnotationLabel - string - text to show above top annotations on volcano plot
#' @field volcanoPlotExpectedTraceCount - numeric - number of base traces present in the active volcano plot (usually between 1 - 3)
#' @field VolcanoSummaryDataFoldChangeFilter - depreciated?
#' @field volcanoMultiSelectText - string - text to show below volcano plot when multiple analytes are chosen
#' @field HeatmapData - tibble - data to use for heatmap plot when multiple analytes are chosen
#' @field BoxplotData - tibble - data to use for boxplot when a single anlayte is chosen
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import glue
#' @import forcats
#' @import plotly
#' @import htmlwidgets
#' @importFrom heatmaply heatmaply
#' @importFrom RColorBrewer brewer.pal
#' @importFrom circlize colorRamp2
#' @importFrom arrow open_dataset
#' @export
ConditionCorrelatesManager <- R6::R6Class(
  "ConditionCorrelatesManager",
  private = list(),
  public = list(
    applicationName = NULL,
    namespace = NULL,
    remoteDB = NULL,
    localDB = NULL,

    analysisVariable = "HasConditionFlag",
    analysisVariableLabel = "History of Co-Occuruing Condition(s)",
    FoldChangeVar = "log2FoldChange",
    SignificanceVariable = "-log10pvalue",

    Study = "NA",
    Platform = "",
    Experiment = "",
    QueryAnalyte = "",
    Analyte = NULL,
    MinComorbitityMembership = -1,
    Sex = NULL,
    Age = NULL,
    StatTest = NULL,
    Covariates = NULL,
    AdjustmentMethod = NULL,
    AnalytePlotStatAnnotation = NULL,
    AnalyteData = NULL,
    ParticipantComorbidities = NULL,
    ComorbiditySummary = NULL,
    Comorbidities = NULL,

    VolcanoSummaryData = NULL,
    VolcanoSummaryDataXAxisLabel = "",
    VolcanoSummaryDataYAxisLabel = "",
    VolcanoSummaryMaxFoldChange = 0,
    VolcanoPlotTitle = "",
    volcanoTopAnnotationLabel = "",
    volcanoPlotExpectedTraceCount = 3,
    volcanoSourceData = NULL,
    volcanoEventData = tibble::tibble(
      curveNumber = -1,
      pointNumber = -1,
      x = -1,
      y = -1,
      key = ""
    ),
    VolcanoSummaryDataFoldChangeFilter = NULL,
    volcanoMultiSelectText = "",

    HeatmapData = NULL,
    BoxplotData = NULL,

    #' @description
    #' Create a new instance of ConditionCorrelatesManager object
    #' @param applicationName string - applicationName
    #' @param id string - namespace for this class
    #' @param namespace_config tibble - configuration values for this namespace instance of the object
    #' @param remoteDB R6 class to manage remote database queries
    #' @param localDB R6 class to manange local database queries
    #' @return A new `ConditionCorrelatesManager` object.
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){
      self$applicationName <- applicationName
      self$remoteDB <- remoteDB
      self$localDB <- localDB
    },

    #' @description
    #' return hidden /shown / disabled / enabled class for GSEA button based on
    #'  chosen comparison platform and whether or not Volcano Plot is rendered
    addGSEAInputClass = function() {
      hide <- "hide"
      disabled <- "disabled"
      if (!is.null(self$ComparisonPlatform)) {
        if (grepl("SOMA", self$ComparisonPlatform) | grepl("RNA", self$ComparisonPlatform)) {
          hide <- "show"
        }
        if (!is.null(self$VolcanoSummaryData)) {
          disabled <- "enabled"
        }
      }
      return(
        glue::glue("shinyjs-{hide} shinyjs-{disabled}")
      )
    },

    #' @description
    #' Set Volcano Summary Data along with other volcano plot properties
    #'
    #' @return none

    #' @description
    #' Retrieve list of query analytes for a given platform and experiment
    getQueryAnalytes = function() {
      return(
        self$remoteDB$getQuery(
          "EXEC [shiny].[GetAnalytesByPlatformExperiment] ?, ?",
          tibble::tibble(
            "Platform" = self$Platform,
            "ExperimentID" = self$Experiment
          )
        ) |>
          dplyr::arrange(Analyte)
      )
    },

    #' @description
    #' validate that all inputs have been chosen to fetch volcano summary data
    #'
    validate_volcano_plot = function() {
      return(self$Study != "")
    },

    #' @description
    #' Set Volcano Summary Data
    #'  Also sets ParticipantComorbidities, ComorbiditySummary, Comborbidities, AnalyteData, and volcano plot properties
    #' @return none
    getVolcanoSummaryData = function() {

      self$ParticipantComorbidities <- self$getParticipantComorbiditiesByExperimentAnalyte(
        self$Experiment,
        self$QueryAnalyte
      )

      self$ComorbiditySummary <- self$getComorbidtySummary(
        self$ParticipantComorbidities,
        self$MinComorbitityMembership
      )

      self$Comorbidities <- self$ParticipantComorbidities |>
        dplyr::inner_join(self$ComorbiditySummary, by = "Condition") |>
        dplyr::select(-Condition) |>
        dplyr::rename("Condition" = label)

      self$VolcanoSummaryData <- self$Comorbidities |>
        dplyr::mutate(
          HasConditionFlag = factor(HasConditionFlag),
          HasConditionFlag = forcats::fct_relevel(HasConditionFlag, "No")
        ) |>
        dplyr::select(LabID, "Analyte" = Condition, log2MeasuredValue, HasConditionFlag, !!self$Covariates) |>
        CUSOMShinyHelpers::getStatTestByKeyGroup(
          id = LabID,
          key = Analyte,
          response = log2MeasuredValue,
          independentVariable = HasConditionFlag,
          baselineLabel = "No",
          testMethod = self$StatTest,
          adjustmentMethod = self$AdjustmentMethod,
          covariates = self$Covariates
        ) |>
        dplyr::mutate(
          formattedPValue = unlist(
            purrr::pmap(
              .l = list(p.value, p.value.adjustment.method),
              CUSOMShinyHelpers::formatPValue
            )
          ),
          text = glue::glue(
            "Condition: {Analyte}<br />fold change: {round(FoldChange,2)}<br />
            {formattedPValue}<br /><i>Click to see corresponding Sina Plot</i>"
          )
        ) |>
        dplyr::ungroup()

      self$AnalyteData <- self$VolcanoSummaryData |>
        dplyr::select(Analyte, log2FoldChange, `-log10pvalue`, text) |>
        dplyr::arrange(-log2FoldChange) |>
        dplyr::mutate(
          Analyte = forcats::fct_inorder(Analyte),
          "Analysis" = "T21 v Comorbidities"
        ) |>
        dplyr::top_n(30, wt = abs(log2FoldChange))

      self$VolcanoPlotTitle <- glue::glue("Effect of {self$analysisVariableLabel} on all {self$analytesLabel}")
      self$VolcanoSummaryMaxFoldChange <- max(abs(self$VolcanoSummaryData$log2FoldChange))
      self$VolcanoSummaryDataXAxisLabel <- "log<sub>2</sub>(Fold Change)"
      self$VolcanoSummaryDataYAxisLabel <- glue::glue(
        "-log<sub>10</sub>({ifelse(self$Adjusted,\"q-value \",\"p-value \")})"
      )

    },

    #' @description
    #' Get formatted Volcano Summary Data for data table
    #' @param .data - tibble - data to be formatted
    #' @return tibble
    getFormattedVolcanoSummaryData = function(.data) {

      adjusted_ind <- self$AdjustmentMethod != "none"
      p_value_label <- ifelse(adjusted_ind, "q-value", "p-value")
      log_10_p_value_label <- ifelse(adjusted_ind, "-log<sub>10</sub>(q-value)", "-log<sub>10</sub>(p-value)")

      old_names <- c(
        "Analyte", "No", "Yes", "log2FoldChange", "p.value.adjustment.method",
      "p.value.original", "FoldChange", "p.value", "-log10pvalue", "lmFormula"
      )
      new_names <- c(
        "Condition", "Without History of Condition", "With History of Condition",
        "log<sub>2</sub>(Fold Change)", "adjustment method", "p-value (original)",
        "Fold Change", p_value_label, log_10_p_value_label, "model"
      )

      .data |>
        dplyr::rename_with(~ new_names, all_of(old_names)) |>
        dplyr::select(-c(pvalueCutoff, formattedPValue, text, ivs))

    },

    #' @description
    #' Get Participant Comorbidities for a given experiment and analyte
    #' @param experiment - string
    #' @param query_analyte - string
    #' @return tibble
    getParticipantComorbiditiesByExperimentAnalyte = function(experiment, query_analyte) {

      dataframe <- self$remoteDB$getQuery(
        "EXEC [shiny].[GetDataByExperimentAnalyte] ?, ?",
        tibble::tibble(
          "ExperimentID" = experiment,
          "Analyte" = query_analyte
        )
      ) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})")
        ) |>
        dplyr::rename(record_id = Record_ID) |>
        dplyr::inner_join(
          arrow::open_dataset("Remote_Data/participant_conditions") |>
            dplyr::collect()|>
            dplyr::mutate(HasConditionFlag = ifelse(
              HasCondition == "True", 1,
              ifelse(HasCondition == "False", 0, NA)
              )
            ) |>
            dplyr::select(record_id, Condition, HasCondition, HasConditionFlag) |>
            dplyr::group_by(record_id, Condition) |>
            dplyr::summarise(HasConditionFlag = sum(HasConditionFlag), .groups = "drop") |>
            dplyr::mutate(HasConditionFlag = ifelse(HasConditionFlag > 0, "Yes", "No")) |>
            tidyr::drop_na()
          , by = "record_id"
        ) |>
        dplyr::inner_join(
          arrow::open_dataset("Remote_Data/participant_encounter") |>
            dplyr::collect()|>
            dplyr::inner_join(
              arrow::open_dataset("Remote_Data/participants") |>
                dplyr::collect(),
              by = "record_id"
            ) |>
            ###### LIMIT TO ONLY T21 #############
            dplyr::filter(Karyotype == "Trisomy 21") |>
            dplyr::select(record_id, LabID, Sex, "Age" = AgeAtTimeOfVisit),
          by = c("record_id", "LabID")
        )

      return(dataframe)

    },

    #' @description
    #' Get Comoborbidity Summary data - appends each comorbidity label with {positive} {negative} counts
    #' @param .data - tibble - tibble of participant comborbidity data
    #' @param min_comorbitity_membership - numeric - minimum threshold of diagnosed to include in analysis
    #' @return tibble
    getComorbidtySummary = function(.data, min_comorbitity_membership) {

      age_censor_exists <- arrow::open_dataset("Remote_Data/participant_conditions") |>
        dplyr::collect() |>
        colnames() |>
        tibble::tibble() |>
        setNames("attr") |>
        dplyr::filter(attr == "ConditionCensorshipAgeGroup") |>
        nrow() |>
        (\(val) val > 0)()

      dataframe <- .data |>
        dplyr::group_by(Condition, HasConditionFlag) |>
        dplyr::summarise(n = dplyr::n_distinct(record_id), .groups = "drop") |>
        tidyr::pivot_wider(id_cols = Condition, names_from = HasConditionFlag, values_from = n, values_fill = 0)

      if (age_censor_exists) {
        dataframe <- dataframe |>
          dplyr::inner_join(
              arrow::open_dataset("Remote_Data/participant_conditions") |>
                dplyr::collect()|>
                dplyr::select(Condition, ConditionCensorshipAgeGroup) |>
                dplyr::distinct() |>
                dplyr::mutate(ConditionCensorshipAgeGroup = ifelse(
                  is.na(ConditionCensorshipAgeGroup),
                  "Age > 0",
                  ConditionCensorshipAgeGroup)
                ),
              by = "Condition"
            )
      }

      dataframe <- dataframe |>
        dplyr::mutate(
          #total = `No` + `Yes`,
          min = ifelse(`No` <= `Yes`, `No`, `Yes`),
          incompleteFlag = dplyr::case_when(`No` == 0 ~ 1, `Yes` == 0 ~ 1, TRUE ~ 0),
          ### REMOVING AGE CENSOR FOR NOW...
          label = glue::glue("{Condition} ({`Yes`}:{`No`})")
          #label = glue("{Condition} ({`Yes`}:{`No`}) [{ConditionCensorshipAgeGroup}]")
        ) |>
        dplyr::filter(
          incompleteFlag == 0,
          min > min_comorbitity_membership
        )
        #|> dplyr::select(-c(ConditionCensorshipAgeGroup))

      return(dataframe)

    },

    #' @description
    #' Get Volcano Plot
    #' @param .data - tibble - data for plot
    #' @param ns - namespace for plot
    #' @return plotly object
    getVolcanoPlot = function(.data, ns) {

      self$volcanoSourceData <- .data |>
        dplyr::mutate(
          shape = "circle",
          selectedPoint = 0
        )

      a <- self$volcanoSourceData |>
        CUSOMShinyHelpers::getVolcanoAnnotations(
          foldChangeVar = log2FoldChange,
          significanceVariable = `-log10pvalue`,
          selected = selectedPoint,
          arrowLabelTextVar = Analyte,
          upRegulatedText = "Up in those with History of Condition",
          includeThresholdLabel = FALSE
        )

      self$volcanoSourceData <- self$volcanoSourceData |>
        CUSOMShinyHelpers::addSignificanceGroup(
          foldChangeVar = log2FoldChange,
          significanceVariable = `-log10pvalue`,
          adjustedInd = a$parameters$adjustedInd,
          significanceThreshold = a$parameters$significanceThresholdTransformed,
          originalSignificanceThreshold = a$parameters$significanceThreshold
        )

      self$volcanoPlotExpectedTraceCount <- self$volcanoSourceData |>
        dplyr::distinct(significanceGroup, shape) |>
        nrow()

      p <- self$volcanoSourceData |>
        CUSOMShinyHelpers::getVolcanoPlot(
          foldChangeVariable = log2FoldChange,
          significanceVariable = `-log10pvalue`,
          significanceGroup = significanceGroup,
          text = text,
          key = Analyte,
          color = color,
          shape = shape,
          plotName = ""
        ) |>
        plotly::layout(
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            itemclick = "toggleothers",
            itemsizing = "constant",
            valign = "middle",
            xanchor = "center",
            x = 0.5,
            y = -0.12,
            title = list(
              text = "",
              side = "left",
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
            text = HTML(glue::glue("Inpact of {self$QueryAnalyte} on all Co-Occuring Conditions")),
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
              text = glue::glue("log<sub>2</sub>(Fold Change)"),
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
            fixedrange = FALSE
          ),
          yaxis = list(
            title = list(
              text = glue::glue("-log<sub>10</sub>({ifelse(a$parameters$adjustedInd,\"q-value \",\"p-value \")})"),
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
            fixedrange = FALSE
          ),
          annotations = c(a$annotations, a$arrow),
          margin = list(t = 75)
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - Volcano Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}'),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            #list(plotlyCustomIcons$VolcanoPlotTutorial),
            # list("select2d"),
            # list("lasso2d"),
            list("zoom2d"),
            list("zoomIn2d"),
            list("zoomOut2d"),
            list("resetScale2d"),
            list("toImage")
          )
        ) |> htmlwidgets::onRender('
            function(el) {
              el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
            }'
        )

      p$x$source <- ns("VolcanoPlot")

      return(p)

    },

    #' @description
    #' helper function to update various attributes for the chosen analyte
    updateAnalyteAttributes = function() {

      self$AnalyteSearchName <- CUSOMShinyHelpers::parseDelimitedString(self$Analyte, 1)

      if (length(self$Analyte) == 1) {

        self$AnalytePlotStatAnnotation <- self$VolcanoSummaryData |>
          dplyr::filter(Analyte == self$Analyte) |>
          dplyr::ungroup() |>
          dplyr::select(p.value, p.value.adjustment.method) |>
          dplyr::mutate(formatted.p.value = CUSOMShinyHelpers::formatPValue(p.value, p.value.adjustment.method)) |>
          dplyr::select(formatted.p.value)

        self$volcanoMultiSelectText <- ""

      }

      if (length(self$Analyte) > 1) {
        self$volcanoMultiSelectText <- self$VolcanoSummaryData |>
          dplyr::filter(Analyte %in% self$Analyte) |>
          dplyr::summarise(
            count = dplyr::n(),
            minFC = round(min(FoldChange), 4),
            maxFC = round(max(FoldChange), 4),
            minP = min(p.value),
            maxP = max(p.value)
          ) |>
          dplyr::mutate(
            text = glue::glue(
              "<center>{count} points selected. Min Fold Change: {minFC}, Max Fold Change: {maxFC}</center>"
            )
          ) |>
          dplyr::select(text) |>
          dplyr::pull()
      }
    },

    #' @description
    #' helper function to add annotation to volcano plot based on chosen analyte
    #' @param plot_name string - name of target volcano plot
    #' @param ns namespace to properly derive fully-qualified plot name
    annotate_volcano_point = function(plot_name, ns) {

      plot_name <- ns(plot_name)
      if (all(self$Analyte != "")) {
        if (length(self$Analyte) == 1) {
          self$volcanoEventData <- self$volcanoSourceData |>
            dplyr::arrange(desc(significanceGroup)) |>
            dplyr::select(
              significanceGroup,
              shape,
              key = Analyte,
              x = !!self$FoldChangeVar,
              y = !!self$SignificanceVariable
            ) |>
            dplyr::mutate(
              group = glue::glue("{significanceGroup}-{shape}"),
              t = dplyr::dense_rank(group),
              curveNumber = t - 1
            ) |>
            dplyr::group_by(group) |>
            dplyr::mutate(
              r = dplyr::row_number(),
              pointNumber = r - 1
            ) |>
            dplyr::ungroup() |>
            dplyr::filter(key == self$Analyte) |>
            dplyr::select(curveNumber, pointNumber, x, y, key)

          self$volcanoEventData <- self$volcanoEventData |>
            dplyr::filter(key == self$Analyte)

          keys <- glue::glue_collapse(self$Analyte, sep = "|")
          shinyjs::runjs(
            glue::glue(
              'annotatePointByKey(
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
          shinyjs::runjs(
            glue::glue(
              'annotatePointByKey(
                "{plot_name}",
                -1,
                -1,
                "{keys}",
                5
              );'
            )
          )
          keys <- glue::glue_collapse(self$Analyte, sep = "|")
          shinyjs::runjs(glue::glue('updateSelectedKeys("{plot_name}","{keys}");'))
        }

        shinyjs::runjs(
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

        shinyjs::runjs(glue::glue('annotatePointByKey("{plot_name}","{keys}",5);'))
      }

    },

    #' @description
    #' small helper function to get correct value for volcanoMultiSelectText
    getVolcanoMultiSelectText = function() {
      if (length(self$Analyte) > 1 & self$volcanoMultiSelectText == "") {
        self$updateAnalyteAttributes()
      } else if (length(self$Analyte) == 1)  {
        self$volcanoMultiSelectText <- ""
      }
      return(self$volcanoMultiSelectText)
    },

    #' @description
    #' Get Heatmap Plot
    #' @param .data - tibble - data for plot
    #' @param ns - namespace for plot
    #' @return plotly object
    getHeatmapPlot = function(.data, ns) {

      limit <- .data |>
        dplyr::pull(log2FoldChange) |>
        abs() |>
        max() |>
        plyr::round_any(0.01, f = ceiling)

      long_data <- .data |>
        dplyr::select(name = Analyte, variable = Analysis, value = log2FoldChange) |>
        dplyr::arrange(desc(value))

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
        main = HTML(glue::glue("Fold Change with {self$analysisVariableLabel}")),
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
            text = HTML(glue::glue("Fold Change with {self$analysisVariableLabel}")),
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
            filename = glue::glue('{self$applicationName} - Heatmap {format(Sys.time(),"%Y%m%d_%H%M%S")}'),
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
    },

    #' @description
    #' Get Formatted Analyte Summary data for data table
    #' @return tibble
    getFormattedAnalyteSummaryData = function() {

      adjusted_ind <- self$AdjustmentMethod != "none"
      p_value_label <- ifelse(adjusted_ind, "q-value", "p-value")
      log_10_p_value_label <- ifelse(adjusted_ind, "-log<sub>10</sub>(q-value)", "-log<sub>10</sub>(p-value)")

      old_names <- c(
        "Analyte", "No", "Yes", "FoldChange", "p.value.original", "p.value.adjustment.method",
        "log2FoldChange", "p.value", "-log10pvalue", "lmFormula"
      )
      new_names <- c(
        "Condition", "Without History of Condition", "With History of Condition",
        "Fold Change", "p-value (original)", "adjustment method", "log<sub>2</sub>(Fold Change)",
        p_value_label, log_10_p_value_label, "Model"
      )

      return(
        self$HeatmapData |>
          dplyr::select(Analyte) |>
          dplyr::inner_join(
            self$VolcanoSummaryData |>
              dplyr::mutate(Analyte = factor(Analyte))
            , by = "Analyte"
          ) |>
          dplyr::rename_with(~ new_names, all_of(old_names)) |>
          dplyr::select(-c(formattedPValue, text, ivs))
      )

    },

    #' @description
    #' set data for boxplot
    #' @return none
    getBoxPlotData = function() {

      self$BoxplotData <- self$Comorbidities |>
        dplyr::filter(Condition == self$Analyte) |>
        dplyr::mutate(
          text = "",
          highlightGroup = dplyr::case_when(
            LabID %in% self$GroupA ~ "A",
            LabID %in% self$GroupB ~ "B"
          )
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(isBaseline = ifelse(HasConditionFlag == "No", TRUE, FALSE)) |>
        CUSOMShinyHelpers::addGroupCount(group = HasConditionFlag, addLineBreak = FALSE) |>
        dplyr::select(-n) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          text = glue::glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}")
        )

      self$AnalytePlotStatAnnotation <- self$VolcanoSummaryData |>
        dplyr::filter(Analyte == self$Analyte) |>
        dplyr::ungroup() |>
        dplyr::select(p.value, p.value.adjustment.method) |>
        dplyr::mutate(formatted.p.value = CUSOMShinyHelpers::formatPValue(p.value, p.value.adjustment.method)) |>
        dplyr::select(formatted.p.value)

    },

    #' @description
    #' get boxplot
    #' @param .data - tibble - data used for boxplot
    #' @param ns - namespace to apply to plot
    #' @return plotly object
    getBoxPlot = function(.data, ns) {

      group_baseline_label <- .data |>
        dplyr::filter(isBaseline) |>
        dplyr::select(HasConditionFlag) |>
        dplyr::distinct() |>
        dplyr::pull()

      p <- .data |>
        CUSOMShinyHelpers::getBoxPlotWithHighlightGroup(
          key = LabID,
          group = HasConditionFlag,
          groupBaselineLabel = group_baseline_label,
          value = log2MeasuredValue,
          valueLabel = log2Measurement,
          text = text,
          highlightGroup = highlightGroup,
          plotName = glue::glue("Condition")
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
            text = HTML(glue::glue("{self$Platform} - {self$QueryAnalyte} - {self$Analyte}"))
          ),
          annotations = list(
            list(
              x = 0.5,
              y = -0.07,
              text = glue::glue("History of {self$Analyte}"),
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
            filename = glue::glue('{self$applicationName} - Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}'),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            #list(plotlyCustomIcons$AnalytePlotTutorial),
            # list("select2d"),
            # list("lasso2d"),
            list("toImage")
            #list(plotlyCustomIcons$BoxplotCompareGroup),
            #list(plotlyCustomIcons$BoxplotClear)
          )
        )

      p$x$source <- ns("BoxPlot")

      return(p)

    },

    #' @description
    #' get formatted data for boxplot data table
    #' @param .data - tibble - data to format
    #' @return tibble
    getFormattedBoxplotData = function(.data) {

      dataframe <- .data

      measurement <- as.character(dataframe[1, "Measurement"])

      return(
        dataframe |>
          dplyr::select(
            "Platform" = Platform, "Study" = ExperimentStudyName,
            Analyte, LabID, Condition, HasConditionFlag, Sex, MeasuredValue
          ) |>
          dplyr::rename(`:=`(!!measurement, MeasuredValue)) |>
          dplyr::arrange(Analyte)
      )
    }

  )
)
