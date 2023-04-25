#' @export
FeatureAnalysisManager <- R6::R6Class(
  "FeatureAnalysisManager",
  private = list(),
  public = list(
    applicationName = NULL,
    namespace = NULL,
    remoteDB = NULL,
    localDB = NULL,

    analysisVariable = "",
    analysisVariableLabel = "",
    analysisType = "",
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

    BaseData = NULL,

    VolcanoSummaryData = NULL,
    VolcanoSummaryDataXAxisLabel = "",
    VolcanoSummaryDataYAxisLabel = "",
    VolcanoSummaryMaxFoldChange = 0,
    VolcanoPlotTitle = "",
    volcanoTopAnnotationLabel = "",
    volcanoPlotExpectedTraceCount = 3,
    VolcanoSummaryDataFoldChangeFilter = NULL,
    volcanoMultiSelectText = "",

    Analyte = "",
    AnalyteSearchName = "",
    AnalyteData = NULL,
    AnalytePlotMethod = "boxplot",
    AnalytePlotTitle = "",
    AnalytePlotStatAnnotation = "",
    AnalytePlotXAxisLabel = "",

    HeatmapData = NULL,

    GSEAData = NULL,
    GSEAAnalytes = "",
    GSEATraceName = "",
    GSEAGenesetName = "",
    GSEAPathwayData = NULL,

    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){

      self$applicationName <- applicationName
      self$remoteDB <- remoteDB
      self$localDB <- localDB

      namespace_config <- namespace_config |>
        dplyr::filter(Namespace == id)
      self$namespace <- namespace_config$Namespace
      self$analysisVariable <- namespace_config$AnalysisVariableName #getAnalysisVariable(namespace)
      self$analysisVariableLabel <- namespace_config$AnalysisVariableLabel #getAnalysisVariableLabel(namespace)
      self$analysisType <- namespace_config$AnalysisType #etAnalysisType(namespace)
      self$groupBaselineLabel <- namespace_config$AnalysisVariableBaselineLabel #getAnalysisVariableBaselineLabel(namespace)
      self$volcanoTopAnnotationLabel <- namespace_config$AnalysisVolcanoPlotTopAnnotation #getVolcanoTopAnnotationLabel(namespace)

    },

    getKaryotypeChoices = function(karyotypes) {

      if(self$namespace == "Karyotype") {
        return(
          tibble::tibble(
            choiceNames = glue::glue(
              '<div>
                {glue::glue_collapse(karyotypes,sep = " vs. ")}
                <span
                  data-toggle="tooltip"
                  data-placement="auto right"
                  title=""
                  class="fas fa-info-circle gtooltip info-tooltip"
                  data-original-title="Test for differences between Trisomy 21 & Controls">
                </span>
              </div>'
            ),
            choiceValues = glue::glue_collapse(karyotypes,sep=";")
          )
        )
      }

      else if(self$namespace == "Comorbidity") {
        return(
          tibble::tibble(
            choiceNames =  karyotypes[1],
            choiceValues = karyotypes[1]
          )
        )
      }

      else {

        karyotypeInputCounts <- self$localDB$getQuery(
          "SELECT LabID,Analyte,Karyotype
          FROM sourceData
          WHERE ExperimentStudyName  = ({study})",
          tibble::tibble(study = self$Study)
          ) |>
          dplyr::group_by(Analyte,Karyotype) |>
          dplyr::summarise(
            n = dplyr::n_distinct(LabID), .groups = 'drop'
          ) |>
          dplyr::ungroup() |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarise(
            n = round(median(n)), .groups = 'drop'
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            sort = dplyr::case_when(
              Karyotype == "Trisomy 21" ~ 1,
              TRUE ~ 99
            ),
            choiceNames = glue::glue("{Karyotype} (n={n})"),
            choiceValues = Karyotype
          ) |>
          dplyr::arrange(sort)

        if(self$analysisType == "Continuous") {
          return(
            karyotypeInputCounts |>
              dplyr::bind_rows(
                tibble::tibble(
                  Karyotype = glue::glue_collapse(karyotypes, sep = ";"),
                  n = NA,
                  sort = 999,
                  choiceNames = glue::glue(
                    '<div>{glue::glue_collapse(karyotypes, sep = " vs. ")}
                        <span
                          data-toggle="tooltip"
                          data-placement="auto right"
                          title=""
                          class="fas fa-info-circle gtooltip info-tooltip"
                          data-original-title="Test for differences in {self$analysisVariable} trajectories between Trisomy 21 & Controls">
                        </span>
                      </div>'
                  ),
                  choiceValues = glue::glue_collapse(karyotypes,sep=";")
                )
              ) |>
              dplyr::arrange(sort)
          )
        } else {
          return(
            karyotypeInputCounts
          )
        }
      }
    },

    getConditionTree = function(conditions) {

      tree <- conditions |>
        CUSOMShinyHelpers::dfToTree()

      if (!is.null(self$Conditions)) {
        selected_nodes <- get_selected(self$Conditions, format = "classid") |>
          unlist() |>
          tibble::as_tibble() |>
          dplyr::pull()

        if(length(selected_nodes) > 0) {
          for(i in seq_along(tree)) {
            if(is.list(tree[i])) {
              for(node in names(tree[i][[1]])) {
                if(node %in% selected_nodes) {
                  attr(tree[[i]][[node]],"stselected") <- TRUE
                  attr(tree[[i]][[node]],"stopened") <- TRUE
                }
              }
            }
          }
        }
      }
      return(
        tree
      )
    },

    getCovariateChoices = function() {
      return(
        setdiff(c("Age","Sex"),self$analysisVariable)
      )
    },

    getDisabledInputClass = function(inputName) {
      if(self$analysisVariable == inputName) {
        return(
          "shinyjs-disabled"
        )
      }
    },

    getHiddenInputClass = function(inputName) {
      if(self$analysisVariable == inputName) {
        return(
          "shinyjs-hide"
        )
      }
    },

    addInputSpecialClass = function(inputName, class = c("disabled","hide")){
      class <- match.arg(class)
      if(self$analysisVariable == inputName) {
        return(
          glue::glue("shinyjs-{class}")
        )
      }
    },

    addGSEAInputClass = function() {
      hide <- "hide"
      disabled <- "disabled"
      if(!is.null(self$Study)) {
        if (grepl('SOMA',self$Study) | grepl('RNA', self$Study)) {
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

    showGSEAButton = function(button_name) {
      if (grepl('SOMA',self$Study) | grepl('RNA', self$Study)) {
        return(
          do.call(shinyjs::show, list(button_name))
        )
      }
      else {
        return(
          do.call(shinyjs::hide, list(button_name))
        )
      }
    },

    enableGSEAButton = function(button_name) {
      if(!is.null(self$VolcanoSummaryData)) {
        do.call(shinyjs::enable, list(button_name))
      }
      else {
        do.call(shinyjs::disable, list(button_name))
      }
    },

    getBaseData = function() {

      self$BaseData <- self$localDB$getQuery(
        "SELECT ExperimentStudyName, LabID, Karyotype, Sex, Age, BMI, Analyte, MeasuredValue, Measurement
          FROM sourceData
          WHERE ExperimentStudyName = ({study})
          AND (Age >= ({minAge}) OR Age IS NULL)
          AND (Age <= ({maxAge}) OR Age IS NULL)
          AND Sex IN ({sexes*})
          AND Karyotype IN ({karyotypes*})",
        tibble::tibble(
          study = self$Study,
          minAge = min(self$Age),
          maxAge = max(self$Age),
          sexes = self$Sex,
          karyotypes = unlist(stringr::str_split(self$Karyotype, pattern = ';'))
        )
      ) |>
        dplyr::filter(!is.na(!!rlang::sym(self$analysisVariable))) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})")
        )

    },

    getVolcanoSummaryData = function() {

      self$getBaseData()

      self$Adjusted <- self$AdjustmentMethod != "none"

      if(self$analysisType == "Categorical") {

        dataframe <- self$BaseData |>
          dplyr::select(LabID, Analyte, log2MeasuredValue, self$analysisVariable, self$Covariates) |>
          dplyr::mutate_at(dplyr::vars(self$analysisVariable), ~forcats::fct_relevel(.x, self$groupBaselineLabel)) |>
          CUSOMShinyHelpers::getStatTestByKeyGroup(
            id = LabID,
            key = Analyte,
            response = log2MeasuredValue,
            independentVariable = !!rlang::sym(self$analysisVariable),
            baselineLabel = self$groupBaselineLabel,
            testMethod = self$StatTest,
            adjustmentMethod = self$AdjustmentMethod,
            covariates = self$Covariates
          ) |>
          dplyr::mutate(
            formattedPValue = unlist(purrr::pmap(.l = list(p.value,p.value.adjustment.method), CUSOMShinyHelpers::formatPValue)),
            text = glue::glue('Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}')
          ) |>
          dplyr::ungroup()

      }

      else {

        dataframe <- self$BaseData |>
          dplyr::select(LabID, Analyte, log2MeasuredValue, self$analysisVariable, self$Covariates, Karyotype ) |>
          dplyr::mutate(Karyotype = forcats::fct_relevel(Karyotype, "Control")) |>
          CUSOMShinyHelpers::getLinearModelWithInteraction(
            id = LabID,
            key = Analyte,
            response = log2MeasuredValue,
            independentVariable = !!rlang::sym(self$analysisVariable),
            covariates = self$Covariates,
            interactionVariable = Karyotype,
            adjustmentMethod = self$AdjustmentMethod
          ) |>
          dplyr::mutate(
            formattedPValue = unlist(purrr::pmap(.l = list(p.value,p.value.adjustment.method), CUSOMShinyHelpers::formatPValue)),
            text = glue::glue('Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}')
          ) |>
          dplyr::ungroup()
      }

      if(nrow(dataframe)>0) {

        self$VolcanoSummaryData <- dataframe |>
          dplyr::mutate(
            log2FoldChange= log2(FoldChange),
            `-log10pvalue` = -log10(p.value),
            `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
            formattedPValue = unlist(purrr::pmap(.l = list(p.value,p.value.adjustment.method), CUSOMShinyHelpers::formatPValue)),
            text = glue::glue('Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}')
          )

        self$VolcanoPlotTitle <- glue::glue("Effect of {self$analysisVariableLabel} on all {self$analytesLabel}")
        self$VolcanoSummaryMaxFoldChange <- max(abs(self$VolcanoSummaryData$log2FoldChange))
        self$VolcanoSummaryDataXAxisLabel <- "log<sub>2</sub>(Fold Change)"
        self$VolcanoSummaryDataYAxisLabel <- glue::glue("-log<sub>10</sub>({ifelse(self$Adjusted,\"q-value \",\"p-value \")})")

      }
    },

    getFormattedVolcanoSummaryData =  function(.data) {

      adjustedInd <- self$AdjustmentMethod != "none"
      pValueLabel <- ifelse(adjustedInd,"q-value","p-value")
      log10pValueLabel <- ifelse(adjustedInd,"-log<sub>10</sub>(q-value)","-log<sub>10</sub>(p-value)")

      oldNames = c("log2FoldChange", "p.value.adjustment.method", "p.value.original", "FoldChange", "p.value", "-log10pvalue", "lmFormula")
      newNames = c("log<sub>2</sub>(Fold Change)", "adjustment method", "p-value (original)", "Fold Change", pValueLabel, log10pValueLabel,"model")

      .data |>
        dplyr::rename_with(~ newNames, all_of(oldNames)) |>
        dplyr::select(-c(pvalueCutoff,formattedPValue,text,ivs))

    },

    getVolcanoPlot = function(.data, ns) {

      .data <- .data |>
        dplyr::mutate(
          shape = "circle",
          selectedPoint = 0
        )

      a <- .data |>
        CUSOMShinyHelpers::getVolcanoAnnotations(
          foldChangeVar = !!rlang::sym(self$FoldChangeVar),
          significanceVariable = !!rlang::sym(self$SignificanceVariable),
          selected = selectedPoint,
          arrowLabelTextVar = self$Analyte,
          upRegulatedText = self$volcanoTopAnnotationLabel,
          includeThresholdLabel = FALSE
        )

      .data <- .data |>
        CUSOMShinyHelpers::addSignificanceGroup(
          foldChangeVar = !!rlang::sym(self$FoldChangeVar),
          significanceVariable = !!rlang::sym(self$SignificanceVariable),
          adjustedInd = a$parameters$adjustedInd,
          significanceThreshold = a$parameters$significanceThresholdTransformed,
          originalSignificanceThreshold = a$parameters$significanceThreshold
        )

      self$volcanoPlotExpectedTraceCount <- .data |>
        dplyr::distinct(significanceGroup, shape) |>
        nrow()

      p <- .data |>
        CUSOMShinyHelpers::getVolcanoPlot(
          foldChangeVariable = !!rlang::sym(self$FoldChangeVar),
          significanceVariable = !!rlang::sym(self$SignificanceVariable),
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
            orientation = 'h',
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
          annotations=c(a$annotations, a$arrow),
          margin = list( t = 75)
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - {self$Study} Volcano Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
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
        ) |> htmlwidgets::onRender('
          function(el) {
            el.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
          }'
        )

      p$x$source <- ns("VolcanoPlot")

      return(p)

    },

    updateAnalyteAttributes = function() {

      self$AnalyteSearchName <- CUSOMShinyHelpers::parseDelimitedString(self$Analyte,1)

      if(length(self$Analyte) == 1 ) {

        self$AnalytePlotStatAnnotation <- self$VolcanoSummaryData |>
          dplyr::filter(Analyte == self$Analyte) |>
          dplyr::ungroup() |>
          dplyr::select(p.value,p.value.adjustment.method) |>
          dplyr::mutate(formatted.p.value = CUSOMShinyHelpers::formatPValue(p.value,p.value.adjustment.method)) |>
          dplyr::select(formatted.p.value)

        self$volcanoMultiSelectText <- ""

      }

      if(length(self$Analyte) > 1) {
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
            text = glue::glue("<center>{count} points selected. Min Fold Change: {minFC}, Max Fold Change: {maxFC}</center>")
          ) |>
          dplyr::select(text) |>
          dplyr::pull()
      }
    },

    getAnalyteData = function() {

      self$AnalytePlotMethod <- getAnalytePlotMethod(self$analysisType, length(self$Analyte))
      self$AnalytePlotTitle <- getAnalytePlotTitle(self$analysisVariableLabel, self$AnalytePlotMethod, self$Analyte, self$Karyotype)
      # reset label -- gets formatted with HTML tags and (n=__)
      self$groupBaselineLabel <- stringr::str_split(gsub("<.*?>", "", self$groupBaselineLabel),' ', simplify = TRUE)[1]

      if(length(self$Analyte) == 1) {

        self$AnalyteData <- self$BaseData |>
          dplyr::filter(
            Analyte %in% self$Analyte,
            log2MeasuredValue != Inf,
            log2MeasuredValue != -Inf
          ) |>
          dplyr::mutate(
            log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
            log2Measurement = glue::glue("log<sub>2</sub>({Measurement})"),
            highlightGroup = dplyr::case_when(
              1 == 1 ~ NA
              # LabID %in% input$GroupA ~ "A",
              # LabID %in% input$GroupB ~ "B"
            )
          )

        if(self$AnalytePlotMethod == "boxplot") {

           self$AnalyteData <- self$AnalyteData |>
            dplyr::rowwise() |>
            CUSOMShinyHelpers::addGroupCount(group = !!rlang::sym(self$analysisVariable), addLineBreak = FALSE) |>
            dplyr::select(-n) |>
            dplyr::ungroup() |>
            dplyr::mutate( text = glue::glue("LabID: {LabID} <br />{log2Measurement}: {log2MeasuredValue}") )

          self$groupBaselineLabel <- self$AnalyteData |>
            dplyr::select(!!rlang::sym(self$analysisVariable)) |>
            dplyr::distinct() |>
            dplyr::filter(grepl(self$groupBaselineLabel,!!rlang::sym(self$analysisVariable))) |>
            dplyr::pull()

          self$AnalytePlotXAxisLabel <- getAnalytePlotXAxisLabel(self$namespace,self$analysisVariableLabel)

        }

      }

      else {

        self$AnalytePlotMethod <- "heatmap"

        self$AnalyteData <- self$VolcanoSummaryData |>
          dplyr::filter(Analyte %in% self$Analyte) |>
          dplyr::select(Analyte, log2FoldChange, text) |>
          dplyr::arrange(-log2FoldChange) |>
          dplyr::mutate(Analyte = forcats::fct_inorder(Analyte), "Analysis" = "T21vD21")

      }
    },

    getAnalytePlot = function(.data, ns) {

      if (self$AnalytePlotMethod == "boxplot") {

        p <- .data |>
          CUSOMShinyHelpers::getBoxPlotWithHighlightGroup(
            key = LabID,
            group = !!rlang::sym(self$analysisVariable),
            groupBaselineLabel = self$groupBaselineLabel,
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
              filename = glue::glue('{self$applicationName} - {self$Analyte} Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}'),
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
          CUSOMShinyHelpers::getScatterPlotByGroup(
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
              filename = glue::glue('{self$applicationName} - {self$Analyte} Analyte Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}'),
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

      }

    },

    getFormattedAnalyteSummaryData =  function(.data) {

      if(self$AnalytePlotMethod == "boxplot") {

        dataframe <- self$AnalyteData

        measurement <- as.character(dataframe[1,'Measurement'])

        return(
          dataframe |>
            dplyr::select("Study" = ExperimentStudyName, Analyte, LabID, Karyotype, Sex, MeasuredValue) |>
            dplyr::rename(`:=`(!!measurement, MeasuredValue)) |>
            dplyr::arrange(Analyte)
        )

      }

      else {

        adjustedInd <- self$AdjustmentMethod != "none"
        pValueLabel <- ifelse(adjustedInd,"q-value","p-value")
        log10pValueLabel <- ifelse(adjustedInd,"-log<sub>10</sub>(q-value)","-log<sub>10</sub>(p-value)")

        oldNames = c("FoldChange","p.value.original", "p.value.adjustment.method","log2FoldChange", "p.value", "-log10pvalue","lmFormula")
        newNames = c("Fold Change","p-value (original)","adjustment method","log<sub>2</sub>(Fold Change)", pValueLabel, log10pValueLabel,"Model")

        return(
          self$AnalyteData |>
            dplyr::select(Analyte) |>
            dplyr::inner_join(self$VolcanoSummaryData, by = "Analyte") |>
            dplyr::rename_with(~ newNames, all_of(oldNames)) |>
            dplyr::select(-c(formattedPValue,text,ivs))
        )

      }

    },
    getGSEAData = function() {

      ranks <- self$VolcanoSummaryData |>
        dplyr::rowwise() |>
        dplyr::mutate(ParsedComparisonAnalyte = CUSOMShinyHelpers::parseDelimitedString(Analyte,1)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          ID = ParsedComparisonAnalyte,
          t = (`-log10pvalue` * log2FoldChange)
        ) |>
        dplyr::select(ID, t) |>
        dplyr::filter(!is.na(t)) |>
        dplyr::arrange(-abs(t)) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        tibble::deframe()

      gsea <- CUSOMShinyHelpers::runfGSEA(geneset = TrisomExploreR:::GSEA_hallmarks, ranks = ranks) |>
        dplyr::mutate(
          Leading.edge.genes = purrr::map_chr(leadingEdge, toString),
          Leading.edge.genes = gsub(' ','',Leading.edge.genes)
        ) |>
        dplyr::select("Gene.set" = pathway, "Size" = size, ES, NES, "p.value" = pval, "q.value" = padj, Leading.edge.genes) |>
        dplyr::mutate(Gene.set = stringr::str_to_title(trimws(gsub('_',' ',gsub('HALLMARK','',Gene.set)))))

      self$GSEAData <- list(
        "ranks" = ranks,
        "hallmarks" = GSEA_hallmarks,
        "gsea" = gsea
      )
    },

    getGSEAPlot = function(.data, ns) {

      data <- .data$gsea |>
        dplyr::mutate(
          `-log10qvalue` = -log(q.value),
          text = glue::glue("Gene Set: {Gene.set}<br />NES: {NES} <br />-log<sub>10</sub>(q-value): {`-log10qvalue`}")
        ) |>
        dplyr::top_n(25, wt = abs(NES))

      limit <- data |>
        dplyr::pull(NES) |>
        abs() |>
        max() |>
        plyr::round_any(1, f = ceiling) * 1.15

      p <- data |>
        dplyr::arrange(`-log10qvalue`) |>
        plotly::plot_ly(
          type = "bar",
          x = ~ `-log10qvalue`,
          y = ~ reorder(Gene.set,NES),
          hoverinfo = "text",
          hovertext = ~ text,
          customdata = ~ Leading.edge.genes,
          marker = list(
            color = ~ NES,
            autocolorscale = FALSE,
            colorscale = "RdBlu",
            cauto  = FALSE,
            cmax = limit,
            cmid = 0,
            cmin = -limit,
            colorbar = list(
              title = 'NES',
              tickmode = "auto",
              len = 0.5,
              yanchor = "middle",
              y = 0.5
            )
          )
        ) |>
        plotly::layout(
          title = list(
            text = "GSEA: Top 25 Hallmark gene sets <br />T21 vs. Control"
          ),
          showlegend = FALSE,
          xaxis = list(
            title = list(
              text = "-log<sub>10</sub>(q-value)"
            ),
            showlines = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            title = list(
              text = ""
            ),
            showlines = FALSE
          ),
          margin = list(
            t = 65
          ),
          shapes = list(
            list(
              type = "line",
              xref = "x",
              yref = "paper",
              axref = "paper",
              ayref = "y",
              y0 = 0,
              y1 = 1,
              x0 = -log(0.1),
              x1 = -log(0.1),
              line = list(
                color = "black",
                dash = "dot"
              )
            )
          )
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - GSEA Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      p$x$source <- ns("GSEAPlot")

      return(p)

    },

    getGSEAPathwayData = function(pathName) {

      gseaParam <- 0

      stats <- self$GSEAData$ranks

      pathwayNammed <- self$GSEAData$gsea |>
        dplyr::filter(Gene.set == pathName) |>
        dplyr::select(Leading.edge.genes) |>
        dplyr::mutate(id = dplyr::row_number()) |>
        tidyr::separate_rows(Leading.edge.genes, sep = ",") |>
        dplyr::select(-id) |>
        purrr::simplify()

      rnk <- rank(-stats)
      ord <- order(rnk)
      statsAdj <- stats[ord]
      statsAdj <- sign(statsAdj) * (abs(statsAdj)^gseaParam)
      statsAdj <- statsAdj/max(abs(statsAdj))

      pathway <- unname(as.vector(na.omit(match(pathwayNammed, names(statsAdj)))))
      pathway <- sort(pathway)

      gseaRes <- fgsea::calcGseaStat(
        statsAdj,
        selectedStats = pathway,
        returnAllExtremes = TRUE
      )

      bottoms <- gseaRes$bottoms
      tops <- gseaRes$tops
      n <- length(statsAdj)
      xs <- as.vector(rbind(pathway - 1, pathway))
      ys <- as.vector(rbind(bottoms, tops))

      GSEAScores <- tibble::tibble(
        x = c(0, xs, n + 1),
        y = c(0, ys, 0)
      ) |>
      dplyr::inner_join(
        tibble::tibble(x = pathway, Gene = pathwayNammed),
        by = "x"
      ) |>
      dplyr::rename("Rank" = x, "ES" = y) |>
      dplyr::relocate("Gene")

      self$GSEAPathwayData <- self$VolcanoSummaryData |>
        dplyr::filter(Analyte %in% self$Analyte) |>
        dplyr::select(Analyte, log2FoldChange, `-log10pvalue`) |>
        tidyr::separate(col = "Analyte", into = "Gene", remove = FALSE) |>
        dplyr::left_join(GSEAScores, by = "Gene" ) |>
        dplyr::group_by(Gene) |>
        dplyr::arrange(-log2FoldChange) |>
        dplyr::mutate(r = dplyr::row_number()) |>
        dplyr:: ungroup() |>
        dplyr::filter(r == 1) |>
        dplyr::select(-c(ES,r)) |>
        dplyr::mutate(
          log2FoldChange = format(log2FoldChange, scientific = TRUE),
          `-log10pvalue` = format(`-log10pvalue`,scientific = TRUE),
          Gene = glue::glue("<a href=\"https://www.genecards.org/Search/Keyword?queryString={Gene}\" target=\"_blank\">{Gene}</a>")
        ) |>
        dplyr::relocate(Gene) |>
        dplyr::rename("log<sub>2</sub>(Fold Change)" = log2FoldChange, "-log<sub>10</sub>(q-value)" = `-log10pvalue`) |>
        dplyr::arrange(Rank)

      self$GSEAAnalytes <- self$GSEAPathwayData |>
        dplyr::select(Analyte) |>
        dplyr::summarise(text = toString(Analyte)) |>
        dplyr::mutate(text = gsub(', ','|',text)) |>
        dplyr::pull()

      self$GSEAGenesetName <- glue::glue("HALLMARK_{gsub(' ','_',stringr::str_to_upper(self$GSEATraceName))}")

    },

    getGSEAEnrichmentPlot = function(.data, ns) {

      p <- CUSOMShinyHelpers::plotGSEAEnrichment(
        pathName = self$GSEATraceName,
        stats = .data$ranks,
        res = .data$gsea,
        title = glue::glue("T21 vs. Control:\n{self$GSEATraceName}")
      ) |>
      plotly::layout(
        margin = list(
          autoexpand = TRUE,
          l = 10,
          r = 30,
          t = 75
        )
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "svg",
          filename = glue::glue('{self$applicationName} - {self$Study} GSEA Enrichment Plot {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
          width = NULL,
          height = NULL
        ),
        modeBarButtons = list(
          list("zoom2d"),
          list("zoomIn2d"),
          list("zoomOut2d"),
          list("resetScale2d"),
          list("toImage")
        )
      )

      p$x$source <- ns("GSEAEnrichmentPlot")

      p

    }

  )
)

getAnalytePlotMethod <- function(analysisType, analyteCount) {
  if(analyteCount > 1) {
    return("heatmap")
  }
  else if(analysisType == "Categorical") {
    return("boxplot")
  }
  else if(analysisType == "Continuous") {
    return("scatterplot")
  }
  else {
    return("unknown")
  }
}

getAnalytePlotTitle <- function(analysisVariable, plotMethod, analyte, groupVariable) {

  groupVarCount <- length(stringr::str_split(groupVariable, pattern = ";", simplify = TRUE))

  if(plotMethod == "boxplot") {
    return(glue::glue("Effect of {analysisVariable} on {analyte}"))
  }
  else if (plotMethod == "scatterplot" && groupVarCount == 1) {
    return(glue::glue("Effect of {analysisVariable} in {groupVariable} on {analyte}"))
  }
  else if (plotMethod == "scatterplot" && groupVarCount > 1) {
    glue::glue("Comparison of {analysisVariable} trajectories between karyotype for {analyte}")
  }
}

getAnalytePlotXAxisLabel <- function(namespace,analysisVariable) {
  if(namespace == "Comorbidity") {
    return(
      glue::glue("Has Any {analysisVariable}")
    )
  }
  else {
    return(
      analysisVariable
    )
  }
}
