#' Plotly function to generate standard volcano plot from fold change data
#'
#' @param .data dataframe containing fold change data
#' @param foldChangeVariable fold change variable
#' @param significanceVariable p value variable
#' @param significanceGroup significance group column - each group
#' results in a separate trace
#' @param text column containing text values to show in tooltip
#' @param key - key column in fold change data - will be used for
#' capuring select events
#' @param plotName - name to attribute to plot
#' (used for tracking clicks, events, etc)
#' @param color - column with color to use for each row / group in dataset -
#' should be unique per significance group / trace
#' @param shape - optional shape - should be passed as a nammed
#' column in source data.
#' @return returns Plotly scatter plot showing fold change vs p value
#' colored by significance group
#' @importFrom rlang enquo
#' @import dplyr
#' @import plotly
#' @importFrom stringr str_detect
#' @export
getVolcanoPlot <- function(
  .data,
  foldChangeVariable,
  significanceVariable,
  significanceGroup,
  text,
  key,
  plotName,
  color,
  shape = "circle"
) {

  foldChangeVariable <- rlang::enquo(foldChangeVariable)
  significanceVariable <- rlang::enquo(significanceVariable)
  significanceGroup <- rlang::enquo(significanceGroup)
  text <- rlang::enquo(text)
  key <- rlang::enquo(key)
  color <- rlang::enquo(color)
  shape <- rlang::enquo(shape)

  maxFoldChange <- getMaxAbsValue(
    .data,
    !!foldChangeVariable,
    inf.rm = TRUE,
    buffer = 1.1
  )

  maxPValue <- getMaxAbsValue(
    .data,
    !!significanceVariable,
    inf.rm = TRUE,
    buffer = 1.1
  )

  if (maxPValue < 5) {
    maxPValue <- 5
  }

  unselectedOpacity <- ifelse(
      nrow(.data |>
        dplyr::filter(selectedPoint == 1)
      ) == 0,
      1.0,
      0.7
    )

  groups <- .data |>
    dplyr::select(!!significanceGroup, !!shape) |>
    dplyr::distinct() |>
    dplyr::mutate(
      sortOrder = dplyr::case_when(
        stringr::str_detect(significanceGroup, "down") ~ 1,
        stringr::str_detect(significanceGroup, "up") ~ 999,
        TRUE ~ 500
      )
    ) |>
    dplyr::arrange(sortOrder)

  p <- plotly::plot_ly()

  for(i in 1:nrow(groups)) {

    i_group <- as.character(groups[i, 1])
    i_shape <- as.character(groups[i, 2])
    i_showlegend <- i_shape == "circle"

    df <- .data |>
      dplyr::filter(
        !!significanceGroup == i_group,
        !!shape == i_shape
      )

    selectedIndex <- ifelse(
      nrow(
        df |>
          dplyr::filter(selectedPoint == 1)
      ) > 0,
      which(df$selectedPoint == 1) - 1,
      -1
    )

    groupColor <- df |>
      dplyr::select(!!color) |>
      dplyr::distinct() |>
      dplyr::pull()

    p <- p |>
      plotly::add_trace(
        data = df,
        type = "scatter",
        x = foldChangeVariable,
        y = significanceVariable,
        name = i_group,
        text = text,
        hoverinfo = "text",
        mode = "markers",
        color = significanceGroup,
        colors = groupColor,
        key = key,
        showlegend = i_showlegend,
        legendgroup = i_group,
        marker = list(
          symbol = shape,
          size = 8,
          width = 2,
          color = groupColor
        ),
        selectedpoints = list(
          selectedIndex
        ),
        selected = list(
          marker = list(
            color = "#ff0000",
            opacity = 1,
            size = 14
          )
        ),
        unselected = list(
          marker = list(
            color = groupColor,
            opacity = unselectedOpacity,
            size = 8
          )
        )
      )
  }

  p <- p |>
    plotly::layout(
      showlegend = TRUE,
      legend = list(
        x = 100,
        y = 0.1,
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
        zeroline = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        range = c(-maxFoldChange, maxFoldChange),
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
        zeroline = TRUE,
        showline = TRUE,
        showticklabels = TRUE,
        range = c(0, maxPValue),
        fixedrange = FALSE
      ),
      margin = list(
        autoexpand = TRUE,
        l = 10,
        r = 30,
        t = 30
      )
    )

  p$x$source <- paste0(plotName, "VolcanoPlot")

  p

}

#' volcano plot Plotly function to get standard volcano annotations
#'
#' @param maxFoldChange - numeric - indicates the max value along
#' the x axis for the plot (fold change) - helps anchor the
#' annotations along the x-axis
#' @param upRegulatedText - text to use for "up regulated" groups --
#' down regulated text will simply replace "up" with "down"
#' @param significanceThreshold - numeric - significance threshold
#' @param adjustedInd - logical - indicates if the significance
#' variable has been adjusted with a multiple hypothesis correction
#' @param includeThresholdLabel - logical - whether to include
#' arrow with significance threshold
#' @return list of lists of plotly annotation objects
#' -- up regulated arror at top of plot (with up regulated text)
#' -- down regulated arrow at the top of plot
#' -- p value threshold with "p or q < threshold" text and up arrow
#' @import dplyr
#' @importFrom stringr str_replace
#' @export

getDefaultVolcanoAnnotations <- function(
  maxFoldChange,
  upRegulatedText,
  significanceThreshold,
  adjustedInd = FALSE,
  includeThresholdLabel = TRUE
) {

  upTextLength <- nchar(upRegulatedText) * 1.25

  upAnchor <- 0.75 + (upTextLength / 100 / 2)

  downRegulatedText <- dplyr::case_when(
    grepl("Up", upRegulatedText) ~ stringr::str_replace(
      upRegulatedText, "Up", "Down"
    ),
    grepl("Increasing", upRegulatedText) ~ stringr::str_replace(
      upRegulatedText, "Increasing", "Decreasing"
    ),
    grepl("Greater", upRegulatedText) ~ stringr::str_replace(
      upRegulatedText, "Greater", "Decreased"
    ),
    TRUE ~ upRegulatedText
  )

  downTextLength <- nchar(downRegulatedText) * 1.25

  downAnchor <- 0.25 - (downTextLength / 100 / 2)

  significanceThresholdAnnotation <- ifelse(adjustedInd, "q", "p")

  return(
    list(
      list(
        x = maxFoldChange,
        y = 1,
        xref = "x",
        yref = "paper",
        axref = "x",
        ayref = "y",
        showarrow = TRUE,
        arrowcolor = "#1D4D7C",
        ax = 0,
        ay = 0
      ),
      list(
        x = upAnchor,
        y = 1.06,
        text = upRegulatedText,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(
          family = "Arial",
          size = 12
          )
      ),
      list(
        x = -maxFoldChange,
        y = 1,
        xref = "x",
        yref = "paper",
        axref = "x",
        ayref = "y",
        showarrow = TRUE,
        arrowcolor = "#3E99CD",
        ax = 0,
        ay = 0
      ),
      list(
        x = downAnchor,
        y = 1.06,
        text = downRegulatedText,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(
          family = "Arial",
          size = 12
        )
      ),
      list(
        x = 0.025,
        y = 0.015,
        text = ifelse(
          includeThresholdLabel,
          glue::glue("&#9650; <b>{significanceThresholdAnnotation} < {significanceThreshold}</b>"),
          ""
        ),
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
      )
    )
  )
}

#' volcano plot Plotly function to get standard volcano line dividing significance cutoff
#'
#' @param cutoffThreshold - numeric - y coordinate for line
#' @param color - string - color to use for line - defaults to "black"
#' @param lineType - string - line style to use - defaults to "dash"
#' @return list of lists with line shape definition
#'
#'
#' @export
getDefaultVolcanoLine <- function(cutoffThreshold, color="black", lineType="dash") {

  return(
    list(
      list(
        type = "line",
        xref = "paper",
        yref = "y",
        axref = "paper",
        ayref = "y",
        y0 = cutoffThreshold,
        y1 = cutoffThreshold,
        x0 = 0,
        x1 = 1,
        line = list(
          color = color,
          dash = lineType
        )
      )
    )
  )

}

#' gets all possible volcano plot annotations in a single call.
#' Combines list output from default and arrow annotations.
#'
#' @param .data dataframe containing fold change data along
#' with selected point indicator "selected_"
#' @param foldChangeVar fold change variable
#' @param significanceVariable significance value variable
#' @param selected selected indicator column
#' @param arrowLabelTextVar name of column in dataframe to
#' pull for arrow annotation text
#' @param ... - additional named arguments to include in calls to
#' other functions.
#' @return returns list of lists of annotation objects
#' -- annotations - list of default volcano annotations
#' -- shapes - list of shapes - defaults to dotted line separating
#' significance groups
#' -- arrow - list of arrow annotations to highlight points on volcano plot
#' -- parameters - metatdata used to generate annotation lists
#' @importFrom rlang enquo
#' @import dplyr
#' @export
getVolcanoAnnotations <- function(
  .data,
  foldChangeVar,
  significanceVariable,
  selected,
  arrowLabelTextVar,
  ...
) {

  foldChangeVar <- rlang::enquo(foldChangeVar)
  foldChangeVar <- rlang::enquo(foldChangeVar)
  significanceVariable <- rlang::enquo(significanceVariable)
  selected <- rlang::enquo(selected)
  arrowLabelTextVar <- rlang::enquo(arrowLabelTextVar)

  maxFoldChange <- getMaxAbsValue(
    .data,
    !!foldChangeVar,
    inf.rm = TRUE,
    buffer = 1.1
  )

  includeArrow <- dim(
    .data |>
      dplyr::filter(
        !!selected == 1,
        !is.na(!!foldChangeVar),
        !is.na(!!significanceVariable)
      )
    )[1] > 0

  adjustmentMethodVar <- colnames(.data)[which(grepl('adjust', colnames(.data)))][1]

  tranformationVar <- colnames(.data)[which(grepl('log', colnames(.data)))][1]

  adjustmentMethod <- .data |>
    dplyr::ungroup() |>
    dplyr::select(!!adjustmentMethodVar) |>
    dplyr::distinct() |>
    dplyr::pull()

  adjustedInd <- adjustmentMethod != "none"

  significanceThreshold <- ifelse(adjustedInd, 0.1, 0.05)

  significanceThresholdTransformed <- ifelse(
    !is.na(tranformationVar),
    -log10(significanceThreshold),
    significanceThreshold
  )

  parameters <- list(
    "maxFoldChange" = maxFoldChange,
    "includeArrow" = includeArrow,
    "adjustmentMethod" = adjustmentMethod,
    "adjustedInd" = adjustedInd,
    "significanceThreshold" = significanceThreshold,
    "significanceThresholdTransformed" = significanceThresholdTransformed
  )

  annotations <- getDefaultVolcanoAnnotations(
    maxFoldChange,
    significanceThreshold = significanceThreshold,
    adjustedInd = adjustedInd,
    ...
    )

  shapes <- getDefaultVolcanoLine(
    cutoffThreshold = significanceThresholdTransformed,
    color = "black",
    lineType = "dash"
  )

  if (includeArrow) {

    xcoordinate <- .data |>
      dplyr::ungroup() |>
      dplyr::filter(!!selected == 1) |>
      dplyr::select(!!foldChangeVar) |>
      dplyr::pull()

    ycoordinate <- .data |>
      dplyr::ungroup() |>
      dplyr::filter(!!selected == 1) |>
      dplyr::select(!!significanceVariable) |>
      dplyr::pull()

    arrowLabelText <- .data |>
      dplyr::ungroup() |>
      dplyr::filter(!!selected == 1) |>
      dplyr::select(!!arrowLabelTextVar) |>
      dplyr::pull()

    arrow <- getVolcanoArrowAnnotation(
      xcoordinate,
      ycoordinate,
      arrowLabelText
    )

  } else {

    arrow <- list()

  }

  return(
    list(
      "annotations" = annotations,
      "shapes" = shapes,
      "arrow" = arrow,
      "parameters" = parameters
    )
  )
}

#' volcano plot Plotly function to add volcano plot arrow annotation to specfic point
#'
#' @param xCoordinate - numeric - indicates the x coordinate to use for the annotation
#' @param  yCoordinate - numeric -indicates the y coordinate to use for the annotation
#' @param  text - text to use for arrow annotation
#' @return plotly annotation object
#'
#'
#' @export
getVolcanoArrowAnnotation <- function(xCoordinate,yCoordinate,text) {

  return(
    list(
      list(
        x = xCoordinate,
        y = yCoordinate,
        text = text,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 1,
        startarrowhead = 1,
        arrowside = "end",
        arrowcolor = "#e74c3c",
        ax = 20,
        ay = -40,
        font = list(color = 'Black',
                    family = 'Arial',
                    size = 16),
        bgcolor = "#abb2b9",
        standoff = 4
      )
    )
  )
}

#' Function to add 3 group significance group label to fold change dataframe
#'
#' @param .data fold change dataframe 
#' @param foldChangeVar name of fold change column
#' @param significanceVariable name of significance variable column (p-value, q-value, etc)
#' @param adjustedInd logical - indicating whether or not a multiple hypothesis correction was applied. When TRUE, "q" will is used. When FALSE "p" is used. 
#' @param significanceThreshold numeric - value indicating threshold at which an observation in the significanceVariable coulumn is considered statistically significant. 
#' @param originalSignificanceThreshold numeric - value indicating un-transformed threshold at which an observation in the significanceVariable coulumn is considered statistically significant. 
#' @return returns labels and colors for 3 signficance groups (up, down, not significant)
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tidyr replace_na
#' @export
addSignificanceGroup <- function(
  .data, 
  foldChangeVar,
  significanceVariable,
  adjustedInd,
  significanceThreshold,
  originalSignificanceThreshold
) {

  foldChangeVar <- rlang::enquo(foldChangeVar)
  significanceVariable <- rlang::enquo(significanceVariable)

  significanceLetter <- ifelse(adjustedInd,"q","p")

  .data |>
    dplyr::mutate(
      significanceGroup = dplyr::case_when(
        (!!significanceVariable > significanceThreshold & !!foldChangeVar < -0) ~ glue::glue("down ({significanceLetter} < {originalSignificanceThreshold})"), 
        (!!significanceVariable > significanceThreshold & !!foldChangeVar >= 0) ~ glue::glue("up ({significanceLetter} < {originalSignificanceThreshold})"), 
        TRUE ~ "not significant"), 
      color = dplyr::case_when(
        grepl("down",significanceGroup) ~ "#3E99CD", 
        grepl("up",significanceGroup) ~ "#1D4D7C", 
        grepl("not",significanceGroup) ~ "#686868")
      ) |>
    tidyr::replace_na(list(significanceGroup = "not significant"))

}

#' gets maximum abs value from dataframe column
#'
#' @param .data dataframe
#' @param .var numeric column name to calcualte max abs value
#' @param inf.rm logical indicating whether to remove INF values
#' @param buffer numeric value to add buffer or padding to max abs value
#' @return maximum abs value for column within dataframe
#' @importFrom rlang enquo
#' @import dplyr
#' @export
getMaxAbsValue <- function(
  .data,
  .var,
  inf.rm = TRUE,
  buffer = 1.1
) {

  .var <- rlang::enquo(.var)

  dataframe  <- .data |>
    dplyr::ungroup() |>
    dplyr::select(!!.var)

  if(inf.rm) {

    dataframe  <- dataframe |>
      dplyr::filter(!!.var != Inf)

  }

  maxAbsValue <- dataframe |>
    dplyr::summarise(
      max = max(abs(!!.var)) * buffer
    ) |>
    dplyr::pull()

  return(maxAbsValue)

}