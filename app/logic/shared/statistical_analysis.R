box::use(
  fgsea[fgseaMultilevel],
  dplyr[select, filter, mutate, arrange, inner_join, group_by, summarise, ungroup, rowwise, pull, rename, distinct, n_distinct, add_count],
  rlang[enquo, quo_name, `:=`, `!!!`],
  stats[lm, p.adjust],
  broom[tidy],
  tidyr[pivot_longer, pivot_wider, separate_rows, nest, unnest],
  purrr[map],
  tibble[tibble, as_tibble],
)

#' Calculate statistical test between 2 groups for each key value in a dataframe
#'
#' @param .data A dataframe
#' @param id A string or numeric column - represents a unique
#' observation within each key/group combination. PersonId, LabId, etc
#' @param key A string or numeric column - key value for dataframe.
#' Statistics will be computed between groups for each key value
#' @param response A numeric column - numerical value to use
#' with statitical test between groups.
#' @param independentVariable A string column indicating group membership - should be binary.
#' @param baselineLabel independentVariable label used for fold change comparison / calculation
#' @param testMethod a string - indicating which statisical
#' test to perform. One of either ks.test, t.test, or wilcox.test
#' @param ... dots - accomodate additional arguments
#' function is called as parent router
#' @return dataframe containing resulting p.values for each key/group stat test
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @import dplyr
#' @export
getStatTestByKeyGroup <- function(
  .data,
  id,
  key,
  response,
  independentVariable,
  baselineLabel,
  testMethod,
  ...
)
{

  id <- rlang::enquo(id)
  key <- rlang::enquo(key)
  response <- rlang::enquo(response)
  independentVariable <- rlang::enquo(independentVariable)

  if (testMethod == "Linear Model") {
    finalData <- .data |>
      getLinearModel(
        !!id,
        !!key,
        !!response,
        !!independentVariable,
        ...
      )
  } else {

    foldChange <- .data |>
      CUSOMShinyHelpers::summarizeByGroup(
        !!response,
        !!key,
        !!independentVariable,
        na.rm = TRUE
      ) |>
      CUSOMShinyHelpers::calculateFoldChangeByKeyGroup(
        !!key,
        !!independentVariable,
        median,
        baselineLabel,
        inf.rm = TRUE
      )

    statsData <- .data |>
      getPairwiseStatTestByKeyGroup(
        !!id,
        !!key,
        !!independentVariable,
        !!response,
        method = testMethod,
        ...
      )

    finalData <- dplyr::inner_join(
      foldChange,
      statsData,
      by = rlang::quo_name(key)
    )

  }

  return(finalData)

}

#' run linear model
#'
#' @param .data data for model
#' @param id id column - a linear model will be run for each id value in .data
#' @param key a unique identifier per id
#' @param response a numeric column to be used as response variable in linear model
#' @param independentVariable indepdendent variable to be used in linear model
#' @param covariates list of covariate columns to be included in linear model
#' @param adjustmentMethod p.value adjustment method to be applied
#' @param ... dots - accomodate additional arguments
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @import dplyr
getLinearModel <- function(
  .data,
  id,
  key,
  response,
  independentVariable,
  covariates,
  adjustmentMethod,
  ...
) {

  id <- rlang::enquo(id)
  key <- rlang::enquo(key)
  response <- rlang::enquo(response)
  independentVariable <- rlang::enquo(independentVariable)

  if (!is.null(covariates)) {
    modelCovariates <- .data |>
      dplyr::select(!!key, !!id, !!response, !!covariates) |>
      dplyr::group_by(!!key) |>
      dplyr::summarise_at(
        dplyr::vars(!!covariates),
        dplyr::n_distinct
      ) |>
      tidyr::pivot_longer(!!covariates) |>
      dplyr::mutate(KeepVar = ifelse(value >= 2, 1, 0)) |>
      dplyr::filter(KeepVar == 1) |>
      dplyr::select(name) |>
      dplyr::distinct() |>
      dplyr::pull()
  } else {
    modelCovariates <- NULL
  }

  independentVariableClass <- .data |>
    dplyr::select(!!independentVariable) |>
    dplyr::pull() |>
    class()

  independentVars <- as.list(
    c(
      rlang::quo_name(independentVariable),
      modelCovariates
    )
  )

  ivs <- paste(
    purrr::map(independentVars, rlang::quo_name),
    collapse = " + "
  )

  lmformula <- paste(rlang::quo_name(response), " ~ ", ivs)

  linearModelData <- .data |>
    dplyr::select(
      !!key,
      !!id,
      !!response,
      !!!independentVars
    ) |>
    tidyr::nest(
      data = c(!!id, !!response, !!!independentVars)
    ) |>
    dplyr::mutate(
      fit = purrr::map(data, ~stats::lm(lmformula, data = .x)),
      tidied = purrr::map(fit, broom::tidy)
    ) |>
    tidyr::unnest(tidied) |>
    dplyr::select(
      !!key,
      term,
      estimate,
      p.value
    ) |>
    dplyr::group_by(!!key) |>
    dplyr::summarize(
      log2_denom = dplyr::first(estimate),
      log2_num = dplyr::nth(estimate, n = 2) + log2_denom,
      log2FoldChange = dplyr::nth(estimate, n = 2),
      FoldChange = 2^log2FoldChange,
      p.value.original = dplyr::nth(p.value, n = 2)
    ) |>
    dplyr::arrange(p.value.original) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      p.value = stats::p.adjust(p.value.original,
      method = getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod),
      n = length(p.value.original))
    ) |>
    dplyr::mutate(p.value.adjustment.method = adjustmentMethod) |>
    dplyr::mutate(`-log10pvalue` = -log10(p.value)) |>
    dplyr::mutate(lmFormula = lmformula, ivs = ivs)

  if(independentVariableClass %in% c("factor", "character")) {

    independentVariableLevels <- levels(
      .data[[rlang::quo_name(independentVariable)]]
    )

    linearModelData <- linearModelData |>
      dplyr::rename(
        `:=`(
          !!rlang::quo_name(independentVariableLevels[1]),
          log2_denom
        ),
        `:=`(
          !!rlang::quo_name(independentVariableLevels[2]),
          log2_num
        )
        )

  } else {

    linearModelData <- linearModelData |>
      dplyr::select(-log2_denom) |>
      dplyr::rename(
        `:=`(
          !!rlang::quo_name(independentVariable),
          log2_num
        )
      )
  }

  return(linearModelData)

}

#' @export
getLinearModelWithInteraction <- function(
  .data,
  id,
  key,
  response,
  independentVariable,
  covariates,
  interactionVariable,
  adjustmentMethod = "none",
  ...
) {

  id <- rlang::enquo(id)
  key <- rlang::enquo(key)
  response <- rlang::enquo(response)
  independentVariable <- rlang::enquo(independentVariable)
  interactionVariable <- rlang::enquo(interactionVariable)

  if (!is.null(covariates)) {

    modelCovariates <- .data |>
      dplyr::select(!!key, !!id, !!response, !!covariates) |>
      dplyr::group_by(!!key) |>
      dplyr::summarise_at(
        dplyr::vars(!!covariates),
        dplyr::n_distinct
      ) |>
      tidyr::pivot_longer(!!covariates) |>
      dplyr::mutate(KeepVar = ifelse(value >= 2, 1, 0)) |>
      dplyr::filter(KeepVar == 1) |>
      dplyr::select(name) |>
      dplyr::distinct() |>
      dplyr::pull()
    } else {
    modelCovariates <- NULL
  }

  independentVariableClass <- .data |>
    dplyr::select(!!independentVariable) |>
    dplyr::pull() |>
    class()

  independentVars <- as.list(
    c(
      rlang::quo_name(independentVariable),
      modelCovariates
    )
  )

  addInteractionTerm <- .data |>
    dplyr::select(!!interactionVariable) |>
    dplyr::summarise(
      n = dplyr::n_distinct(!!interactionVariable)
    ) |>
    dplyr::mutate(
      AddInteraction = ifelse(
        n >= 2,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::pull(AddInteraction)

  if (addInteractionTerm) {

    ivs =  glue::glue_collapse(
      purrr::map(
        c(
          independentVars,
          rlang::quo_name(interactionVariable)
        ),
        rlang::quo_name
      ),
      sep = " + "
    )

    interactionTerm <- glue::glue(
      "{rlang::quo_name(independentVariable)} * {rlang::quo_name(interactionVariable)}"
      )

    allVars <- glue::glue("{ivs} + {interactionTerm}")

  } else {

    ivs <- glue::glue_collapse(
      purrr::map(
        independentVars,
        rlang::quo_name
      )
      , sep = " + "
    )

    allVars <- ivs

  }

  lmformula = glue::glue("{rlang::quo_name(response)} ~ {allVars}")

  rawModelData <- .data |>
    dplyr::select(
      !!key, 
      !!id, 
      !!response, 
      !!!independentVars, 
      !!interactionVariable
    ) |>
    tidyr::nest(
      data = c(!!id, !!response, !!!independentVars, !!interactionVariable)
    ) |>
    dplyr::mutate(
        fit = purrr::map(data, ~stats::lm(lmformula, data = .x)),
        tidied = purrr::map(fit, broom::tidy)
    ) |>
    tidyr::unnest(tidied) |>
    dplyr::select(
      !!key,
      term,
      estimate,
      std.error,
      statistic,
      p.value
    ) |>
    dplyr::mutate(
      interaction.term.flag = stringr::str_detect(term, ":")
        & stringr::str_detect(term, rlang::quo_name(independentVariable))
        & stringr::str_detect(term, rlang::quo_name(interactionVariable))
    ) |>
    dplyr::group_by(!!key) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup()

  interactionTermLocation <- rawModelData |>
    dplyr::filter(interaction.term.flag == TRUE) |>
    dplyr::group_by(rank) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(n) |>
    dplyr::top_n(1, n) |>
    dplyr::select(rank) |>
    dplyr::pull()

  if (length(interactionTermLocation) == 0) {
      interactionTermLocation <- 999
  }

  linearModelData <- rawModelData |>
    dplyr::group_by(!!key) |>
    dplyr::summarize(
      log2_denom = dplyr::first(estimate),
      log2_num = dplyr::nth(estimate, n = 2) + log2_denom,
      log2FoldChange = ifelse(
        addInteractionTerm,
        dplyr::nth(estimate, n = interactionTermLocation),
        dplyr::nth(estimate, n = 2)
      ),
      FoldChange = 2^log2FoldChange,
      p.value.original = dplyr::nth(p.value, n = 2),
      p.value.interaction = dplyr::nth(p.value, n = interactionTermLocation)
    ) |>
    dplyr::arrange(p.value.original) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      p.value.original = dplyr::case_when(
        is.na(p.value.interaction) ~ p.value.original,
        TRUE ~ p.value.interaction
      )
    ) |>
    dplyr::select(-p.value.interaction) |>
    dplyr::mutate(
      p.value = stats::p.adjust(
        p.value.original,
        method = getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod),
        n = length(p.value.original)
      ),
      `-log10pvalue` = -log10(p.value),
      p.value.adjustment.method = adjustmentMethod,
      lmFormula = lmformula,
      ivs = ivs
    )

    if (independentVariableClass %in% c("factor", "character")) {

      independentVariableLevels <- levels(
        .data[[rlang::quo_name(independentVariable)]]
      )

      linearModelData <- linearModelData |>
      dplyr::rename(
        `:=`(!!rlang::quo_name(independentVariableLevels[1]), log2_denom),
        `:=`(!!rlang::quo_name(independentVariableLevels[2]), log2_num)
        )

    }

    else {

      linearModelData <- linearModelData |>
        dplyr::select(-log2_denom) |>
        dplyr::rename(`:=`(!!rlang::quo_name(independentVariable), log2_num))

    }

  return(linearModelData)

}

getPairwiseStatTestByKeyGroup <- function (
  .data,
  .id,
  .key,
  .group,
  .response,
  method,
  adjustmentMethod,
  ...
) {

  .id <- rlang::enquo(.id)
  .key <- rlang::enquo(.key)
  .group <- rlang::enquo(.group)
  .response <- rlang::enquo(.response)

  groupLabels <- .data |>
    dplyr::select(!!.group) |>
    dplyr::distinct() |>
    dplyr::pull()
  
  StatResults <- .data |>
    dplyr::select(!!.key, !!.group, !!.response) |>
    dplyr::group_by(!!.key, !!.group) |>
    dplyr::summarise(!!.response := list(!!.response)) |>
    tidyr::pivot_wider(
      names_from = !!.group,
      values_from = !!.response,
      values_fill = NA
    ) |>
    dplyr::rename(
      "x" = groupLabels[1],
      "y" = groupLabels[2]
    ) |>
    tidyr::nest(data = c(y, x)) |>
    dplyr::mutate(
      fit = purrr::map(
        data, ~ runStatMethod(
          method,
          unlist(.x$x),
          unlist(.x$y)
          )
        ),
      tidied = purrr::map(fit, broom::tidy)
    ) |>
    tidyr::unnest(tidied) |>
    dplyr::select(-c(data, fit))


  if ("p.value" %in% colnames(StatResults)) {

    StatResults$p.value <- as.numeric(
      gsub(
        ".*<",
        "\\1",
        format.pval(StatResults$p.value)
      )
    )
    StatResults$p.value.original <- StatResults$p.value
    StatResults$p.value <- stats::p.adjust(
      StatResults$p.value,
      getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod)
    )
    StatResults$p.value.adjustment.method <- adjustmentMethod
    StatResults <- StatResults |>
      dplyr::mutate(`-log10pvalue` = -log10(p.value))

  } else {

    StatResults <- StatResults |>
      dplyr::mutate(
        statistic = NA,
        p.value = NA,
        `-log10pvalue` = 0,
         method = method,
         error = paste0("Not Enough Observations to run ", method)
      )
  }

  return(StatResults)

}

runStatMethod <- function(method, x, y) {

  methodName <- getStatTestByKeyGroup.getMethodName(method)

  tryCatch({

    result <- do.call(methodName, args = list(x, y))

    return(result)


  }, error = function(err) {

    return(NA)

  })

}

#' Return implemented stat test methods
#' @return vector of all implemented stat test methods
#' @export
getStatTestByKeyGroup.methods <- c("Linear Model",
  "Kolmogorov-Smirnov Test",
  "Student's t-test",
  "Wilcoxon test"
  )



# internal lookup between label and method name for all stat tests
getStatTestByKeyGroup.getMethodName <- function(method) {

  statMethods <- tibble::tibble(
    StatTestMethodLabel = c("Kolmogorov-Smirnov Test",
                            "Student's t-test",
                            "Wilcoxon test"
    ),
    StatTestMethodName = c("ks.test", "t.test", "wilcox.test")
  )

  methodName <- statMethods |>
    dplyr::filter(
      StatTestMethodLabel == method | StatTestMethodName == method
    ) |>
    dplyr::select(StatTestMethodName) |>
    dplyr::pull()

  if(length(methodName) == 0) {
    msg <- paste0("'", method, "' Method Not Yet Implemented")
    stop(msg, call. = FALSE)
  } else {
    return(methodName)
  }

}

# internal lookup between label and method name for all adjustment methods
getStatTestByKeyGroup.getAdjustmentMethodName <- function(adjustment) {

  adjustmentMethods <- data.frame(
    AdjustmentMethodLabel = c("None", "Bonferroni", "Benjamini-Hochberg (FDR)"),
    AdjustmentMethodName = c("none", "bonferroni", "BH")
  )

  adjustmentName <- as.character(
    adjustmentMethods[which(
      adjustmentMethods$AdjustmentMethodLabel == adjustment
      ),
    "AdjustmentMethodName"]
    )

  if(length(adjustmentName) == 0) {
    return(adjustment)
  } else {
    return(adjustmentName)
  }

}

#' @export
formatPValue <- function(
  significanceVariable,
  adjustmentMethod = "none",
  formatInsignificantValues = FALSE
  ) {

  if (!is.na(significanceVariable)) {
    adjustedInd <- adjustmentMethod != "none"
    threshold <- ifelse(adjustedInd, 0.1, 0.05)
    if (significanceVariable <= threshold || formatInsignificantValues) {
      formattedValue <- formatC(significanceVariable, format = "e", digits = 2)
      prefix <- ifelse(adjustedInd, "q", "p")
      return(
        glue::glue("{prefix}-value = {formattedValue}")
      )
    } else {
      return("No significant difference")
    }
  } else {    
    return("Unable to compute using chosen methods")
  }
}

#' @export
addGroupCount <- function(.data, group, addLineBreak = TRUE) {

  group <- rlang::enquo(group)
  lineBreak <- ifelse(addLineBreak,"\n","")

  return(
    .data |>
      dplyr::add_count(!!group)|>
      dplyr::mutate(
        `:=`(!!group, paste0("<b>",!!group,"</b>",lineBreak," (n=",n,")"))
      )
    )
}

#' @export
runfGSEA <- function(geneset, ranks, min_size = 15, max_size = 500, gsea_param = 0, eps = 0.0) {

  # Run positive enrichment
  fgseaRes_positive <- fgsea::fgseaMultilevel(
    pathways = geneset,
    stats = ranks,
    minSize = min_size,
    maxSize = max_size,
    gseaParam = gsea_param,
    eps = eps,
    scoreType = "pos"
  )
  # Run negative enrichment
  fgseaRes_negative <- fgsea::fgseaMultilevel(
    pathways = geneset,
    stats = ranks,
    minSize = min_size,
    maxSize = max_size,
    gseaParam = gsea_param,
    eps = eps,
    scoreType = "neg"
  )

  # Combine positive and negative results + re-adjust pvals
  fgseaRes_pos_neg <- dplyr::inner_join(
    fgseaRes_positive |>
      tibble::as_tibble(),
    fgseaRes_negative |>
      tibble::as_tibble(),
    by = c("pathway"),
    suffix = c("_POS", "_NEG")
  )

  fgseaRes_combined <- dplyr::bind_rows(
      fgseaRes_pos_neg |>
      dplyr::filter(ES_POS > abs(ES_NEG)) |>
      dplyr::select(pathway) |>
      dplyr::inner_join(
        fgseaRes_positive,
        by = c("pathway")
      ),
      fgseaRes_pos_neg |>
      dplyr::filter(ES_POS < abs(ES_NEG)) |>
      dplyr::select(pathway) |>
      dplyr::inner_join(
        fgseaRes_negative,
        by = c("pathway")
      )
    ) |>
    dplyr::mutate(
      padj = stats::p.adjust(pval, method = "BH")
    ) |>
    dplyr::arrange(padj, -abs(NES))

  return(fgseaRes_combined)

}

#' @export
calculate_GSEA_scores <- function(stats, pathway_data, gsea_param = 0) {

    pathway_nammed <- pathway_data |>
      dplyr::select(Leading.edge.genes) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      tidyr::separate_rows(Leading.edge.genes, sep = ",") |>
      dplyr::select(-id) |>
      purrr::simplify()

    rnk <- rank(-stats)
    ord <- order(rnk)
    stats_adj <- stats[ord]
    stats_adj <- sign(stats_adj) * (abs(stats_adj)^gsea_param)
    stats_adj <- stats_adj / max(abs(stats_adj))

    pathway <- unname(as.vector(stats::na.omit(match(pathway_nammed, names(stats_adj)))))
    pathway <- sort(pathway)

    gsea_result <- fgsea::calcGseaStat(
      stats_adj,
      selectedStats = pathway,
      returnAllExtremes = TRUE
    )

    bottoms <- gsea_result$bottoms
      tops <- gsea_result$tops
      n <- length(stats_adj)
      xs <- as.vector(rbind(pathway - 1, pathway))
      ys <- as.vector(rbind(bottoms, tops))

    gsea_scores <- tibble::tibble(
        x = c(0, xs, n + 1),
        y = c(0, ys, 0)
      ) |>
      dplyr::inner_join(
        tibble::tibble(x = pathway, Gene = pathway_nammed),
        by = "x"
      ) |>
      dplyr::rename("Rank" = x, "ES" = y) |>
      dplyr::relocate("Gene")

    return(gsea_scores)

}
