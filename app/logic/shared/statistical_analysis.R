box::use(
  fgsea[fgseaMultilevel],
  dplyr[select, filter, mutate, arrange, inner_join, group_by, summarize, summarise, ungroup,
    rowwise, pull, rename, distinct, n, n_distinct, add_count, summarise_at, vars,
    first, nth, row_number, top_n, case_when],
  rlang[enquo, quo_name, `:=`, `!!!`, sym, enquos],
  stats[lm, p.adjust],
  broom[tidy],
  tidyr[pivot_longer, pivot_wider, separate_rows, nest, unnest],
  purrr[map, map_df],
  tibble[tibble, as_tibble],
  stringr[str_detect],
  glue[glue, glue_collapse]
)

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
) {

  id <- enquo(id)
  key <- enquo(key)
  response <- enquo(response)
  independentVariable <- enquo(independentVariable)

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
      summarizeByGroup(
        !!response,
        !!key,
        !!independentVariable,
        na.rm = TRUE
      ) |>
      calculateFoldChangeByKeyGroup(
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

    finalData <- inner_join(
      foldChange,
      statsData,
      by = quo_name(key)
    )

  }

  return(finalData)

}

#' @export
getGroupedStatTestByKeyGroup <- function(
  .data,
  groupVar,
  ...
) {

  groupVar <- enquo(groupVar)

  groups <- .data |>
    select(!!groupVar) |>
    distinct() |>
    pull()

  return(
    .data |>
      select(!!groupVar) |>
      distinct() |>
      pull() |>
      map_df(function(x) {
        .data |>
          filter(!!groupVar == x) |>
          getStatTestByKeyGroup(...) |>
          mutate(`:=`(!!groupVar, x)) |>
          ungroup() |>
          select(!!groupVar, p.value)
      })
  )

}

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

  id <- enquo(id)
  key <- enquo(key)
  response <- enquo(response)
  independentVariable <- enquo(independentVariable)

  if (!is.null(covariates)) {
    modelCovariates <- .data |>
      select(!!key, !!id, !!response, !!covariates) |>
      group_by(!!key) |>
      summarise_at(
        vars(!!covariates),
        n_distinct
      ) |>
      pivot_longer(!!covariates) |>
      mutate(KeepVar = ifelse(value >= 2, 1, 0)) |>
      filter(KeepVar == 1) |>
      select(name) |>
      distinct() |>
      pull()
  } else {
    modelCovariates <- NULL
  }

  independentVariableClass <- .data |>
    select(!!independentVariable) |>
    pull() |>
    class()

  independentVars <- as.list(
    c(
      quo_name(independentVariable),
      modelCovariates
    )
  )

  ivs <- paste(
    map(independentVars, quo_name),
    collapse = " + "
  )

  lmformula <- paste(quo_name(response), " ~ ", ivs)

  linearModelData <- .data |>
    select(
      !!key,
      !!id,
      !!response,
      !!!independentVars
    ) |>
    nest(
      data = c(!!id, !!response, !!!independentVars)
    ) |>
    mutate(
      fit = map(data, ~lm(lmformula, data = .x)),
      tidied = map(fit, tidy)
    ) |>
    unnest(tidied) |>
    select(
      !!key,
      term,
      estimate,
      p.value
    ) |>
    group_by(!!key) |>
    summarize(
      log2_denom = first(estimate),
      log2_num = nth(estimate, n = 2) + log2_denom,
      log2FoldChange = nth(estimate, n = 2),
      FoldChange = 2^log2FoldChange,
      p.value.original = nth(p.value, n = 2)
    ) |>
    arrange(p.value.original) |>
    ungroup() |>
    mutate(
      p.value = p.adjust(p.value.original,
      method = getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod),
      n = length(p.value.original))
    ) |>
    mutate(
      p.value.adjustment.method = adjustmentMethod,
      `-log10pvalue` = -log10(p.value),
      lmFormula = lmformula, ivs = ivs
    )

  if (independentVariableClass %in% c("factor", "character")) {

    independentVariableLevels <- levels(
      .data[[quo_name(independentVariable)]]
    )

    linearModelData <- linearModelData |>
      rename(
        `:=`(
          !!quo_name(independentVariableLevels[1]),
          log2_denom
        ),
        `:=`(
          !!quo_name(independentVariableLevels[2]),
          log2_num
        )
        )

  } else {

    linearModelData <- linearModelData |>
      select(-log2_denom) |>
      rename(
        `:=`(
          !!quo_name(independentVariable),
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

  id <- enquo(id)
  key <- enquo(key)
  response <- enquo(response)
  independentVariable <- enquo(independentVariable)
  interactionVariable <- enquo(interactionVariable)

  if (!is.null(covariates)) {

    modelCovariates <- .data |>
      select(!!key, !!id, !!response, !!covariates) |>
      group_by(!!key) |>
      summarise_at(
        vars(!!covariates),
        n_distinct
      ) |>
      pivot_longer(!!covariates) |>
      mutate(KeepVar = ifelse(value >= 2, 1, 0)) |>
      filter(KeepVar == 1) |>
      select(name) |>
      distinct() |>
      pull()
    } else {
    modelCovariates <- NULL
  }

  independentVariableClass <- .data |>
    select(!!independentVariable) |>
    pull() |>
    class()

  independentVars <- as.list(
    c(
      quo_name(independentVariable),
      modelCovariates
    )
  )

  addInteractionTerm <- .data |>
    select(!!interactionVariable) |>
    summarise(
      n = n_distinct(!!interactionVariable)
    ) |>
    mutate(
      AddInteraction = ifelse(
        n >= 2,
        TRUE,
        FALSE
      )
    ) |>
    pull(AddInteraction)

  if (addInteractionTerm) {

    ivs <- glue_collapse(
      map(
        c(
          independentVars,
          quo_name(interactionVariable)
        ),
        quo_name
      ),
      sep = " + "
    )

    interactionTerm <- glue(
      "{quo_name(independentVariable)} * {quo_name(interactionVariable)}"
      )

    allVars <- glue("{ivs} + {interactionTerm}")

  } else {

    ivs <- glue_collapse(
      map(
        independentVars,
        quo_name
      )
      , sep = " + "
    )

    allVars <- ivs

  }

  lmformula <- glue("{quo_name(response)} ~ {allVars}")

  rawModelData <- .data |>
    select(
      !!key,
      !!id,
      !!response,
      !!!independentVars,
      !!interactionVariable
    ) |>
    tidyr::nest(
      data = c(!!id, !!response, !!!independentVars, !!interactionVariable)
    ) |>
    mutate(
        fit = map(data, ~stats::lm(lmformula, data = .x)),
        tidied = map(fit, tidy)
    ) |>
    unnest(tidied) |>
    select(
      !!key,
      term,
      estimate,
      std.error,
      statistic,
      p.value
    ) |>
    mutate(
      interaction.term.flag = str_detect(term, ":")
        & str_detect(term, quo_name(independentVariable))
        & str_detect(term, quo_name(interactionVariable))
    ) |>
    group_by(!!key) |>
    mutate(rank = row_number()) |>
    ungroup()

  interactionTermLocation <- rawModelData |>
    filter(interaction.term.flag == TRUE) |>
    group_by(rank) |>
    summarise(n = n()) |>
    ungroup() |>
    arrange(n) |>
    top_n(1, n) |>
    select(rank) |>
    pull()

  if (length(interactionTermLocation) == 0) {
      interactionTermLocation <- 999
  }

  linearModelData <- rawModelData |>
    group_by(!!key) |>
    summarize(
      log2_denom = first(estimate),
      log2_num = nth(estimate, n = 2) + log2_denom,
      log2FoldChange = ifelse(
        addInteractionTerm,
        nth(estimate, n = interactionTermLocation),
        nth(estimate, n = 2)
      ),
      FoldChange = 2^log2FoldChange,
      p.value.original = nth(p.value, n = 2),
      p.value.interaction = nth(p.value, n = interactionTermLocation)
    ) |>
    arrange(p.value.original) |>
    ungroup() |>
    mutate(
      p.value.original = case_when(
        is.na(p.value.interaction) ~ p.value.original,
        TRUE ~ p.value.interaction
      )
    ) |>
    select(-p.value.interaction) |>
    mutate(
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
        .data[[quo_name(independentVariable)]]
      )

      linearModelData <- linearModelData |>
      rename(
        `:=`(!!quo_name(independentVariableLevels[1]), log2_denom),
        `:=`(!!quo_name(independentVariableLevels[2]), log2_num)
        )

    } else {

      linearModelData <- linearModelData |>
        select(-log2_denom) |>
        rename(`:=`(!!quo_name(independentVariable), log2_num))

    }

  return(linearModelData)

}

getPairwiseStatTestByKeyGroup <- function(
  .data,
  .id,
  .key,
  .group,
  .response,
  method,
  adjustmentMethod,
  ...
) {

  .id <- enquo(.id)
  .key <- enquo(.key)
  .group <- enquo(.group)
  .response <- enquo(.response)

  groupLabels <- .data |>
    select(!!.group) |>
    distinct() |>
    pull()

  StatResults <- .data |>
    select(!!.key, !!.group, !!.response) |>
    group_by(!!.key, !!.group) |>
    summarise(!!.response := list(!!.response)) |>
    pivot_wider(
      names_from = !!.group,
      values_from = !!.response,
      values_fill = NA
    ) |>
    rename(
      "x" = groupLabels[1],
      "y" = groupLabels[2]
    ) |>
    nest(data = c(y, x)) |>
    mutate(
      fit = map(
        data, ~ runStatMethod(
          method,
          unlist(.x$x),
          unlist(.x$y)
          )
        ),
      tidied = map(fit, tidy)
    ) |>
    unnest(tidied) |>
    select(-c(data, fit))

  if ("p.value" %in% colnames(StatResults)) {

    StatResults$p.value <- as.numeric(
      gsub(
        ".*<",
        "\\1",
        format.pval(StatResults$p.value)
      )
    )
    StatResults$p.value.original <- StatResults$p.value
    StatResults$p.value <- p.adjust(
      StatResults$p.value,
      getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod)
    )
    StatResults$p.value.adjustment.method <- adjustmentMethod
    StatResults <- StatResults |>
      mutate(`-log10pvalue` = -log10(p.value))

  } else {

    StatResults <- StatResults |>
      mutate(
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

  statMethods <- tibble(
    StatTestMethodLabel = c("Kolmogorov-Smirnov Test",
                            "Student's t-test",
                            "Wilcoxon test"
    ),
    StatTestMethodName = c("ks.test", "t.test", "wilcox.test")
  )

  methodName <- statMethods |>
    filter(
      StatTestMethodLabel == method | StatTestMethodName == method
    ) |>
    select(StatTestMethodName) |>
    pull()

  if (length(methodName) == 0) {
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

  if (length(adjustmentName) == 0) {
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

summarizeByGroup <- function(
  .data,
  .summaryVar,
  ...,
  na.rm = TRUE
) {

  .summaryVar <- enquo(.summaryVar)
  .group_vars <- enquos(...)

  .data |>
    group_by(!!!.group_vars) |>
    summarise(
      median = median(!!.summaryVar, na.rm = na.rm),
      mean = mean(!!.summaryVar, na.rm = na.rm),
      n = n()
    )
}

calculateFoldChangeByKeyGroup <- function(
  .data,
  .key,
  .group,
  .response,
  baselineGroupLabel,
  inf.rm = TRUE,
  ...
) {

  .key <- enquo(.key)
  .group <- enquo(.group)
  .response <- enquo(.response)

  groupLabels <- .data |>
    ungroup() |>
    select(!!.group) |>
    unique()

  comparisonGroupLabel <- groupLabels |>
    filter(!!.group != baselineGroupLabel) |>
    pull() |>
    as.character()

  baselineGroupLabel <- sym(baselineGroupLabel)
  comparisonGroupLabel <- sym(comparisonGroupLabel)

  foldChangeData <- .data |>
    select(!!.key, !!.group, !!.response) |>
    pivot_wider(names_from = !!.group, values_from = !!.response, values_fill = NA) |>
    mutate(
      log2FoldChange = !!comparisonGroupLabel - !!baselineGroupLabel,
      FoldChange = 2^log2FoldChange
    )

  if (inf.rm) {
      foldChangeData <- foldChangeData |>
        filter(FoldChange != Inf)
  }

  return(foldChangeData)
}

#' @export
addGroupCount <- function(.data, group, addLineBreak = TRUE) {

  group <- enquo(group)
  lineBreak <- ifelse(addLineBreak, "\n", "")

  return(
    .data |>
      add_count(!!group) |>
      mutate(
        `:=`(!!group, paste0("<b>", !!group, "</b>", lineBreak, " (n=", n, ")"))
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
      filter(ES_POS > abs(ES_NEG)) |>
      select(pathway) |>
      dplyr::inner_join(
        fgseaRes_positive,
        by = c("pathway")
      ),
      fgseaRes_pos_neg |>
      filter(ES_POS < abs(ES_NEG)) |>
      select(pathway) |>
      dplyr::inner_join(
        fgseaRes_negative,
        by = c("pathway")
      )
    ) |>
    mutate(
      padj = p.adjust(pval, method = "BH")
    ) |>
    arrange(padj, -abs(NES))

  return(fgseaRes_combined)

}

#' @export
calculate_GSEA_scores <- function(stats, pathway_data, gsea_param = 0) {

    pathway_nammed <- pathway_data |>
      select(Leading.edge.genes) |>
      mutate(id = row_number()) |>
      tidyr::separate_rows(Leading.edge.genes, sep = ",") |>
      select(-id) |>
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
      rename("Rank" = x, "ES" = y) |>
      dplyr::relocate("Gene")

    return(gsea_scores)

}
