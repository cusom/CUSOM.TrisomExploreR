#' @export
ImmuneMapComorbidityManager <- R6::R6Class(
  "ImmuneMapComorbidityManager",
  inherit = ImmuneMapFeatureAnalysisManager,
  private = list(),
  public = list(
    analysisMetadata = NULL,
    analysisChoices = NULL,
    Analysis = NULL,
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){
      super$initialize(applicationName, id, namespace_config, remoteDB, localDB)
    },
    getBaseData = function() {

      baseData <- localDB$getQuery(
          "SELECT ExperimentStudyName, LabID, record_id, Karyotype, Sex, Age, Analysis, CellType, Analyte, MeasuredValue, Measurement
            FROM sourceData
            WHERE ExperimentStudyName = ({study})
            AND (Analysis = ({analysis}) OR  Analysis = ({compoundAnalysis}))
            AND Age >= ({minAge})
            AND Age <= ({maxAge})
            AND Sex IN ({sexes*})
            AND Karyotype IN ({karyotypes*})
          ",
          tibble::tibble(
            study = self$Study,
            analysis = self$Analysis,
            compoundAnalysis = glue::glue("{self$Analysis};{glue::glue_collapse(self$CellType, sep =';')}"),
            minAge = min(self$Age),
            maxAge = max(self$Age),
            sexes = self$Sex,
            karyotypes = unlist(stringr::str_split(self$Karyotype, pattern = ';'))
          )
        ) |>
        tidyr::separate(Analysis, into = c("Analysis1","Analysis2"), sep = ";", remove = FALSE) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          CellTypeFilter = ifelse(self$Analysis == "Signaling", Analysis2, CellType)
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(CellTypeFilter %in% self$CellType) |>
        dplyr::select(-c(Analysis1,Analysis2)) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})")
        )

      conditionData <- localDB$getQuery(
        "SELECT record_id, Condition, HasCondition
          FROM ParticipantConditions
          WHERE Condition IN ({conditions*})",
        tibble::tibble(
          conditions = unlist(shinyTree::get_selected(self$Conditions))
        )
      ) |>
        dplyr::mutate(HasConditionFlag = ifelse(HasCondition == 'True', 1, ifelse(HasCondition == 'False', 0, NA))) |>
        dplyr::select(record_id, Condition, HasCondition, HasConditionFlag) |>
        dplyr::group_by(record_id) |>
        dplyr::summarise(HasAnyConditionFlag = sum(HasConditionFlag), .groups = 'drop') |>
        dplyr::mutate(HasAnyConditionFlag = ifelse(HasAnyConditionFlag > 0, "Yes", "No")) |>
        tidyr::drop_na()

      self$BaseData <- dplyr::inner_join(baseData, conditionData, by = "record_id") |>
        dplyr::mutate(HasAnyConditionFlag = forcats::fct_relevel(HasAnyConditionFlag, self$groupBaselineLabel)) |>
        dplyr::filter(!is.na(HasAnyConditionFlag))

    }

  )
)
