#' R6 Class to  manage Immune Map specific Feature Analysis
#' @description
#' subclass of FeatureAnalysisManager - overrides several class methods
#' @field analysisMetadata - tibble - defines UI analysis choice metadata
#' @field analysisChoices - tibble - values available for analysis choices on UI side
#' @field Analysis - string - chosen analysis
#' @importFrom arrow open_dataset
#' @export
ImmuneMapFeatureAnalysisManager <- R6::R6Class(
  "ImmuneMapFeatureAnalysisManager",
  inherit = FeatureAnalysisManager,
  private = list(),
  public = list(
    analysisMetadata = NULL,
    analysisChoices = NULL,
    Analysis = NULL,

    #' @description
    #' Create a new instance of a ImmuneMapFeatureAnalysisManager
    #' @param applicationName string - name of application
    #' @param id string - namespace for this instance
    #' @param namespace_config list - configurations for this namespace
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){
      super$initialize(applicationName, id, namespace_config, remoteDB, localDB)
    },

    #' @description
    #' #' helper function to get analysis choices for UI inputs based on chosen study
    #' @param study - string - chosen study
    getAnalysisChoices = function(study) {

      self$analysisMetadata <- arrow::open_dataset("Remote_Data/feature_data") |>
        dplyr::collect() |>
        dplyr::distinct(Analysis) |>
        dplyr::mutate(
          NestedSelect = ifelse(Analysis == "Cluster", FALSE, TRUE),
          MultipleSelect = ifelse(Analysis == "Cluster", TRUE, FALSE),
          SelectAll = ifelse(Analysis == "Cluster", TRUE, FALSE),
          HideCellType = ifelse(Analysis == "Cluster", TRUE, FALSE)
        )

      self$analysisChoices <- arrow::open_dataset("Remote_Data/feature_data") |>
        dplyr::filter(
          ExperimentID == self$Study
        ) |>
        dplyr::collect() |>
        dplyr::group_by(Analysis, CellType) |>
        dplyr::summarise(n = n(), .groups = "drop") |>
        dplyr::inner_join(
          self$analysisMetadata
          , by = "Analysis"
        )

    },

    #' @description
    #' helper function to get cell type choices for UI inputs based on chosen study
    #' @param study - string - chosen study
    getCellTypes = function(study) {

      self$getAnalysisChoices(study)

      choices <- self$analysisChoices |>
        tidyr::separate(Analysis, into = c("AnalysisGroup", "Analysis"), sep = ";") |>
        dplyr::mutate(
          Analysis = ifelse(is.na(Analysis), CellType, Analysis),
          SortOrder = dplyr::case_when(
            stringr::str_detect(AnalysisGroup, "Signaling") & Analysis == "Baseline" ~ 1000000000,
            TRUE ~ as.numeric(n)
          )
        ) |>
        dplyr::arrange(AnalysisGroup, desc(SortOrder))

      nested_select <- choices |>
        dplyr::select(NestedSelect) |>
        dplyr::distinct() |>
        dplyr::pull()

      multiple_select <- choices |>
        dplyr::select(MultipleSelect) |>
        dplyr::distinct() |>
        dplyr::pull()

      hide_cell_type <- choices |>
        dplyr::select(HideCellType) |>
        dplyr::distinct() |>
        dplyr::pull()

      if (nested_select) {

        choices <- choices |>
          dplyr::select(Analysis, AnalysisGroup) |>
          dplyr::distinct() |>
          dplyr::group_by(AnalysisGroup) |>
          dplyr::group_map(
            ~ purrr::set_names(stringr::str_c(.x$Analysis))
          ) |>
          purrr::set_names(
            choices |>
              dplyr::distinct(AnalysisGroup)|>
              dplyr::pull()
          )


      } else {

        choices <- choices |>
          dplyr::select(CellType) |>
          dplyr::distinct() |>
          dplyr::pull()

      }

      return(
        list(
          "nestedSelect" = nested_select,
          "MultipleSelect" = multiple_select,
          "HideCellType" = hide_cell_type,
          "choices" = choices
        )
      )
    },

    #' @description
    #' helper function to get analysis input choices
    getAnalysisInputChoices = function() {

      return(
        self$analysisChoices |>
          tidyr::separate(Analysis, into = c("Analysis", "SubAnalysis"), sep = ";") |>
          tidyr::separate(CellType, into = c("Epitope", "CellType"), sep = ";") |>
          dplyr::mutate(CellType = ifelse(is.na(CellType), Epitope, CellType)) |>
          dplyr::filter(CellType %in% self$CellType | SubAnalysis %in% self$CellType) |>
          dplyr::distinct(Analysis) |>
          dplyr::pull()
      )
    },

    #' @description
    #' helper function to get Karyotype input options based on namespace
    #' @param karyotypes string vector of karyotypes for input widget
    getKaryotypeChoices = function(karyotypes) {

      if (self$namespace == "Karyotype") {
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
            choiceValues = glue::glue_collapse(karyotypes, sep = ";")
          )
        )
      } else if (self$namespace == "Comorbidity") {
        return(
          tibble::tibble(
            choiceNames =  karyotypes[1],
            choiceValues = karyotypes[1]
          )
        )
      } else {

        karyotype_input_counts <- arrow::open_dataset("Remote_Data/feature_data") |>
          dplyr::filter(
            ExperimentID == self$Study
          ) |>
          dplyr::collect() |>
          dplyr::select(LabID, CellType, Analysis, Analyte, Karyotype) |>
          dplyr::filter(
            Analysis %in% self$Analysis,
            CellType %in% self$CellType
          ) |>
          dplyr::group_by(Analyte, Karyotype) |>
          dplyr::summarise(n = dplyr::n_distinct(LabID), .groups = "drop") |>
          dplyr::ungroup() |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarise(n = round(median(n)), .groups = "drop") |>
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


        if (self$analysisType == "Continuous") {
          return(
            karyotype_input_counts |>
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
                        data-original-title="Test for differences in {self$analysisVariable}
                        trajectories between Trisomy 21 & Controls">
                      </span>
                    </div>'
                  ),
                  choiceValues = glue::glue_collapse(karyotypes, sep = ";")
                )
              ) |>
              dplyr::arrange(sort)
          )
        } else {
          return(
            karyotype_input_counts
          )
        }
      }
    },

    #' @description
    #' Get / set sample level data with filers applied
    getBaseData = function() {

      self$BaseData <- arrow::open_dataset("Remote_Data/feature_data") |>
        dplyr::filter(
          ExperimentID == self$Study
        ) |>
        dplyr::collect() |>
        dplyr::select(LabID, Karyotype, Sex, Age, BMI, Analysis, CellType, Analyte, MeasuredValue, Measurement) |>
        dplyr::filter(
          (
            Analysis == self$Analysis |
            Analysis == glue::glue("{self$Analysis};{glue::glue_collapse(self$CellType,sep=';')}")
          ),
          Age >= min(self$Age),
          Age <= max(self$Age),
          Sex %in% self$Sex,
          Karyotype %in% unlist(stringr::str_split(self$Karyotype, pattern = ";"))
        ) |>
        tidyr::separate(Analysis, into = c("Analysis1", "Analysis2"), sep = ";", remove = FALSE) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          CellTypeFilter = ifelse(self$Analysis == "Signaling", Analysis2, CellType)
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(CellTypeFilter %in% self$CellType) |>
        dplyr::select(-c(Analysis1, Analysis2)) |>
        dplyr::filter(!is.na(!!rlang::sym(self$analysisVariable))) |>
        dplyr::mutate(
          log2MeasuredValue = ifelse(MeasuredValue == 0, 0, log2(MeasuredValue)),
          log2Measurement = glue::glue("log<sub>2</sub>({Measurement})")
        )

    },

    #' @description
    #' Set Volcano Summary Data along with other volcano plot properties
    getVolcanoSummaryData = function() {

      self$getBaseData()

      self$Adjusted <- self$AdjustmentMethod != "none"

      if (self$analysisType == "Categorical") {

        dataframe <- self$BaseData |>
          dplyr::select(LabID, Analysis, CellType, Analyte, MeasuredValue, self$analysisVariable, self$Covariates) |>
          tidyr::separate(Analysis, into = c("ParsedAnalysis1", "ParsedAnalysis2"), sep = ";", remove = FALSE) |>
          tidyr::separate(Analyte, into = c("ParsedAnalyte1", "ParsedAnalyte2"), sep = ";", remove = FALSE) |>
          dplyr::mutate(
            CompoundAnalyte = dplyr::case_when(
              Analysis == "Cluster" ~ glue::glue("{Analysis};{CellType};{Analyte}"),
              Analysis == "Lineage" ~ glue::glue("{Analysis};{CellType};{Analyte}"),
              Analysis == "Cell Frequencies" ~ glue::glue(";{CellType};{Analyte}"),
              grepl("Signaling", Analysis) ~ glue::glue("{ParsedAnalysis2};{ParsedAnalyte1};{Analyte}"),
              TRUE ~ glue::glue(";;{Analyte}")
            )
          ) |>
          dplyr::mutate_at(
            dplyr::vars(self$analysisVariable),
            ~forcats::fct_relevel(.x, self$groupBaselineLabel)
          ) |>
          CUSOMShinyHelpers::getStatTestByKeyGroup(
            id = LabID,
            key = CompoundAnalyte,
            response = MeasuredValue,
            independentVariable = !!rlang::sym(self$analysisVariable),
            baselineLabel = self$groupBaselineLabel,
            testMethod = self$StatTest,
            adjustmentMethod = self$AdjustmentMethod,
            covariates = self$Covariates
          ) |>
          tidyr::separate(
            col = CompoundAnalyte,
            into = c("Analysis", "CellType", "Analyte", "Analyte2"),
            sep = ";",
            remove = FALSE
          ) |>
          dplyr::mutate(
            Analyte = ifelse(is.na(Analyte2), Analyte, glue::glue("{Analyte};{Analyte2}")),
            Analysis = dplyr::case_when(
              Analysis == "" ~ "Lineage",
              Analysis %in% c("Baseline", "+ IFNA2A") ~ "Signaling Eiptope",
              TRUE ~ Analysis
            ),
            formattedPValue = unlist(
              purrr::pmap(
                .l = list(p.value, p.value.adjustment.method),
                CUSOMShinyHelpers::formatPValue
              )
            ),
            text = glue::glue(
              "{Analysis}: {CellType} <br />Analyte: {Analyte}<br />
              fold change: {round(FoldChange,2)}<br />{formattedPValue}"
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(-c(CompoundAnalyte, Analyte2)) |>
          dplyr::relocate(Analysis, CellType, Analyte)

      } else {

        dataframe <- self$BaseData |>
          dplyr::select(
            LabID, Analysis, CellType, Analyte, MeasuredValue, self$analysisVariable, self$Covariates, Karyotype
          ) |>
          tidyr::separate(Analysis, into = c("ParsedAnalysis1", "ParsedAnalysis2"), sep = ";", remove = FALSE) |>
          tidyr::separate(Analyte, into = c("ParsedAnalyte1", "ParsedAnalyte2"), sep = ";", remove = FALSE) |>
          dplyr::mutate(
            CompoundAnalyte = dplyr::case_when(
              Analysis == "Cluster" ~ glue::glue("{Analysis};{CellType};{Analyte}"),
              Analysis == "Lineage" ~ glue::glue("{Analysis};{CellType};{Analyte}"),
              Analysis == "Cell Frequencies" ~ glue::glue(";{CellType};{Analyte}"),
              grepl("Signaling", Analysis) ~ glue::glue("{ParsedAnalysis2};{ParsedAnalyte1};{Analyte}"),
              TRUE ~ glue::glue(";;{Analyte}")
            )
          ) |>
          dplyr::mutate(Karyotype = forcats::fct_relevel(Karyotype, "Control")) |>
          CUSOMShinyHelpers::getLinearModelWithInteraction(
            id = LabID,
            key = CompoundAnalyte,
            response = MeasuredValue,
            independentVariable = !!rlang::sym(self$analysisVariable),
            covariates = self$Covariates,
            interactionVariable = Karyotype,
            adjustmentMethod = self$AdjustmentMethod
          ) |>
          tidyr::separate(
            col = CompoundAnalyte,
            into = c("Analysis", "CellType", "Analyte", "Analyte2"),
            sep = ";",
            remove = FALSE
          ) |>
          dplyr::mutate(
            Analyte = ifelse(is.na(Analyte2), Analyte, glue::glue("{Analyte};{Analyte2}")),
            Analysis = dplyr::case_when(
              Analysis == "" ~ "Lineage",
              Analysis %in% c("Baseline", "+ IFNA2A") ~ "Signaling Eiptope",
              TRUE ~ Analysis
            ),
            formattedPValue = unlist(
              purrr::pmap(
                .l = list(p.value, p.value.adjustment.method),
                CUSOMShinyHelpers::formatPValue
              )
            ),
            text = glue::glue(
              "Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}"
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(-c(CompoundAnalyte, Analyte2)) |>
          dplyr::relocate(Analysis, CellType, Analyte)
      }

      if (nrow(dataframe) > 0) {

        self$VolcanoSummaryData <- dataframe |>
          dplyr::mutate(
            log2FoldChange = log2(FoldChange),
            `-log10pvalue` = -log10(p.value),
            `p.value.adjustment.method` = "Benjamini-Hochberg (FDR)",
            formattedPValue = unlist(
              purrr::pmap(
                .l = list(p.value, p.value.adjustment.method),
                CUSOMShinyHelpers::formatPValue
              )
            ),
            text = glue::glue(
              "Analyte: {Analyte}<br />fold change: {round(FoldChange,2)}<br />{formattedPValue}"
            )
          )

        self$VolcanoPlotTitle <- glue::glue("Effect of {self$analysisVariableLabel} on all {self$analytesLabel}")
        self$VolcanoSummaryMaxFoldChange <- max(abs(self$VolcanoSummaryData$log2FoldChange))
        self$VolcanoSummaryDataXAxisLabel <- "log<sub>2</sub>(Fold Change)"
        self$VolcanoSummaryDataYAxisLabel <- glue::glue(
          "-log<sub>10</sub>({ifelse(self$Adjusted,\"q-value \",\"p-value \")})"
        )

      }
    }
  )
)
