#' R6 Class to manage Clincal Data Analysis
#' @description
#' R6 Class to manage all data analysis within various modules in the Clinical Data application
#' @field applicationName - string - application name
#' @field namespace - string - namespace for this instance
#' @field remoteDB - R6 Class to manage remote database queries
#' @field localDB - R6 Class to manange local database queries
#' @field ParticipantEncounter - deprecated
#' @field AllParticipants - deprecated
#' @field AllN - numeric - count of all Participants
#' @field Enrollments - tibble of counts of participants by Karyotype
#' @field SampleDetail - tibble of biospecimen detail
#' @field biospecimen_tree - biospecimens in hierarchy / tree stucture
#' @field Biospecimens - selected biospecimen values from UI
#' @field PlatformExperiments - tibble of platform-experiment combinations
#' @field ParticipantPlatformExperiment - tibble of participants, platfoms, and experiments
#' @field AnalysisAvailable - nested list of experiments / studies by platform group
#' @field Karyotypes - character vector - selected karotypes
#' @field Sex - character vector - selected sexes
#' @field Age - numeric vector - selected ages
#' @field OmicsSamples - character vector - selected omics
#' @field Samples - character vector - selected samples
#' @field ParticipantData - tibble of participant level data filtered from inputs
#' @field ConditionClassData - tibble of condition class data
#' @field conditionClasses - character vector - selected condition classes
#' @field conditions - character vector - selected conditions
#' @field conditionClassCounts - tibble - counts of condition classes by Karyotype
#' @field conditionClassSummary - tibble - summary counts of chosen condition classes by Karyotype
#' @field conditionCounts - tibble - counts of conditions by Karyotype
#' @field conditionSummary - tibble - summary counts of conditions by Karyotype
#' @field ConditionClassSexCounts - tibble - counts of condition classes by Sex and Karyotype
#' @field ConditionSexCounts - tibble - counts of conditions by Sex and Karyotype
#' @field selectedAnnotationLevel - string - chosen annotation level for conditions - Condition Class vs Condition
#' @field SelectedConditions - character vector - chosen condition classes or conditions
#' @field upsetPlotData - tibble - wide-form matrix / data frame of condition counts vs. records
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import plotly
#' @import leaflet
#' @importFrom tigris geo_join
#' @importFrom arrow open_dataset
#' @export
ClinicalDataAnalysisManager <- R6::R6Class(
  "ClinicalDataAnalysisManager",
  private = list(),
  public = list(
    applicationName = NULL,
    namespace = NULL,
    remoteDB = NULL,
    localDB = NULL,

    ParticipantEncounter = NULL,
    AllParticipants = NULL,
    AllN = NULL,
    Enrollments = NULL,
    SampleDetail = NULL,
    biospecimen_tree = NULL,
    Biospecimens = NULL,
    PlatformExperiments = NULL,
    ParticipantPlatformExperiment = NULL,
    AnalysisAvailable = NULL,
    Karyotypes = NULL,
    Sex = NULL,
    Age = NULL,
    OmicsSamples = NULL,
    Samples = NULL,

    ParticipantData = NULL,

    ConditionClassData = NULL,
    conditionClasses = NULL,
    conditions = NULL,
    ## Tables
    conditionClassCounts = NULL,
    conditionClassSummary = NULL,
    conditionCounts = NULL,
    conditionSummary = NULL,
    ConditionClassSexCounts = NULL,
    ConditionSexCounts = NULL,

    selectedAnnotationLevel = NULL,
    SelectedConditions = NULL,
    upsetPlotData = NULL,

    #' @description
    #' Create a new instance of a ClinicalDataAnalysisManager object
    #' @param applicationName string - applicationName
    #' @param id string - namespace for this class
    #' @param namespace_config tibble - configuration values for this namespace instance of the object
    #' @param remoteDB R6 class to manage remote database queries
    #' @param localDB R6 class to manange local database queries
    #' @param data additional data passed from app manager to be used for inputs and data filtering
    #' @return A new `ClinicalDataAnalysisManager` object.
    initialize = function(applicationName, id, namespace_config, remoteDB, localDB, data) {

      self$applicationName <- applicationName
      self$remoteDB <- remoteDB

      self$Enrollments <- data$Enrollments

      self$AllN <- sum(data$Enrollments$ParticipantCount)

      self$ConditionClassData <- arrow::open_dataset("Remote_Data/participant_conditions") |>
        dplyr::collect() |>
        tidyr::separate_rows(ConditionClass, sep = ";", convert = TRUE) |>
        dplyr::filter(!is.na(Condition))

      self$conditionClassCounts <- dplyr::inner_join(
        self$ConditionClassData |>
          dplyr::filter(HasCondition == "True") |>
          dplyr::group_by(Karyotype, ConditionClass) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop"),
        self$ConditionClassData |>
          dplyr::group_by(Karyotype, ConditionClass) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop"),
          by = c("ConditionClass", "Karyotype")
        ) |>
        dplyr::mutate(Frequency = `n.x` / `n.y`) |>
        dplyr::select(Karyotype, ConditionClass, Frequency) |>
        dplyr::mutate(
          Frequency = round(Frequency, 4) * 100,
          Karyotype = dplyr::case_when(
            Karyotype == "Control" ~ "Control %",
            TRUE ~ "Trisomy 21 %"
          )
        ) |>
        tidyr::pivot_wider(names_from = Karyotype, values_from = Frequency) |>
        dplyr::filter(!is.na(ConditionClass)) |>
        dplyr::rename("Class of Condition" = ConditionClass)

      self$updateSummaryConditionClassCounts()

      self$SampleDetail <- data$SampleDetail

      self$PlatformExperiments <- data$PlatformExperiments

      self$ParticipantPlatformExperiment <- arrow::open_dataset("Remote_Data/participant_platform_experiment") |>
        dplyr::collect()

    },

    #' @description
    #' get biospecimen hierarchy for input
    getBiospecimenSampleHierarchy = function() {

      self$biospecimen_tree <-  self$SampleDetail |>
        dplyr::filter(
          !is.na(samplePath)
        ) |>
        dplyr::distinct(samplePath) |>
        dplyr::mutate(
          pathString = ifelse(is.na(samplePath), sample_type, samplePath),
          pathString = glue::glue("AllSamples/{pathString}")
        ) |>
        dplyr::select(pathString) |>
        dplyr::distinct() |>
        data.tree::as.Node()

      self$biospecimen_tree$name <- NULL

      return(self$biospecimen_tree)

    },

    #' @description
    #' get aggregated HTML string of chosen biospecimens with line breaks per condition
    getSelectedBiospecimenList = function() {

      return(
        shinyTree::get_selected(self$Biospecimens, format = "slices") |>
          unlist() |>
          names() |>
          tibble::as_tibble() |>
          purrr::set_names("pathString") |>
          dplyr::mutate(pathString = gsub("\\.", "/", pathString)) |>
          dplyr::inner_join(
            # get path and path string mashed up for leafs only...
            self$biospecimen_tree$Get("pathString", filterFun = data.tree::isLeaf) |>
              tibble::enframe(name = "path", value = "pathString")
            , by = "pathString"
          ) |>
          dplyr::distinct(path) |>
          dplyr::arrange() |>
          dplyr::summarise(text = stringr::str_c(path, collapse = "<br />")) |>
          dplyr::pull()
      )
    },

    #' @description
    #' Get / set participant level data based on chosen filters
    #' @return none
    getParticipantData = function() {

      self$ParticipantData <- arrow::open_dataset("Remote_Data/participants") |>
        dplyr::collect() |>
        dplyr::filter(
          Sex %in% self$Sex,
          Karyotype %in% self$Karyotypes,
          AgeAtInitialConsent >= as.numeric(min(self$Age)),
          AgeAtInitialConsent <= as.numeric(max(self$Age))
        )
    },

    #' @description
    #' Get participant sex distribution plot
    #' @param .data - tibble
    #' @return plotly object
    getSexesOverviewPlot = function(.data) {

      dataframe <- self$ParticipantData |>
        dplyr::group_by(Karyotype, Sex) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

      d21 <- dataframe |> dplyr::filter(Karyotype == "Control") |> dplyr::select(Sex,n)
      d21_n <- if (nrow(d21) > 0) {sum(d21[, "n"])} else {0}
      t21 <- dataframe |> dplyr::filter(Karyotype != "Control") |> dplyr::select(Sex,n)
      t21_n <- if (nrow(t21) > 0) {sum(t21[, "n"])} else {0}

      p <- plotly::plot_ly() |>
        plotly::add_pie(
          data = d21,
          labels = ~ Sex,
          values = ~ n,
          textinfo = "label+percent",
          hoverinfo = "none",
          name = "Control",
          domain = list(row = 0, column = 0),
          marker = list(
            colors = c("#BBBDC0", "#f2f2f3"),
            line = list(
              color = "#FFFFFF",
              width = 1
            )
          )
        ) |>
        plotly::add_pie(
          data = t21,
          labels = ~ Sex,
          values = ~ n,
          textinfo = "label+percent",
          hoverinfo = "none",
          name = "Trisomy 21",
          domain = list(row = 0, column = 1),
          marker = list(
            colors = c("#1D4D7C", "#808AA2"),
            line = list(
              color = "#FFFFFF",
              width = 1)
          )
        ) |>
        plotly::layout(
          title = list(
            text = "Sex Distribution",
            x = 0,
            pad = list(l = 10)
          ),
          showlegend = FALSE,
          grid = list(rows = 1, columns = 2),
          annotations = list(
            list(
              x = 0.20,
              y = -0.175,
              xref = "paper",
              yref = "paper",
              text = glue::glue("Control <br />(n={d21_n})"),
              showarrow = FALSE,
              font = list(
                family = "Arial"
              )
            ),
            list(
              x = 0.80,
              y = -0.175,
              xref = "paper",
              yref = "paper",
              text = glue::glue("Trisomy 21 <br />(n={t21_n})"),
              showarrow = FALSE,
              font = list(
                family = "Arial"
              )
            )
          ),
          legend = list(orientation = "b"),
          margin = list(
            l = 10,
            r = 10,
            b = 50,
            t = 50
          ),
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          plot_bgcolor = "#FEF7EA"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue("{self$applicationName} - Sex Distribution {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)

    },

    #' @description
    #' Get Age distribution plot
    #' @param .data - tibble
    #' @return plotly object
    getAgeDistributionOverviewPlot = function(.data) {

      p <- self$ParticipantData |>
        dplyr::filter(!is.na(Karyotype)) |>
        dplyr::group_by(Karyotype, AgeAtInitialConsentAgeBand) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop") |>
        plotly::plot_ly(
          x = ~ AgeAtInitialConsentAgeBand,
          y = ~ n,
          color = ~ Karyotype,
          colors = c("#BBBDC0", "#1D4D7C"),
          type = "bar",
          text = ~ glue::glue("{Karyotype} - {AgeAtInitialConsentAgeBand} years old: {n}"),
          hoverinfo = "text",
          textposition = "none"
        ) |>
        plotly::layout(
          title = list(
            text = "Age at Enrollment",
            x = 0,
            pad = list(l = 10)
          ),
          xaxis = list(
            title = "Age At Enrollment",
            font = list(
              family = "Arial",
              size = 16
            ),
            tickangle = -45
          ),
          yaxis = list(
            title = "# of Participants",
            font = list(
              family = "Arial",
              size = 16
            )
          ),
          margin = list(
            l = 10,
            r = 10,
            b = 50,
            t = 50
          ),
          legend = list(
            x = 0.9,
            y = 0.9,
            font = list(
              family = "Arial",
              size = 16
            )
          )
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue("{self$applicationName} - Age at Enrollment {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

       return(p)

    },

    #' @description
    #' Get Probands  plot
    #' @param .data - tibble
    #' @return plotly object
    getProbandsPlot = function(.data) {

      p <- .data |>
        dplyr::filter(ProbandRelationship != "Control") |>
        dplyr::select(record_id, ProbandRelationship) |>
        dplyr::mutate(
          r = 1,
          T21Only = dplyr::case_when(ProbandRelationship == "T21 Only" ~ 1, TRUE ~ 0),
          T21withparents = dplyr::case_when(stringr::str_detect(ProbandRelationship, "Parent") ~ 1, TRUE ~ 0),
          T21withsiblings = dplyr::case_when(stringr::str_detect(ProbandRelationship, "Sib") ~ 1, TRUE ~ 0)
        ) |>
        dplyr::group_by(r) |>
        dplyr::summarise(
          `T21 Only` = sum(T21Only),
          `T21 with parents` = sum(T21withparents),
          `T21 with siblings`  = sum(T21withsiblings)
        ) |>
        tidyr::pivot_longer(!r, names_to = "FamilyUnit", values_to = "n") |>
        dplyr::select(FamilyUnit, n) |>
        plotly::plot_ly(
          x = ~ n,
          y = ~ FamilyUnit,
          color = ~n,
          colors = c("#BBBDC0", "#1D4D7C"),
          type = "bar",
          text = ~ glue::glue("{FamilyUnit}: {n}"),
          hoverinfo = "text",
          showlegend = FALSE,
          showscale = FALSE,
          textposition = "none"
        ) |>
        plotly::layout(
          title = list(
            text = "Family Unit",
            x = 0,
            pad = list(l = 10)
          ),
          xaxis = list(
            title = "# of Participants",
            font = list(
              family = "Arial",
              size = 16
            )
          ),
          yaxis = list(
            title = "",
            font = list(
              family = "Arial",
              size = 16
            ),
            type = "category",
            categoryorder = "array",
            categoryarray = c("T21 with parents", "T21 with siblings", "T21 Only")
          ),
          margin = list(
            l = 10,
            r = 10,
            b = 50,
            t = 50
          ),
          showlegend = FALSE
        ) |>
        plotly::hide_colorbar() |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue("{self$applicationName} - Family Units {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

    },

    #' @description
    #' Get Samples Availble plot
    #' @param .data - tibble
    #' @return plotly object
    getSamplesAvailablePlot = function(.data) {

      available_samples <- self$SampleDetail |>
        dplyr::inner_join(.data, by = "record_id") |>
        dplyr::mutate(
          aliquot_available = dplyr::case_when(
            out ~ 0,
            volume == 0 ~ 0,
            TRUE ~ 1
          )
        ) |>
        dplyr::group_by(sample_id) |>
        dplyr::mutate(sample_available = max(aliquot_available)) |>
        dplyr::ungroup() |>
        select(record_id, sample_id, vial_barcode_tag, samplePath, aliquot_available, sample_available)

      if (length(shinyTree::get_selected(self$Biospecimens)) > 0) {

        available_samples <-  shinyTree::get_selected(self$Biospecimens, format = "slices") |>
          unlist() |>
          names() |>
          tibble::as_tibble() |>
          purrr::set_names("pathString") |>
          dplyr::mutate(pathString = gsub("\\.", "/", pathString)) |>
          dplyr::inner_join(
            # get path and path string mashed up for leafs only...
            self$biospecimen_tree$Get("pathString", filterFun = data.tree::isLeaf) |>
              tibble::enframe(name = "path", value = "pathString")
            , by = "pathString"
          ) |>
          dplyr::select("samplePath" = pathString) |>
          inner_join(available_samples, by = "samplePath")

      }

      p <- available_samples |>
        dplyr::filter(!is.na(samplePath)) |>
        dplyr::mutate(
          leaf_sample_type = stringr::str_split_i(samplePath, "\\/", -1)
        ) |>
        dplyr::group_by(leaf_sample_type) |>
        dplyr::summarise(
          n_records = n_distinct(record_id),
          n_samples = sum(sample_available),
          n_vials = sum(aliquot_available),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          text = glue::glue(
            "{leaf_sample_type}
              # of Participants: {formattable::comma(n_records, digits = 0)}
              # of Samples Available: {formattable::comma(n_samples, digits = 0)}
              # of Aliquots Available: {formattable::comma(n_vials, digits = 0)}
              "
          )
        ) |>
        plotly::plot_ly(
          x = ~ leaf_sample_type,
          y = ~ n_records,
          color = ~ leaf_sample_type,
          colors = c("#BBBDC0", "#1D4D7C"),
          type = "bar",
          text = ~ text,
          hoverinfo = "text",
          textposition = "none"
        ) |>
        plotly::layout(
          showlegend = FALSE,
          title = list(
            text = "Samples Available",
            x = 0,
            pad = list(l = 10)
          ),
          xaxis = list(
            title = "",
            font = list(
              family = "Arial",
              size = 16
            ),
            tickangle = -45,
            "categoryorder" = "total descending"
          ),
          yaxis = list(
            title = "# of Participants",
            font = list(
              family = "Arial",
              size = 16
            )
          ),
          margin = list(
            l = 10,
            r = 10,
            b = 50,
            t = 50
          )
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue("{self$applicationName} - Samples Available {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)


    },

    #' @description
    #' Get OMICS Samples Availble plot
    #' @param .data - tibble
    #' @return plotly object
    getOmicsSamplesAvailablePlot = function(.data) {

      p <- self$PlatformExperiments |>
        dplyr::filter(ExperimentID %in% self$AnalysisAvailable) |>
        dplyr::inner_join(
          self$ParticipantPlatformExperiment |>
            dplyr::distinct(record_id, ExperimentID),
          by = c("ExperimentID")
        ) |>
        dplyr::inner_join(.data, by = "record_id") |>
        dplyr::group_by(PlatformGroup, PlatformDisplayName, ExperimentStudyName, ExperimentID) |>
        dplyr::summarise(
          n_records = dplyr::n_distinct(record_id)
        ) |>
        dplyr::mutate(
          text = glue::glue(
            "
            {ExperimentStudyName} - {PlatformDisplayName}
            Total Participants: {formattable::comma(n_records, digits = 0)}
            "
          )
        ) |>
        plotly::plot_ly(
          x = ~ PlatformGroup,
          y = ~ n_records,
          color = ~ ExperimentID,
          type = "bar",
          showlegend = FALSE,
          showscale = FALSE,
          text = ~ text,
          hoverinfo = "text",
          textposition = "none"
        ) |>
        plotly::layout(
          barmode = "stack",
          showlegend = FALSE,
          title = list(
            text = "Analyses Available",
            x = 0,
            pad = list(l = 10)
          ),
          margin = list(
            l = 10,
            r = 10,
            b = 50,
            t = 50
          ),
          xaxis = list(
            title = "",
            font = list(
              family = "Arial",
              size = 16
            ),
            tickangle = -45,
            type = "category",
            categoryorder = "array",
            categoryarray = c(
              "Transcriptome",
              "Proteome",
              "Metabolome",
              "Immune Map"
            )
          ),
          yaxis = list(
            title = "# of Datasets",
            font = list(
              family = "Arial",
              size = 16
            )
          )
        ) |>
        plotly::hide_colorbar() |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue(
              "{self$applicationName} - Omics Analyses Available {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
            ),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)

    },

    #' @description
    #' Get Race/Ethnicity plot
    #' @param .data - tibble
    #' @return plotly object
    getRaceEthnicityPlot = function(.data) {

      race_ethnicity <- .data |>
        dplyr::mutate(
          Race = ifelse(Race == "" | is.na(Race), "Unknown", Race),
          Race = ifelse(Race == ">1 race", "Multiracial", Race),
          Ethnicity = ifelse(Ethnicity == "" | is.na(Ethnicity), "Unknown", Ethnicity)
        ) |>
        dplyr::select(Race, Ethnicity, record_id) |>
        dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_trim))

      p <- plot_ly() |>
        add_pie(
          data = count(race_ethnicity, Race),
          labels = ~Race,
          values = ~n,
          name = "Race",
          legendgroup = "Race",
          textposition = "inside",
          textinfo = "label+percent",
          insidetextfont = list(color = "#FFFFFF"),
          hovertemplate = "%{percent} of participants are %{label}<extra></extra>",
          hole = 0.45,
          domain = list(
            x = c(0, 0.45),
            y = c(0.0, 0.98)
          )
        ) |>
        add_pie(
          data = count(race_ethnicity, Ethnicity),
          labels = ~Ethnicity,
          values = ~n,
          name = "Ethnicity",
          legendgroup = "Ethnicity",
          textposition = "inside",
          textinfo = "label+percent",
          insidetextfont = list(color = "#FFFFFF"),
          hovertemplate = "%{percent} of participants are %{label}<extra></extra>",
          hole = 0.45,
          domain = list(
            x = c(0.55, 1),
            y = c(0.0, 0.98)
          )
        ) |>
        layout(
          title = list(
            text = "Race & Ethnicity",
            x = 0,
            pad = list(
              l = 10
            )
          ),
          showlegend = TRUE,
          legend = list(
            title = "Race / Ethnicity",
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            font = list(
              size = 12
            )
          ),
          uniformtext = list(
            minsize = 8,
            mode = "hide"
          ),
          margin = list(
            l = 10,
            r = 10,
            b = 50,
            t = 50
          ),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          annotations = list(
            list(
              x = 0.22,
              y = -0.05,
              text = "Race",
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE,
              font = list(
                size = 16
              )
            ),
            list(
              x = 0.78,
              y = -0.05,
              text = "Ethnicity",
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE,
              font = list(
                size = 16
              )
            )
          )
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue(
              "{self$applicationName} - Race and Ethnicity {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
            ),
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)

    },

    #' @description
    #' Get Participants States plot
    #' @param .data - tibble
    #' @return plotly object
    getParticipantStatesPlot = function(.data) {

      states_geo <- tigris::geo_join(
        states_shp,
        .data |>
          dplyr::select(State, record_id) |>
          dplyr::group_by(State) |>
          dplyr::summarize(total = dplyr::n_distinct(record_id), .groups = "drop"),
        "NAME", "State"
      ) |>
        dplyr::filter(!is.na(total))

      pal <- leaflet::colorNumeric("Blues", domain = states_geo$total)

      popup_sb <- ifelse(
        states_geo$total > 5,
        glue::glue("{states_geo$NAME} Participants: {as.character(states_geo$total)}"),
        glue::glue("{states_geo$NAME} Participants: Less than 5")
      )

      tag_map_title <- tags$style(HTML("
          .leaflet-control.map-title {
            transform: translate(-50%,20%);
            position: fixed !important;
            left: 50%;
            text-align: center;
            padding-left: 10px;
            padding-right: 10px;
            background: rgba(255,255,255,0.75);
            font-weight: bold;
            font-size: 28px;
          }
        "))

      title <- tags$div(
        tag_map_title, HTML("State of Residence")
      )

      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(-98.483330, 38.712046, zoom = 4) |>
        leaflet::addPolygons(
          data = states_geo,
          fillColor = ~ pal(states_geo$total),
          fillOpacity = 0.9,
          weight = 0.2,
          smoothFactor = 0.2,
          highlight = leaflet::highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.6,
            bringToFront = TRUE
          ),
          label = popup_sb,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) |>
        leaflet::addLegend(
          pal = pal,
          values = states_geo$total,
          position = "bottomright",
          title = "Participants"
        ) |>
        leaflet::addControl(
          title,
          position = "topleft",
          className = "map-title"
        )

    },

    #' @description
    #' set summary condition class counts tibble
    #' @return none
    updateSummaryConditionClassCounts = function() {

      if (!is.null(self$conditionClasses)) {

        p <- self$ConditionClassData|>
          dplyr::filter(HasCondition == "True") |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")


        if (nrow(p[p$Karyotype == "Control", ]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tible::tibble(
              Karyotype = "Control",
              n = 0
            )
          )
        }

        if (nrow(p[p$Karyotype == "Trisomy 21", ]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tibble::tibble(
              Karyotype = "Trisomy 21",
              n = 0
            )
          )
        }

        a <- self$ConditionClassData |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

        self$conditionClassSummary <- dplyr::inner_join(
            p,
            a,
            by = c("Karyotype")
          ) |>
          dplyr::mutate(frequency = n.x / n.y) |>
          dplyr::select(-c(n.x, n.y)) |>
          dplyr::mutate(
            frequency = round(frequency, 4),
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            ConditionClass = "Conditions Classes Total"
          ) |>
          dplyr::select(ConditionClass, Karyotype, frequency) |>
          tidyr::pivot_wider(names_from = Karyotype, values_from = frequency) |>
          dplyr::select(ConditionClass, `Control %`, `Trisomy 21 %`)

      } else {

        p <- self$ConditionClassData |>
          dplyr::filter(HasCondition == "True") |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

        if (nrow(p[p$Karyotype == "Control", ]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tibble::tibble(
              Karyotype = "Control",
              n = 0
            )
          )
        }

        if (nrow(p[p$Karyotype == "Trisomy 21", ]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tibble::tibble(
              Karyotype = "Trisomy 21",
              n = 0
              )
            )
        }

        a <- self$ConditionClassData |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

        self$conditionClassSummary <- dplyr::inner_join(
            p,
            a,
            by = c("Karyotype")
          ) |>
          dplyr::mutate(frequency = n.x / n.y) |>
          dplyr::select(-c(n.x, n.y)) |>
          dplyr::mutate(
            frequency = round(frequency, 4),
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            ConditionClass = "Conditions Classes Total"
          ) |>
          dplyr::select(ConditionClass, Karyotype, frequency) |>
          tidyr::pivot_wider(names_from = Karyotype, values_from = frequency) |>
          dplyr::select(ConditionClass, `Control %`, `Trisomy 21 %`)

      }
    },

    #' @description
    #' get summary condition class counts tibble
    #' @return tibble
    getConditionClassSummary = function() {
      return(self$conditionClassSummary)
    },

    #' @description
    #' set conditions counts tibble
    #' @return none
    updateConditions = function() {

      self$conditions <- self$ConditionClassData |>
        dplyr::filter(ConditionClass %in% self$conditionClasses) |>
        dplyr::select(Condition) |>
        dplyr::distinct() |>
        dplyr::pull()

      p <- self$ConditionClassData |>
        dplyr::filter(
          HasCondition == "True",
          ConditionClass %in% self$conditionClasses
        ) |>
        dplyr::group_by(Condition, Karyotype) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

      a <- self$ConditionClassData |>
        dplyr::filter(ConditionClass %in% self$conditionClasses) |>
        dplyr::group_by(Condition, Karyotype) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

      self$conditionCounts <- dplyr::inner_join(
          p,
          a,
          by = c("Karyotype", "Condition")
        ) |>
        dplyr::mutate(frequency = n.x / n.y) |>
        dplyr::select(-c(n.x, n.y)) |>
        dplyr::mutate(
          frequency = round(frequency, 4) * 100,
          Karyotype = dplyr::case_when(
            Karyotype == "Control" ~ "Control %",
            TRUE ~ "Trisomy 21 %"
          )
        ) |>
        dplyr::select(Condition, Karyotype, frequency) |>
        tidyr::pivot_wider(names_from = Karyotype, values_from = frequency, values_fill = 0) |>
        dplyr::select(Condition, `Control %`, `Trisomy 21 %`)

    },

    #' @description
    #' set conditions summary counts tibble
    #' @return none
    updateSummaryConditionCounts = function() {

      # # #top level condition was chosen
      if (length(self$conditionClasses) > 0) {

        #and a child condition was chosen
        if (length(self$conditions) > 0) {

          #positive diagnoses
          p <- self$ConditionClassData |>
            dplyr::filter(HasCondition == "True") |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::filter(Condition %in% self$conditions) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

          a <- self$ConditionClassData |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::filter(Condition %in% self$conditions) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

        } else {
          # only top level items chosen

          p <- self$ConditionClassData |>
            dplyr::filter(HasCondition == "True") |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")


          a <- self$ConditionClassData |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

        }

        self$conditionSummary <- dplyr::inner_join(
            p,
            a,
            by = c("Karyotype")
          ) |>
          dplyr::mutate(frequency = n.x / n.y) |>
          dplyr::select(-c(n.x, n.y)) |>
          dplyr::mutate(
            frequency = round(frequency, 4),
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            Condition = "Specific Conditions Total"
          ) |>
          dplyr::select(Condition, Karyotype, frequency) |>
          tidyr::pivot_wider(names_from = Karyotype, values_from = frequency)

      } else {

      ###nothing was chosen at top level
        p <- NULL

      }

      if (!is.null(p)) {

        ### Some conditions have 0 participants, fill in that value for the pivot below
        if (nrow(p[p$Karyotype == "Control", ]) == 0) {
          p <- dplyr::bind_rows(p, tibble::tibble(Karyotype = "Control", n = 0))
        }

        if (nrow(p[p$Karyotype == "Trisomy 21", ]) == 0) {
          p <- dplyr::bind_rows(p, tibble::tibble(Karyotype = "Trisomy 21", n = 0))
        }

        self$conditionSummary <- dplyr::inner_join(
            p,
            a,
            by = c("Karyotype")
          ) |>
          dplyr::mutate(frequency = n.x / n.y) |>
          dplyr::select(-c(n.x, n.y)) |>
          dplyr::mutate(
            frequency = round(frequency, 4),
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            Condition = "Specific Conditions Total"
          ) |>
          dplyr::select(Condition, Karyotype, frequency) |>
          tidyr::pivot_wider(names_from = Karyotype, values_from = frequency)

      } else {
        self$conditionSummary <- NULL
      }
    },

    #' @description
    #' set conditions class sex counts tibble
    #' @return none
    updateConditionClassSexCounts = function() {
      if (!is.null(self$conditionClasses)) {

        self$ConditionClassSexCounts <- self$ConditionClassData |>
          dplyr::filter(HasCondition == "True") |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::group_by(Karyotype, Sex, ConditionClass) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop") |>
          tidyr::pivot_wider(names_from = Sex, values_from = n, values_fill = 0) |>
          dplyr::mutate(
            total = Male + Female,
            Female = Female / total,
            Male = Male / total
          ) |>
          dplyr::rename("Condition" = ConditionClass) |>
          tidyr::pivot_longer(!c(Condition, Karyotype), names_to = c("Sex"), values_to = "Percent") |>
          dplyr::arrange(Condition)
      } else {
        self$ConditionClassSexCounts <- NULL
      }
    },

    #' @description
    #' set conditions sex counts tibble
    #' @return none
    updateConditionSexCounts = function() {
      if (!is.null(self$conditionClasses) & !is.null(self$conditions)) {

        cartesian <- tidyr::crossing(
          "Sex" = c("Male", "Female"),
          "Karyotype" = c("Control", "Trisomy 21"),
          "Condition" = self$conditions
        )

        counts <- self$ConditionClassData |>
          dplyr::filter(HasCondition == "True") |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::filter(Condition %in% self$conditions) |>
          dplyr::group_by(Karyotype, Sex, Condition) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = "drop")

        self$ConditionSexCounts <- cartesian |>
          dplyr::left_join(counts, by = c("Sex", "Condition", "Karyotype")) |>
          tidyr::replace_na(list("n" = 0)) |>
          tidyr::pivot_wider(names_from = Sex, values_from = n, values_fill = 0) |>
          dplyr::mutate(
            total = Male + Female,
            Female = Female / total,
            Male = Male / total
          ) |>
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
          dplyr::select(Condition = Condition, Karyotype, Female, Male) |>
          tidyr::pivot_longer(!c(Condition, Karyotype), names_to = "Sex", values_to = "Percent") |>
          dplyr::arrange(Condition)

      } else {
        self$ConditionSexCounts <- NULL
      }

    },

    #' @description
    #' get condition sex plot
    #' @param .data tibble
    #' @param type - string - condition annotation level
    #' @importFrom formattable percent
    #' @return plotly object
    getGetConditionStackedSexPlot = function(.data, type = "Classes") {

      if (type == "Classes") {
        .data <- .data |>
          dplyr::filter(Sex != "total")
      }

      .data$Percent <- formattable::percent(.data$Percent)

      d21 <- .data |>
        dplyr::filter(Karyotype == "Control") |>
        dplyr::mutate(Sex = glue::glue("{Sex} {Karyotype}")) |>
        dplyr::arrange(desc(Condition))

      t21 <- .data |>
        dplyr::filter(Karyotype != "Control") |>
        dplyr::mutate(Sex = glue::glue("{Sex} {Karyotype}")) |>
        dplyr::arrange(desc(Condition))

      p1 <- d21 |>
        plotly::plot_ly(
          type = "bar",
          x = ~ Percent,
          y = ~ Condition,
          color = ~ Sex,
          colors = c("#BBBDC0", "#f2f2f3"),
          legendgroup = ~ Sex,
          text = ~ glue::glue("{Sex}<br />Condition: {Condition}<br /><i>Percent:</i> {Percent}"),
          hoverinfo = "text"
        ) |>
        plotly::layout(
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5
          ),
          margin = list(
            autoexpand = TRUE,
            l = 10,
            r = 30,
            t = 0
          ),
          font = list(
            family = "Arial"
          ),
          xaxis = list(
            showgrid = FALSE,
            title = "",
            font = list(
              family = "Arial"
            ),
            tickformat = ".00%",
            range = c(0, 1)
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            font = list(
              family = "Arial"
            ),
            type = "category",
            categoryorder = "category descending"
          ),
          barmode = "stack",
          shapes = list(
            list(
              type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = 0.5,
              x1 = 0.5,
              line = list(
                color = "red",
                dash = "dash"
              )
            )
          )
        )

      p2 <- t21 |>
        plotly::plot_ly(
          type = "bar",
          x = ~ Percent,
          y = ~ Condition,
          color = ~ Sex,
          colors = c("#1D4D7C", "#3E99CD"),
          text = ~ glue::glue("{Sex}<br />Condition: {Condition}<br /><i>Percent:</i> {Percent}"),
          hoverinfo = "text",
          legendgroup = ~ Sex
        ) |>
        plotly::layout(
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5
          ),
          margin = list(
            autoexpand = TRUE,
            l = 10,
            r = 30,
            t = 0
          ),
          font = list(
            family = "Arial"
          ),
          xaxis = list(
            title = "",
            showgrid = FALSE,
            font = list(
              family = "Arial"
            ),
            tickformat = ".00%",
            range = c(0, 1)
          ),
          yaxis = list(
            showgrid = FALSE,
            font = list(
              family = "Arial"
            ),
            type = "category",
            categoryorder = "category descending",
            title = ""
          ),
          barmode = "stack",
          shapes = list(
            list(
              type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = 0.5,
              x1 = 0.5,
              line = list(
                color = "red",
                dash = "dash"
              )
            )
          )
        )

      if (nrow(d21) > 0 & nrow(t21) > 0) {
        p <- plotly::subplot(p1, p2, nrows = 1, shareY = TRUE, margin = 0.05)
      }
      if (nrow(d21) > 0 & nrow(t21) == 0) {
        p <- p1
      }
      if (nrow(d21) == 0 & nrow(t21) > 0) {
        p <- p2
      }

      p <- p |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          modeBarButtons = list(
            list("toImage")
          )
        )

      if (type == "Classes") {
        p <- p |>
          plotly::layout(
            title = list(
              text = "Condition Classes by Sex and Karyotype",
              x = 0,
              pad = list(l = 10)
            ),
            margin = list(
              l = 10,
              r = 10,
              b = 50,
              t = 50
            )
          ) |>
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue::glue(
                "{self$applicationName} - Condition Classes by Sex and Karyotype {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
              ),
              width = NULL,
              height = NULL
            ),
            modeBarButtons = list(
              list("toImage")
            )
          )

      } else {
        p <- p |>
          plotly::layout(
            title = list(
              text = "Specific Conditions by Sex and Karyotype",
              x = 0,
              pad = list(l = 10)
            ),
            margin = list(
              l = 10,
              r = 10,
              b = 50,
              t = 50
            )
          ) |>
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue::glue(
                "{self$applicationName} - Specific Conditions by Sex 
                and Karyotype {format(Sys.time(),\"%Y%m%d_%H%M%S\")}"
              ),
              width = NULL,
              height = NULL
            ),
            modeBarButtons = list(
              list("toImage")
            )
          )
      }

      return(p)

    },

    #' @description
    #' set condition upset plot data
    #' @return none
    update_upset_plot_data = function() {

      condition_data <- arrow::open_dataset("Remote_Data/participant_conditions") |>
        dplyr::collect() |>
        tidyr::separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
        dplyr::filter(HasCondition == "True") |>
        dplyr::inner_join(
          arrow::open_dataset("Remote_Data/participants") |>
            dplyr::collect() |>
            dplyr::filter(
              Sex %in% self$Sex,
              AgeAtInitialConsent >= min(self$Age),
              AgeAtInitialConsent <= max(self$Age),
              Karyotype %in% self$Karyotypes,
            ) |>
            dplyr::distinct(record_id)
          , by = "record_id"
        )
      if (self$selectedAnnotationLevel == "Classes of Conditions") {
        condition_data <- condition_data |>
          dplyr::filter(ConditionClass %in% self$SelectedConditions) |>
          dplyr::select(-Condition) |>
          dplyr::rename(Condition = "ConditionClass")
      } else {
        condition_data <- condition_data |>
          dplyr::filter(Condition %in% self$SelectedConditions)
      }

      self$upsetPlotData <- condition_data |>
        dplyr::select(record_id, Condition) |>
        dplyr::distinct() |>
        dplyr::mutate(ConditionMember = 1) |>
        tidyr::pivot_wider(names_from = Condition, values_from = ConditionMember, values_fill = 0) |>
        as.data.frame()

    },

    #' @description
    #' reset condition upset plot data tibble to NULL states
    #' @return none
    clear_upset_plot_data = function() {
      self$upsetPlotData <- NULL
    }

  )
)
