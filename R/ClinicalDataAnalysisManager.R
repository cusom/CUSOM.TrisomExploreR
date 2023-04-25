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
    SamplesAvailable = NULL,
    ParticipantOMICSAvailable = NULL,
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


    initialize = function(applicationName, id, namespace_config, remoteDB, localDB){

      self$applicationName <- applicationName
      self$remoteDB <- remoteDB
      self$localDB <- localDB

      self$Enrollments <- self$localDB$getQuery(
         "SELECT Karyotype, count(1) [ParticipantCount]
          FROM AllParticipants
          GROUP BY Karyotype"
        ) |>
        dplyr::mutate(ParticipantCount = ifelse(Karyotype == "Trisomy 21", ParticipantCount + 8, ParticipantCount + 6))

      self$AllN <- self$localDB$getQuery(
        "SELECT count(distinct record_id) [n]
          FROM AllParticipants"
      ) |>
        dplyr::pull()

      self$ConditionClassData <- self$localDB$getQuery(
        "SELECT * FROM ConditionClassData"
      )

      self$conditionClassCounts <- dplyr::inner_join(
        self$ConditionClassData |>
          dplyr::filter(HasCondition == 'True') |>
          dplyr::group_by(Karyotype,ConditionClass) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop'),
        self$ConditionClassData |>
          dplyr::group_by(Karyotype,ConditionClass) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop'),
          by = c("ConditionClass","Karyotype")
        ) |>
        dplyr::mutate( Frequency = `n.x` / `n.y` ) |>
        dplyr::select(Karyotype,ConditionClass,Frequency) |>
        dplyr::mutate(
          Frequency = round(Frequency,4)*100,
          Karyotype = dplyr::case_when(
            Karyotype == "Control" ~ "Control %",
            TRUE ~ "Trisomy 21 %"
          )
        ) |>
        tidyr::pivot_wider(names_from = Karyotype, values_from = Frequency) |>
        dplyr::filter(!is.na(ConditionClass)) |>
        dplyr::rename("Class of Condition" = ConditionClass)

      self$updateSummaryConditionClassCounts()

    },

    getParticipantData = function() {

      participants <- self$localDB$getQuery(
        "SELECT * FROM AllParticipants"
      )

      self$ParticipantData <- participants |>
        dplyr::filter(
          Sex %in% self$Sex,
          Karyotype %in% self$Karyotypes,
          AgeAtInitialConsent >= as.numeric(min(self$Age)),
          AgeAtInitialConsent <= as.numeric(max(self$Age))
        ) |>
        dplyr::inner_join(
          participants |>
            dplyr::left_join(
              self$localDB$getQuery(
                "SELECT * FROM ParticipantOMICSAvailable"
                ) |>
                tidyr::separate_rows(OMICSSampleAvailable, sep = ",", "OMICSSampleAvailable", convert = TRUE)
              , by = "record_id") |>
            dplyr::mutate(OMICSSampleAvailable = ifelse(is.na(OMICSSampleAvailable),"None",OMICSSampleAvailable)) |>
            dplyr::filter(OMICSSampleAvailable %in% c(self$OmicsSamples,"None")) |>
            dplyr::distinct(record_id),
          by = "record_id"
        ) |>
        dplyr::inner_join(
          participants |>
            tidyr::separate_rows(SamplesAvailable, sep = ",", "SamplesAvailable", convert=TRUE) |>
            dplyr::filter(SamplesAvailable %in% c(self$Samples,"None")) |>
            dplyr::distinct(record_id),
          by = "record_id"
        )

    },

    getSexesOverviewPlot = function(.data) {

      dataframe <- self$ParticipantData |>
        dplyr::group_by(Karyotype,Sex) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups='drop')

      d21 <- dataframe |> dplyr::filter(Karyotype == "Control") |> dplyr::select(Sex,n)
      d21_n <- if(nrow(d21)>0){sum(d21[,"n"])}else{0}
      t21 <- dataframe |> dplyr::filter(Karyotype != "Control") |> dplyr::select(Sex,n)
      t21_n <- if(nrow(t21)>0){sum(t21[,"n"])}else{0}

      p <- plotly::plot_ly() |>
        plotly::add_pie(
          data = d21,
          labels = ~ Sex,
          values = ~ n,
          textinfo = 'label+percent',
          hoverinfo = 'none',
          name = "Control",
          domain = list(row = 0, column = 0),
          marker = list(
            colors = d21Colors,
            line = list(
              color = '#FFFFFF',
              width = 1
            )
          )
        ) |>
        plotly::add_pie(
          data = t21,
          labels = ~ Sex,
          values = ~ n,
          textinfo = 'label+percent',
          hoverinfo = 'none',
          name = "Trisomy 21",
          domain = list(row = 0, column = 1),
          marker = list(
            colors = c("#1D4D7C", "#808AA2"),
            line = list(
              color = '#FFFFFF',
              width = 1)
          )
        ) |>
        plotly::layout(
          title = list(
            text = "Sex Distribution",
            x = 0,
            pad = list(l = 10)
          ),
          showlegend = F,
          grid = list(rows=1, columns=2),
          annotations = list(
            list(
              x = 0.20,
              y = -0.175,
              xref = "paper",
              yref = "paper",
              text = glue::glue("Control <br />(n={d21_n})"),
              showarrow = F,
              font = list (
                family = "Arial"
              )
            ),
            list(
              x = 0.80,
              y = -0.175,
              xref = "paper",
              yref = "paper",
              text = glue::glue("Trisomy 21 <br />(n={t21_n})"),
              showarrow = F,
              font = list (
                family = "Arial"
              )
            )
          ),
          legend = list(orientation = 'b'),
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
          plot_bgcolor='#FEF7EA'
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - Sex Distribution {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)

    },
    getAgeDistributionOverviewPlot = function(.data) {

      p <- self$ParticipantData |>
        dplyr::filter(!is.na(Karyotype)) |>
        dplyr::group_by(Karyotype,AgeAtInitialConsentAgeBand) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop') |>
        plotly::plot_ly(
          x = ~ AgeAtInitialConsentAgeBand,
          y = ~ n,
          color = ~ Karyotype,
          colors = c("#BBBDC0", "#1D4D7C"),
          type = "bar",
          text = ~ glue::glue('{Karyotype} - {AgeAtInitialConsentAgeBand} years old: {n}'),
          hoverinfo = 'text'
        ) |>
        plotly::layout(
          title = list(
            text = "Age at Enrollment",
            x = 0,
            pad = list(l = 10)
          ),
          xaxis = list(
            title = "Age At Enrollment",
            font = list (
              family = "Arial",
              size = 16
            ),
            tickangle = -45
          ),
          yaxis = list (
            title = "# of Participants",
            font = list (
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
            font = list (
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
            filename = glue::glue('{self$applicationName} - Age at Enrollment {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

       return(p)

    },
    getProbandsPlot = function(.data) {

      p <- .data |>
        dplyr::filter(ProbandRelationship != "Control") |>
        dplyr::select(record_id, ProbandRelationship) |>
        dplyr::mutate(
          r = 1,
          T21Only = dplyr::case_when(ProbandRelationship == "T21 Only" ~ 1, TRUE ~ 0),
          T21withparents = dplyr::case_when(stringr::str_detect(ProbandRelationship,"Parent") ~ 1, TRUE ~ 0),
          T21withsiblings = dplyr::case_when(stringr::str_detect(ProbandRelationship,"Sib") ~ 1, TRUE ~ 0)
        ) |>
        dplyr::group_by(r) |>
        dplyr::summarise(
          `T21 Only` = sum(T21Only),
          `T21 with parents` = sum(T21withparents),
          `T21 with siblings`  = sum(T21withsiblings)
        ) |>
        tidyr::pivot_longer(!r, names_to = "FamilyUnit", values_to = "n") |>
        dplyr::select(FamilyUnit,n) |>
        plotly::plot_ly(
          x = ~ n,
          y = ~ FamilyUnit,
          color = ~n,
          colors = c("#BBBDC0", "#1D4D7C"),
          type = "bar",
          text = ~ glue::glue('{FamilyUnit}: {n}'),
          hoverinfo = 'text',
          showlegend = FALSE,
          showscale = FALSE,
          textposition = "none"
        ) |>
        plotly::layout(
          title = list(
            text = "Family Unit",
            x = 0,
            pad = list(l=10)
          ),
          xaxis = list(
            title = "# of Participants",
            font = list (
              family = "Arial",
              size = 16
            )
          ),
          yaxis = list (
            title = "",
            font = list (
              family = "Arial",
              size = 16
            ),
            type = 'category',
            categoryorder = "array",
            categoryarray = c("T21 with parents"
                              ,"T21 with siblings"
                              ,"T21 Only")
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
            filename = glue::glue('{self$applicationName} - Family Units {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

    },
    getSamplesAvailablePlot = function(.data) {

      dataframe <- .data |>
        dplyr::select(record_id,SamplesAvailable) |>
        tidyr::separate_rows(sep = ',','SamplesAvailable',convert=TRUE) |>
        dplyr::filter(SamplesAvailable %in% self$Samples) |>
        dplyr::group_by(SamplesAvailable) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups='drop') |>
        dplyr::mutate(
          Sample = gsub("\\s*\\([^\\)]+\\)", "", SamplesAvailable),
          SampleType = gsub(".*\\((.*)\\).*", "\\1", SamplesAvailable),
          sampleType = ifelse(SamplesAvailable == SampleType, "Other", SampleType),
          SampleType = forcats::fct_relevel(SampleType, c("Other","Lysates","Cryopreserved")),
          SortOrder = dplyr::case_when(
            Sample == "Plasma" ~ 1,
            Sample == "Red Blood Cells" ~ 2,
            Sample == "White Blood Cells " ~ 3,
            Sample == "B cells " ~ 5,
            Sample == "Monocytes " ~ 6,
            Sample == "CD4 T cells " ~ 7,
            Sample == "CD8 T cells " ~ 8,
            Sample == "Tregs " ~ 9,
            Sample == "NK Cell " ~ 10,
            Sample == "PaxDNA" ~ 11,
            Sample == "PaxRNA" ~ 12,
            Sample == "Tongue swab" ~ 13,
            Sample == "Urine" ~ 14,
            TRUE ~ 15
          )
        )

      p <- dataframe |>
        plotly::plot_ly(
          x= ~ Sample,
          y= ~ n,
          color = ~ SampleType,
          colors = c("#BBBDC0", "#1D4D7C"),
          type= "bar",
          text = ~ glue::glue('{Sample} - {SampleType}: {n}'),
          hoverinfo = 'text',
          textposition = "none"
        ) |>
        plotly::layout(
          barmode = 'stack',
          title = list(
            text = "Samples Available",
            x = 0,
            pad = list(l = 10)
          ),
          xaxis = list(
            title = "",
            font = list (
              family = "Arial",
              size = 16
            ),
            tickangle = -45,
            type = 'category',
            categoryorder = "array",
            categoryarray = (
              dataframe |>
               dplyr::arrange(SortOrder) |>
               dplyr::select(Sample) |>
               dplyr::pull()
            )
          ),
          yaxis = list (
            title = "# of Participants",
            font = list (
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
            filename = glue::glue('{self$applicationName} - Samples Available {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)

    },
    getOmicsSamplesAvailablePlot = function(.data) {

      p <- .data |>
        dplyr::left_join(
          self$localDB$getQuery(
            "SELECT * FROM ParticipantOMICSAvailable"
          ),
          by = "record_id"
        ) |>
        dplyr::select(record_id,OMICSSampleAvailable) |>
        dplyr::mutate(OMICSSampleAvailable = ifelse(is.na(OMICSSampleAvailable), "none", OMICSSampleAvailable)) |>
        tidyr::separate_rows(sep = ',','OMICSSampleAvailable', convert=TRUE) |>
        dplyr::filter(OMICSSampleAvailable %in% self$OmicsSamples) |>
        dplyr::group_by(OMICSSampleAvailable) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups='drop') |>
        dplyr::mutate(
          SortOrder = dplyr::case_when(
            OMICSSampleAvailable == "Transcriptome" ~ 1,
            OMICSSampleAvailable == "Proteome" ~ 2,
            OMICSSampleAvailable == "Metabolome" ~ 3,
            OMICSSampleAvailable == "Immune Map" ~ 4,
            TRUE ~ 5
          )
        ) |>
        plotly::plot_ly(
          x = ~ OMICSSampleAvailable,
          y = ~ n,
          color = ~n,
          colors = c("#BBBDC0", "#1D4D7C"),
          type = "bar",
          showlegend = FALSE,
          showscale = FALSE,
          text = ~ glue::glue('{OMICSSampleAvailable}: {n}'),
          hoverinfo = 'text',
          textposition = "none"
        ) |>
        plotly::layout(
          title = list(
            text = "Omics Analyses Available",
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
            font = list (
              family = "Arial",
              size = 16
            ),
            tickangle = -45,
            type = 'category',
            categoryorder = "array",
            categoryarray = c(
              "Transcriptome",
              "Proteome",
              "Metabolome",
              "Immune Map"
            )
          ),
          yaxis = list (
            title = "# of Participants",
            font = list (
              family = "Arial",
              size = 16
            )
          ),
          showlegend = FALSE
        ) |>
        plotly::hide_colorbar() |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - Omics Analyses Available {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)

    },
    getRaceEthnicityPlot = function(.data) {

      RaceEthnicity <- .data |>
        dplyr::mutate(Race = ifelse(Race == "" | is.na(Race), "Unknown", Race)) |>
        dplyr::mutate(Ethnicity = ifelse(Ethnicity == "" | is.na(Ethnicity), "Unknown", Ethnicity)) |>
        dplyr::select(Race,Ethnicity,record_id) |>
        dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_trim))

      p <- RaceEthnicity |>
        dplyr::group_by(Race,Ethnicity) |>
        dplyr::summarize(race_eth_n = dplyr::n_distinct(record_id), .groups='drop') |>
        dplyr::mutate(
          race_eth_total = sum(race_eth_n),
          race_eth_pct = race_eth_n/race_eth_total
        ) |>
        dplyr::inner_join(
          RaceEthnicity |>
            dplyr::group_by(Race) |>
            dplyr::summarize(race_n = dplyr::n_distinct(record_id), .groups='drop') |>
            dplyr::mutate(
              race_total = sum(race_n),
              race_pct = race_n/race_total
            ),
          by = "Race"
        ) |>
        dplyr::mutate(
          race_eth_subgroup_pct = race_eth_n / race_n
        ) |>
        dplyr::select(Race, Ethnicity,race_eth_n, race_pct, race_eth_subgroup_pct) |>
        plotly::plot_ly(
          type = 'bar',
          y = ~ Race,
          x = ~ race_eth_n,
          color = ~ Ethnicity,
          colors = c("#1D4D7C","#3E99CD","#BBBDC0"),
          legendgroup = ~ Ethnicity,
          text = ~ glue::glue('{scales::percent(race_pct)} of Participants identify as <b>{Race}</b><br />
                    {scales::percent(race_eth_subgroup_pct)} of {Race} Participants <br /> identify as <b>{Ethnicity}</b>'
          ),
          hoverinfo = 'text',
          textposition = "none"
        ) |>
        plotly::layout(
          legend = list(
            x = 0.4,
            y = 0.1
          ),
          title = list(
            text = "Race and Ethnicity",
            x = 0,
            pad = list(l = 10)
          ),
          xaxis = list(
            title='',
            showgrid = F,
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            font = list (
              family = "Arial",
              size = 18
            )
          ),
          yaxis = list(
            title='',
            showgrid = F,
            showticklabels = TRUE,
            font = list (
              family = "Arial",
              size = 18
            )
          ),
          margin = list(
            l = 10,
            r = 10,
            b = 50,
            t = 50
          ),
          barmode = 'stack'
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "svg",
            filename = glue::glue('{self$applicationName} - Race and Ethnicity {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
            width = NULL,
            height = NULL
          ),
          modeBarButtons = list(
            list("toImage")
          )
        )

      return(p)

    },
    getParticipantStatesPlot = function(.data) {

      StatesGeo <- tigris::geo_join(
        states_shp,
        .data |>
          dplyr::select(State, record_id) |>
          dplyr::group_by(State) |>
          dplyr::summarize(total = dplyr::n_distinct(record_id), .groups='drop'),
        "NAME", "State"
      ) |>
        dplyr::filter(!is.na(total))

      pal <- leaflet::colorNumeric("Blues", domain = StatesGeo$total)

      popup_sb <- ifelse(
        StatesGeo$total > 5 ,
        glue::glue("{StatesGeo$NAME} Participants: {as.character(StatesGeo$total)}"),
        glue::glue("{StatesGeo$NAME} Participants: Less than 5")
      )

      tag.map.title <- tags$style(HTML("
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
        tag.map.title, HTML("State of Residence")
      )

      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(-98.483330, 38.712046, zoom = 4) |>
        leaflet::addPolygons(
          data = StatesGeo ,
          fillColor = ~ pal(StatesGeo$total),
          fillOpacity = 0.9,
          weight = 0.2,
          smoothFactor = 0.2,
          #popup = ~popup_sb
          highlight = leaflet::highlightOptions(
            weight = 4,
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
          values = StatesGeo$total,
          position = "bottomright",
          title = "Participants"
        ) |>
        leaflet::addControl(
          title,
          position = "topleft",
          className="map-title"
        )

    },

    updateSummaryConditionClassCounts = function() {

      if(!is.null(self$conditionClasses)) {

        p <- self$ConditionClassData|>
          dplyr::filter(HasCondition == 'True') |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop')


        if(nrow(p[p$Karyotype == "Control",]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tible::tibble(
              Karyotype = 'Control',
              n = 0
            )
          )
        }

        if(nrow(p[p$Karyotype == "Trisomy 21",]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tibble::tibble(
              Karyotype = 'Trisomy 21',
              n = 0
            )
          )
        }

        a <- self$ConditionClassData |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop')

        self$conditionClassSummary <- dplyr::inner_join(p, a, by = c("Karyotype")) |>
          dplyr::mutate(frequency = n.x /n.y) |>
          dplyr::select(-c(n.x,n.y)) |>
          dplyr::mutate(
            frequency = round(frequency,4)*100,
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            ConditionClass = "Conditions Classes Total"
          ) |>
          dplyr::select(ConditionClass,Karyotype,frequency) |>
          tidyr::pivot_wider(names_from = Karyotype, values_from = frequency) |>
          dplyr::select(ConditionClass,`Control %`,`Trisomy 21 %`)

      }

      else {

        p <- self$ConditionClassData |>
          dplyr::filter(HasCondition=='True') |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id),.groups = 'drop')

        if(nrow(p[p$Karyotype=="Control",]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tibble::tibble(
              Karyotype = 'Control',
              n = 0
            )
          )
        }

        if(nrow(p[p$Karyotype == "Trisomy 21",]) == 0) {
          p <- dplyr::bind_rows(
            p,
            tibble::tibble(
              Karyotype = 'Trisomy 21',
              n = 0
              )
            )
        }

        a <- self$ConditionClassData |>
          dplyr::group_by(Karyotype) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop')

        self$conditionClassSummary <- dplyr::inner_join(p, a, by = c("Karyotype")) |>
          dplyr::mutate(frequency = n.x /n.y) |>
          dplyr::select(-c(n.x,n.y)) |>
          dplyr::mutate(
            frequency = round(frequency,4)*100,
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            ConditionClass="Conditions Classes Total"
          ) |>
          dplyr::select(ConditionClass,Karyotype,frequency) |>
          tidyr::pivot_wider(names_from = Karyotype,values_from = frequency) |>
          dplyr::select(ConditionClass,`Control %`,`Trisomy 21 %`)

      }
    },
    getConditionClassSummary = function() {
      return(self$conditionClassSummary)
    },
    updateConditions = function() {

      self$conditions <- self$ConditionClassData |>
        dplyr::filter(ConditionClass %in% self$conditionClasses) |>
        dplyr::select(Condition) |>
        dplyr::distinct() |>
        dplyr::pull()

      p <- self$ConditionClassData |>
        dplyr::filter(
          HasCondition == 'True',
          ConditionClass %in% self$conditionClasses
        ) |>
        dplyr::group_by(Condition,Karyotype) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop')

      a <- self$ConditionClassData |>
        dplyr::filter(ConditionClass %in% self$conditionClasses) |>
        dplyr::group_by(Condition,Karyotype) |>
        dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop')

      self$conditionCounts <- dplyr::inner_join(p, a, by = c("Karyotype","Condition")) |>
        dplyr::mutate(frequency = n.x /n.y) |>
        dplyr::select(-c(n.x,n.y)) |>
        dplyr::mutate(
          frequency = round(frequency,4)*100,
          Karyotype = dplyr::case_when(
            Karyotype == "Control" ~ "Control %",
            TRUE ~ "Trisomy 21 %"
          )
        ) |>
        dplyr::select(Condition,Karyotype,frequency) |>
        tidyr::pivot_wider(names_from = Karyotype,values_from = frequency, values_fill = 0) |>
        dplyr::select(Condition,`Control %`,`Trisomy 21 %`)

    },
    updateSummaryConditionCounts = function() {

      # # #top level condition was chosen
      if(length(self$conditionClasses)>0){

        #and a child condition was chosen
        if(length(self$conditions)>0) {

          #positive diagnoses
          p <- self$ConditionClassData |>
            dplyr::filter(HasCondition == 'True') |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::filter(Condition %in% self$conditions) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id),.groups = 'drop')

          a <- self$ConditionClassData |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::filter(Condition %in% self$conditions) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id),.groups = 'drop')

        }
        # only top level items chosen
        else {

          p <- self$ConditionClassData |>
            dplyr::filter(HasCondition == 'True') |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id),.groups = 'drop')


          a <- self$ConditionClassData |>
            dplyr::filter(!is.na(ConditionClass)) |>
            dplyr::filter(ConditionClass %in% self$conditionClasses) |>
            dplyr::group_by(Karyotype) |>
            dplyr::summarize(n = dplyr::n_distinct(record_id),.groups = 'drop')

        }

        self$conditionSummary <- dplyr::inner_join(p, a, by = c("Karyotype")) |>
          dplyr::mutate(frequency= n.x /n.y) |>
          dplyr::select(-c(n.x,n.y)) |>
          dplyr::mutate(
            frequency = round(frequency,4)*100,
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            Condition="Specific Conditions Total"
          ) |>
          dplyr::select(Condition,Karyotype,frequency) |>
          tidyr::pivot_wider(names_from = Karyotype,values_from = frequency)

      }

      ###nothing was chosen at top level
      else {

        p <- NULL

      }

      if(!is.null(p)){

        ### Some conditions have 0 participants, fill in that value for the pivot below
        if(nrow(p[p$Karyotype == "Control",]) == 0) {
          p <- dplyr::bind_rows(p, tibble::tibble(Karyotype = 'Control',n = 0))
        }

        if(nrow(p[p$Karyotype == "Trisomy 21",]) == 0) {
          p <- dplyr::bind_rows(p, tibble::tibble(Karyotype = 'Trisomy 21',n = 0))
        }

        self$conditionSummary <- dplyr::inner_join(p,a,by=c("Karyotype")) |>
          dplyr::mutate(frequency= n.x /n.y) |>
          dplyr::select(-c(n.x,n.y)) |>
          dplyr::mutate(
            frequency = round(frequency,4)*100,
            Karyotype = dplyr::case_when(
              Karyotype == "Control" ~ "Control %",
              TRUE ~ "Trisomy 21 %"
            ),
            Condition="Specific Conditions Total"
          ) |>
          dplyr::select(Condition,Karyotype,frequency) |>
          tidyr::pivot_wider(names_from = Karyotype,values_from = frequency)

      }
      else {
        self$conditionSummary <- NULL
      }
    },
    updateConditionClassSexCounts = function() {
      if(!is.null(self$conditionClasses)) {

        self$ConditionClassSexCounts <- self$ConditionClassData |>
          dplyr::filter(HasCondition == 'True') |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::group_by(Karyotype,Sex,ConditionClass) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id),.groups = 'drop') |>
          tidyr::pivot_wider(names_from = Sex, values_from = n, values_fill = 0) |>
          dplyr::mutate(
            total = Male + Female,
            Female = Female/total,
            Male = Male/total
          ) |>
          dplyr::rename("Condition" = ConditionClass) |>
          tidyr::pivot_longer(!c(Condition,Karyotype), names_to = c("Sex"), values_to = "Percent") |>
          dplyr::arrange(Condition)
      }
      else {
        self$ConditionClassSexCounts <- NULL
      }
    },
    updateConditionSexCounts = function() {
      if(!is.null(self$conditionClasses) & !is.null(self$conditions)) {

        cartesian <- tidyr::crossing(
          "Sex" = c("Male","Female"),
          "Karyotype" = c("Control","Trisomy 21"),
          "Condition" = self$conditions
        )

        counts <- self$ConditionClassData |>
          dplyr::filter(HasCondition == 'True') |>
          dplyr::filter(ConditionClass %in% self$conditionClasses) |>
          dplyr::filter(Condition %in% self$conditions) |>
          dplyr::group_by(Karyotype, Sex, Condition) |>
          dplyr::summarize(n = dplyr::n_distinct(record_id), .groups = 'drop')

        self$ConditionSexCounts <- cartesian |>
          dplyr::left_join(counts, by = c("Sex","Condition","Karyotype")) |>
          tidyr::replace_na(list("n" = 0)) |>
          tidyr::pivot_wider(names_from = Sex, values_from = n, values_fill = 0) |>
          dplyr::mutate(
            total = Male + Female,
            Female = Female/total,
            Male = Male/total
          ) |>
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ tidyr::replace_na(.,0))) |>
          dplyr::select(Condition = Condition, Karyotype, Female, Male) |>
          tidyr::pivot_longer(!c(Condition,Karyotype), names_to = "Sex", values_to = "Percent") |>
          dplyr::arrange(Condition)

      }
      else {
        self$ConditionSexCounts <- NULL
      }

    },

    getGetConditionStackedSexPlot = function(.data, type = "Classes") {

      if(type == "Classes"){
        .data <- .data |>
          dplyr::filter(Sex != "total")
      }

      .data$Percent <- formattable::percent(.data$Percent)

      d21 <- .data |>
        dplyr::filter(Karyotype == "Control") |>
        dplyr::mutate(Sex = glue::glue('{Sex} {Karyotype}')) |>
        dplyr::arrange(desc(Condition))

      t21 <- .data |>
        dplyr::filter(Karyotype != "Control") |>
        dplyr::mutate(Sex = glue::glue('{Sex} {Karyotype}')) |>
        dplyr::arrange(desc(Condition))

      p1 <- d21 |>
        plotly::plot_ly(
          type = 'bar',
          x = ~ Percent,
          y = ~ Condition,
          color = ~ Sex,
          colors = d21Colors,
          legendgroup = ~ Sex,
          text = ~ glue::glue('{Sex}<br />Condition: {Condition}<br /><i>Percent:</i> {Percent}'),
          hoverinfo = 'text'
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
            showgrid = F,
            title='',
            font = list(
              family = "Arial"
            ),
            tickformat = ".00%",
            range = c(0,1)
          ),
          yaxis = list(
            title = '',
            showgrid = F,
            font = list(
              family = "Arial"
            ),
            type = 'category',
            categoryorder = "category descending"
          ),
          barmode = 'stack',
          shapes = list(
            list(
              type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = 0.5,
              x1 = 0.5,
              line = list(color = "red",dash="dash")
            )
          )
        )

      p2 <- t21 |>
        plotly::plot_ly(
          type = 'bar',
          x = ~ Percent,
          y = ~ Condition,
          color = ~ Sex,
          colors = t21Colors,
          text = ~ glue::glue('{Sex}<br />Condition: {Condition}<br /><i>Percent:</i> {Percent}'),
          hoverinfo = 'text',
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
            title='',
            showgrid = F,
            font = list(
              family = "Arial"
            ),
            tickformat = ".00%",
            range = c(0,1)
          ),
          yaxis = list(
            showgrid = F,
            font = list(
              family = "Arial"
            ),
            type = 'category',
            categoryorder="category descending",
            title = ''
          ),
          barmode = 'stack',
          shapes = list(
            list(
              type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = 0.5,
              x1 = 0.5,
              line = list(color = "red",dash="dash")
            )
          )
        )

      if (nrow(d21) > 0 & nrow(t21) > 0) {
        p <- plotly::subplot(p1, p2, nrows = 1, shareY = T, margin=0.05)
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

      if(type == "Classes") {
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
              filename = glue::glue('{self$applicationName} - Condition Classes by Sex and Karyotype {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
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
              filename = glue::glue('{self$applicationName} - Specific Conditions by Sex and Karyotype {format(Sys.time(),"%Y%m%d_%H%M%S")}') ,
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
    update_upset_plot_data = function() {

      conditionData <- self$localDB$getQuery(
        "SELECT * FROM DiagnosedConditions"
        ) |>
        tidyr::separate_rows(sep = ';','ConditionClass',convert = TRUE) |>
        dplyr::filter(HasCondition == "True") |>
        dplyr::inner_join(
          self$localDB$getQuery(
            "SELECT * FROM AllParticipants"
            ) |>
            dplyr::filter(
              Sex %in% self$Sex,
              AgeAtInitialConsent >= min(self$Age),
              AgeAtInitialConsent <= max(self$Age),
              Karyotype %in% self$Karyotypes,
            ) |>
            dplyr::distinct(record_id)
          , by = "record_id"
        )

      if(self$selectedAnnotationLevel == "Classes of Conditions") {
        conditionData <- conditionData |>
          dplyr::filter(ConditionClass %in% self$SelectedConditions) |>
          dplyr::select(-Condition) |>
          dplyr::rename(Condition = "ConditionClass")
      }
      else {
        conditionData <- conditionData |>
          dplyr::filter(Condition %in% self$SelectedConditions )
      }

      self$upsetPlotData <- conditionData |>
        dplyr::select(record_id,Condition) |>
        dplyr::distinct() |>
        dplyr::mutate(ConditionMember = 1) |>
        tidyr::pivot_wider(names_from = Condition, values_from = ConditionMember, values_fill = 0) |>
        as.data.frame()



    },
    clear_upset_plot_data = function() {
      self$upsetPlotData <- NULL
    }

  )
)
