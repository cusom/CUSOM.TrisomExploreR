box::use(
    R6[R6Class],
    glue[glue],
    tibble[tibble, as_tibble, enframe],
    dplyr[select, mutate, group_by, summarise, ungroup, rename_with, distinct, n,
            pull, arrange, dense_rank, row_number, filter, bind_rows, case_when,
            n_distinct, if_else, inner_join, left_join, join_by, cross_join, all_of],
    tidyr[drop_na, separate_rows],
    purrr[pmap, pluck, set_names],
    stringr[str_split_1, str_c],
    stats[median],
    rlang[sym]
)

AnalysisInputsManager <- R6Class(
    "AnalysisInputsManager",
    private = list(
        analysis_config = NULL
    ),
    active = list(
        remote_files = function(value) {
            return(private$analysis_config$remote_files)
        },
        karyotype = function(value) {
            return(
                self$participant_data |>
                    select(DownSyndromeStatus) |>
                    distinct() |>
                    pull()
            )
        },
        sexes = function(value) {
            return(
                self$participant_data |>
                    select(Sex) |>
                    distinct() |>
                    pull()
            )
        },
        races = function(value) {
            return(
                self$participant_data |>
                    select(Race) |>
                    distinct() |>
                    pull()
            )
        },
        ethnicities = function(value) {
            return(
                self$participant_data |>
                    select(Ethnicity) |>
                    distinct() |>
                    pull()
            )
        },
        visit_extended_data = function(value) {
            return(
                self$visit_data |>
                    mutate(
                        age_at_visit_in_years = Age_at_visit_in_days / 365,
                        age_group = ifelse(age_at_visit_in_years >= 18, "Adult", "Under 18")
                    )
            )
        },
        events = function(value) {
            return(
                self$visit_data |>
                    select(Event_Name) |>
                    distinct() |>
                    pull()
            )
        },
        event_with_sequence = function(value) {
            return(
                self$visit_data |>
                    select(Event_Name) |>
                    distinct() |>
                    mutate(t = row_number())
            )
        },
        event_comparisons = function(value) {
            return(
                self$event_with_sequence |>
                    cross_join(
                        self$event_with_sequence
                    ) |>
                    filter(
                        Event_Name.x != Event_Name.y,
                        t.y > t.x
                    ) |>
                    select(-c(t.x, t.y)) |>
                    mutate(
                        analysis = glue(
                            "{Event_Name.x} vs {Event_Name.y}"
                        ),
                        events = glue("{Event_Name.x}|{Event_Name.y}")
                    ) |>
                    select(analysis, events)
            )
        },
        age_at_visit = function(value) {
            return(
                self$visit_extended_data |>
                    select(Age_at_visit_in_days) |>
                    distinct() |>
                    pull()
            )
        },
        age_groups = function(value) {
            return(
                self$visit_extended_data |>
                    select(age_group) |>
                    distinct() |>
                    pull()
            )
        },
        conditions = function(value) {
            return(
                self$participant_data |>
                    select("condition" = Qualifying_feature) |>
                    separate_rows(condition, sep = "; ") |>
                    distinct() |>
                    pull()
            )
        },
        features = function(value) {
            return(
                self$remote_files$get_experiment_data(self$dataset) |>
                    select(self$feature_col) |>
                    distinct() |>
                    arrange(.data[[self$feature_col]]) |>
                    pull()
            )
        }
    ),
    public = list(
        input_config = NULL,
        participant_data = NULL,
        visit_data = NULL,
        feature_col = "Feature",
        dataset = NULL,
        filtered_data = NULL,
        initialize = function(analysis_config, dataset, ...) {
            private$analysis_config <- analysis_config
            self$dataset <- dataset
            self$input_config <- analysis_config$input_config
            self$participant_data <- analysis_config$participant_data
            self$visit_data <- analysis_config$encounter_data |>
                mutate(
                    Age_at_visit_in_days = as.numeric(Age_at_visit_in_days),
                    Height_cm = as.numeric(Height_cm),
                    Weight_kg = as.numeric(Weight_kg)
                )
        },
        get_data = function(
            sexes, races, ethnicities, karyotype, age_at_visit, age_groups, conditions = NULL
        ) {
            self$filtered_data <- self$participant_data |>
                filter(
                    # Skip each filter if the corresponding vector is NULL
                    if (!is.null(sexes))              Sex %in% sexes               else TRUE,
                    if (!is.null(races))              Race %in% races              else TRUE,
                    if (!is.null(ethnicities))        Ethnicity %in% ethnicities   else TRUE,
                    if (!is.null(karyotype)) DownSyndromeStatus %in% karyotype else TRUE,
                    # For `conditions`, match on Qualifying_feature only when provided
                    if (!is.null(conditions))
                        replace_na(grepl(conditions, Qualifying_feature, ignore.case = TRUE), FALSE) else TRUE
                ) |>
                inner_join(
                    self$visit_extended_data, join_by(Internal_ParticipantID, External_ParticipantID)
                ) |>
                filter(
                    if (!is.null(age_at_visit))
                        between(Age_at_visit_in_days, age_at_visit[1], age_at_visit[2])
                        else TRUE,
                    if (!is.null(age_groups)) age_group %in% age_groups else TRUE
                ) |>
                select(Internal_ParticipantID, RecordID, TOFA_LabID, HTP_LabID)
            return(invisible(self$filtered_data))
        }
    )
)

#' @export
EndpointsInputsManager <- R6Class(
    "EndpointsInputsManager",
    inherit = AnalysisInputsManager,
    public = list(
        feature_col = "Feature"
    )
)

#' @export
NULISAInputsManager <- R6Class(
    "NULISAInputsManager",
    inherit = AnalysisInputsManager,
    public = list(
        feature_col = "Analyte"
    )
)

#' @export
OLINKInputsManager <- R6Class(
    "OLINKInputsManager",
    inherit = AnalysisInputsManager,
    public = list(
        feature_col = "Analyte"
    )
)
