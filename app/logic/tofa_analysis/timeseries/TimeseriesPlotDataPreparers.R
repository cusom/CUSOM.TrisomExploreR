box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, mutate, mutate_at, group_by, summarise, ungroup, rename_with, rename, lag,
            distinct, n, pull, arrange, dense_rank, row_number, vars, filter, if_else, slice_max,
            inner_join, left_join, right_join, full_join],
    tidyr[drop_na],
    forcats[fct_relevel],
    purrr[pmap, map2_chr],
    stringr[str_split_1, str_replace],
    rlang[sym]
)

#' @export
BaseTimeseriesPreparer <- R6Class(
    "BaseTimeseriesPreparer",
    private = list(),
    active = list(
        feature_label = function(value) {
            return(
                self$source_data |>
                    select(Feature) |>
                    distinct() |>
                    pull()
            )
        },
        units = function(value) {
            return(
                self$source_data |>
                    select(Units) |>
                    distinct() |>
                    pull()
            )
        }
    ),
    public = list(
        feature = NULL,
        source_data = NULL,
        prepared_data = NULL,
        initialize = function(type, dataset, cohort, feature, ...) {
            self$feature <- feature
        },
        set_source_data = function(data) {
            self$source_data <- data
            return(invisible(self$source_data))
        },
        prepare = function(raw_data) {
            self$prepared_data <- self$set_source_data(raw_data) |>
                select(Internal_ParticipantID, Event_Name, Value) |>
                mutate(
                    Value = as.numeric(Value),
                    text = glue(
                        "ParticipantID: {Internal_ParticipantID}
                        Event_Name: {Event_Name}
                        {self$feature_label}: {Value}"
                    )
                )
            return(invisible(self$prepared_data))
        }
    )
)

#' @export
DifferenceTimeseriesPreparer <- R6Class(
    "DifferenceTimeseriesPreparer",
    inherit = BaseTimeseriesPreparer,
    private = list(),
    active = list(),
    public = list(
        initialize = function(type, dataset, cohort, ...) {
            super$initialize(type, dataset, cohort, ...)
        },
        prepare = function(raw_data) {
            self$prepared_data <- self$set_source_data(raw_data) |>
                select(Internal_ParticipantID, Event_Name, Value) |>
                filter(Event_Name == "Baseline") |>
                inner_join(
                    self$source_data |>
                        select(Internal_ParticipantID, Event_Name, Value) |>
                        filter(Event_Name != "Baseline"),
                    by = "Internal_ParticipantID"
                ) |>
                mutate(
                    Value.y = as.numeric(Value.y),
                    Value.x = as.numeric(Value.x),
                    diff = Value.y - Value.x
                ) |>
                select(
                    Internal_ParticipantID, 
                    Event_Name = Event_Name.y,
                    Value = Value.y,
                    diff
                ) |>
                mutate(
                    text = glue(
                        "ParticipantID: {Internal_ParticipantID}
                        Event_Name: {Event_Name}
                        {self$feature_label}: {Value}
                        Difference from Baseline: {diff}"
                    )
                )           
            return(invisible(self$prepared_data))
        }
    )
)