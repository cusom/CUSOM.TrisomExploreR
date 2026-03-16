box::use(
    R6[R6Class],
    dplyr[inner_join, join_by, filter],
)

#' @export 
BaseTimeseriesDataSource <- R6Class(
    "BaseTimeseriesDataSource",
    active = list(),
    public = list(
        type = NULL,
        dataset = NULL,
        cohort = NULL,
        feature = NULL,
        data = NULL,
        initialize = function(type, dataset, cohort, feature, ...) {
            self$type <- type
            self$dataset <- dataset
            self$cohort <- cohort
            self$feature <- feature
        },
        get_data = function(...) {
            self$data <- self$cohort |>
                inner_join(
                    self$dataset |>
                        filter(Feature == self$feature) ,
                    join_by(Internal_ParticipantID, TOFA_LabID)
                ) 
            return(invisible(self$data))
        }
    )
)
