box::use(
    R6[R6Class],
    dplyr[inner_join, join_by, filter],
)


BaseTimeseriesDataSource <- R6Class(
    "BaseTimeseriesDataSource",
    private = list(
        analysis_config = NULL
    ),
    active = list(
        remote_files = function(value) {
            return(private$analysis_config$remote_files)
        }
    ),
    public = list(
        type = NULL,
        dataset = NULL,
        cohort = NULL,
        feature = NULL,
        data = NULL,
        initialize = function(analysis_config, type, dataset, cohort, feature, ...) {
            private$analysis_config <- analysis_config
            self$type <- type
            self$dataset <- dataset
            self$cohort <- cohort
            self$feature <- feature
        },
        get_data = function(...) {
            stop("get_data not implemented for BaseTimeseriesDataSource")
        }
    )
)

#' @export
EndpointsTimeSeriesDataSource <- R6Class(
    "EndpointsTimeSeriesDataSource",
    inherit = BaseTimeseriesDataSource,
    active = list(
        time_series_data = function(value) {
            return(
                self$remote_files$get_experiment_data(self$dataset)
            )
        }
    ),
    public = list(
        initialize = function(analysis_config, type, dataset, cohort, feature, ...) {
            super$initialize(analysis_config, type, dataset, cohort, feature, ...)
        },
        get_data = function(...) {
            # For endpoints, we want to join the cohort to the dataset first, then filter by feature
            self$data <- self$cohort |>
                inner_join(
                    self$time_series_data,
                    join_by(Internal_ParticipantID, TOFA_LabID)
                ) |>
                filter(Feature == self$feature)
            return(invisible(self$data))
        }
    )
)

#' @export
NULISATimeSeriesDataSource <- R6Class(
    "NULISATimeSeriesDataSource",
    inherit = BaseTimeseriesDataSource,
    public = list(
        initialize = function(analysis_config, type, dataset, cohort, feature, ...) {
            super$initialize(analysis_config, type, dataset, cohort, feature, ...)
        },
        get_data = function(...) {
            return(invisible(self$data))
        }
    )
)

#' @export
OLINKTimeSeriesDataSource <- R6Class(
    "OLINKTimeSeriesDataSource",
    inherit = BaseTimeseriesDataSource,
    public = list(
        initialize = function(analysis_config, type, dataset, cohort, feature, ...) {
            super$initialize(analysis_config, type, dataset, cohort, feature, ...)
        },
        get_data = function(...) {
            return(invisible(self$data))
        }
    )
)
