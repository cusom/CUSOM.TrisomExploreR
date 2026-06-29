box::use(
    R6[R6Class],
    dplyr[inner_join, join_by, filter, mutate, select, distinct, pull, arrange, rename],
)


BaseTimeseriesDataSource <- R6Class(
    "BaseTimeseriesDataSource",
    private = list(
        analysis_config = NULL
    ),
    active = list(
        remote_files = function(value) {
            return(private$analysis_config$remote_files)
        },
        time_series_data = function(value) {
            return(
                self$remote_files$get_experiment_data(self$dataset)
            )
        }
    ),
    public = list(
        visit_data = NULL,
        type = NULL,
        dataset = NULL,
        cohort = NULL,
        feature = NULL,
        data = NULL,
        initialize = function(analysis_config, type, dataset, cohort, feature, ...) {
            private$analysis_config <- analysis_config
            self$visit_data <- analysis_config$encounter_data |>
                mutate(
                    Age_at_visit_in_days = as.numeric(Age_at_visit_in_days),
                    Height_cm = as.numeric(Height_cm),
                    Weight_kg = as.numeric(Weight_kg)
                )
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
    public = list(
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

AnalyteTimeSeriesDataSource <- R6Class(
    "AnalyteTimeSeriesDataSource",
    inherit = BaseTimeseriesDataSource,
    public = list(
        get_data = function(...) {
            self$data <- self$cohort |>
                inner_join(
                    self$time_series_data,
                    join_by(Internal_ParticipantID == record_id, TOFA_LabID == LabID)
                ) |>
                filter(Analyte == self$feature) |>
                inner_join(
                    self$visit_data,
                    join_by(HTP_LabID, TOFA_LabID, Internal_ParticipantID, RecordID)
                ) |>
                rename(
                    Feature = Analyte,
                    Units = Measurement,
                    Value = MeasuredValue
                )
            return(invisible(self$data))
        }
    )
)

#' @export
NULISATimeSeriesDataSource <- R6Class(
    "NULISATimeSeriesDataSource",
    inherit = AnalyteTimeSeriesDataSource
)

#' @export
OLINKTimeSeriesDataSource <- R6Class(
    "OLINKTimeSeriesDataSource",
    inherit = AnalyteTimeSeriesDataSource
)
