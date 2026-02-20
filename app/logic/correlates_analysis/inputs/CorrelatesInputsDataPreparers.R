box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, filter, pull, distinct, arrange],
)

box::use(
    app/logic/feature_analysis/inputs/InputsDataPreparers[
        InputsDataPreparerBase
    ],
)

#' @export
CorrelatesAnalysisInputsPreparer <- R6Class(
    "CorrelatesAnalysisInputsPreparer",
    inherit = InputsDataPreparerBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(analysis_config, app_config) {
            super$initialize(analysis_config, app_config)
        },
        prepare = function(data, study, karyotype, age, sex, params) {
            self$set_prepared_data(data)
        }
    )
)