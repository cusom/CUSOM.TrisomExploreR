box::use(
    R6[R6Class],
    glue[glue, glue_collapse],
    tibble[tibble],
    dplyr[select, filter, between, mutate, group_by, summarise, ungroup, rename_with,
            distinct, n, pull, arrange, dense_rank, row_number, if_else, inner_join,
            case_when],
    tidyr[drop_na],
    forcats[fct_relevel],
    purrr[pmap],
    stringr[str_split, str_c],
    rlang[sym]
)

#' @export
AnalysisInputsDataPreparer <- R6Class(
    "AnalysisInputsDataPreparer",
    private = list(),
    active = list(),
    public = list(
        dataset_data = NULL,
        initialize = function(analysis_config, dataset, ...) {
            self$dataset_data <- analysis_config$dataset_data
        },
        prepare = function(data) {
            # Placeholder for any data preparation steps needed before analysis
            return(data)
        }
    ) 
)