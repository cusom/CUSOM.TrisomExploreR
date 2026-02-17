box::use(
    app/view/custom_ui/input_widgets[dfToTree],
)

box::use(
    R6[R6Class],
    glue[glue],
    tibble[tibble, as_tibble, enframe],
    dplyr[select, mutate, group_by, summarise, ungroup, rename_with, distinct, n,
            pull, arrange, dense_rank, row_number, filter, bind_rows, case_when,
            n_distinct, if_else],
    purrr[pmap, pluck, set_names],
    stringr[str_split_1, str_c],
    stats[median],
    rlang[sym],
    shinyTree[get_selected]
)

# Helper to create HTML tooltip with info icon
make_tooltip <- function(tooltip_text) {
    glue('<span data-toggle="tooltip" data-placement="auto right" ',
        'class="fas fa-info-circle gtooltip info-tooltip" ',
        'data-original-title="{tooltip_text}"></span>')
}

# Helper to create comparison choice HTML
make_comparison_html <- function(karyo_list, tooltip_text) {
    glue('<div>{str_c(karyo_list, collapse = " vs. ")} {make_tooltip(tooltip_text)}</div>')
}

# Helper to create comparison row for karyotype inputs
make_comparison_row <- function(karyo_list, tooltip_text) {
    tibble(
        Karyotype = str_c(karyo_list, collapse = ";"),
        n = NA,
        sort = 999,
        choiceNames = make_comparison_html(karyo_list, tooltip_text),
        choiceValues = str_c(karyo_list, collapse = ";")
    )
}

#' @export
InputsManagerBase <- R6Class(
    "InputsManagerBase",
    private = list(
        app_config = NULL,
        analysis_config = NULL
    ),
    active = list(
        application_id = function(value) {
            return(private$app_config$application_id)
        },
        applicationName = function(value) {
            return(private$analysis_config$ApplicationName)
        },
        namespace = function(value) {
            return(private$analysis_config$Namespace)
        },
        analysisVariable = function(value) {
            return(private$analysis_config$AnalysisVariableName)
        },
        analysisVariableLabel = function(value) {
            return(private$analysis_config$AnalysisVariableLabel)
        },
        analysisType = function(value) {
            return(private$analysis_config$AnalysisType)
        },
        experimentIDs = function(value) {
            return(str_split_1(private$analysis_config$ExperimentIDs, "\\|"))
        },
        remoteDB = function(value) {
            return(private$app_config$remote_db)
        },
        remote_files = function(value) {
            return(private$app_config$remote_files)
        },
        Studies = function(value) {
            return(
                self$input_config$studies |>
                    filter(Values %in% self$experimentIDs)
            )
        },
        StudyLabel = function(value) {
            return(
                self$Studies |>
                    filter(Values == self$Study) |>
                    pull(Text)
            )
        },
        StudyData = function(value) {
            return(
                self$remote_files$get_experiment_data(self$Study)
            )
        },
        KaryotypeCounts = function(value) {
            return(
                self$StudyData |>
                    group_by(Analyte, Karyotype) |>
                    summarise(n = n_distinct(LabID), .groups = "drop") |>
                    group_by(Karyotype) |>
                    summarise(n = round(median(n)), .groups = "drop") |>
                    mutate(
                        sort = if_else(Karyotype == "Trisomy 21", 1, 99),
                        choiceNames = glue("{Karyotype} (n={n})"),
                        choiceValues = Karyotype
                    ) |>
                    arrange(sort)
            )
        },
        Karyotypes = function(value) {
            karyotypes <- self$input_config$karyotypes

            comparison_row <- make_comparison_row(
                karyotypes,
                glue("Test for differences in {self$analysisVariable} trajectories between Trisomy 21 & Controls")
            )
            return(
                bind_rows(self$KaryotypeCounts, comparison_row) |>
                    arrange(sort)
            )
        },
        Sexes = function() {
            return(
                self$input_config$sexes
            )
        },
        Ages = function() {
            return(
                c(min(self$input_config$ages), max(self$input_config$ages))
            )
        },
        CovariateChoices = function(value) {
            return(c("Age", "Sex"))
        },
        StatTestNames = function(value) {
            return(
                self$input_config$statTestschoiceNames
            )
        },
        StatTestValues = function(value) {
            return(
                self$input_config$statTests
            )
        },
        AdjustmentMethodNames = function(value) {
            return(
                self$input_config$adjustmentMethodsNames
            )
        },
        AdjustmentMethodValues = function(value) {
            return(
                self$input_config$adjustmentMethods
            )
        }
    ),
    public = list(
        localDB = NULL,
        input_config = NULL,
        analytesLabel = "Analytes",
        groupBaselineLabel = "",
        FoldChangeVar = "log2FoldChange",
        SignificanceVariable = "-log10pvalue",

        Study = NULL,
        Platform = NULL,
        CellType = NULL,
        Karyotype = NULL,
        Sex = NULL,
        Age = NULL,
        FilterLowCount = NULL,
        StatTest = NULL,
        Covariates = NULL,
        AdjustmentMethod = NULL,
        Adjusted = FALSE,
        SignificanceLabel = "p-value",

        FeatureData = NULL,
        initialize = function(app_config, analysis_config, input_config) {
            private$app_config <- app_config
            private$analysis_config <- analysis_config
            self$input_config <- input_config
        },

        getGetDataButtonClass = function() {
            stop("implement getGetDataButtonClass")
        },

        setConditionTreeAttributes = function(tree) {
            stop("implement setConditionTreeAttributes")
        },

        getDisabledInputClass = function(input_name) {
            if (self$analysisVariable == input_name) {
                return(
                    "shinyjs-disabled"
                )
            }
        },

        getHiddenInputClass = function(input_name) {
            if (self$analysisVariable == input_name) {
                return(
                    "shinyjs-hide"
                )
            }
        },

        addInputSpecialClass = function(input_name, class_name = c("disabled", "hide")) {
            class <- match.arg(class_name)
            if (self$analysisVariable == input_name) {
                return(
                    glue("shinyjs-{class}")
                )
            }
        }
    )
)

# karyotype inuts - only show karyotypes collapsed
#' @export
InputsManagerKaryotype <- R6Class(
    "InputsManagerKaryotype",
    inherit = InputsManagerBase,
    private = list(),
    active = list(
        Karyotypes = function(value) {
            karyotypes <- self$input_config$karyotypes
            return(
                tibble(
                    choiceNames = make_comparison_html(
                        karyotypes,
                        "Test for differences between Trisomy 21 & Controls"
                    ),
                    choiceValues = str_c(karyotypes, collapse = ";")
                )
            )
        }
    ),
    public = list(
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        }
    )
)

# Age inputs - do not show age as covariate
#' @export
InputsManagerAge <- R6Class(
    "InputsManagerAge",
    inherit = InputsManagerBase,
    private = list(),
    active = list(
        CovariateChoices = function(value) {
            return(c("Sex"))
        }
    ),
    public = list(
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        }
    )
)

# Sex inputs - do not show sex as covariate
#' @export
InputsManagerSex <- R6Class(
    "InputsManagerSex",
    inherit = InputsManagerBase,
    private = list(),
    active = list(
        CovariateChoices = function(value) {
            return(c("Age"))
        }
    ),
    public = list(
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        }
    )
)

# Comorbidity inputs - add comorbidity fields
#' @export
InputsManagerComorbidity <- R6Class(
    "InputsManagerComorbidity",
    inherit = InputsManagerBase,
    private = list(),
    active = list(
        Karyotypes = function(value) {
            karyotypes <- self$input_config$karyotypes
            return(
                tibble(
                    choiceNames = karyotypes[1],
                    choiceValues = karyotypes[1]
                )
            )
        },
        ConditionChoices = function(value) {
            return(
                self$input_config$ConditionChoices |>
                    select(ConditionClass, Condition)
            )
        },
        Conditions = function(value) {
            return(
                self$data_source$ConditionChoices
            )
        }
    ),
    public = list(
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        },
        getConditionTree = function(conditions = NULL) {
            tree <- conditions |>
                dfToTree()
            if (!is.null(self$Conditions)) {
                selected_nodes <- shinyTree::get_selected(self$Conditions, format = "classid") |>
                    unlist() |>
                    tibble::as_tibble() |>
                    dplyr::pull()

                if (length(selected_nodes) > 0) {
                    for (i in seq_along(tree)) {
                        if (is.list(tree[i])) {
                            for (node in names(tree[i][[1]])) {
                                if (node %in% selected_nodes) {
                                    attr(tree[[i]][[node]], "stselected") <- TRUE
                                    attr(tree[[i]][[node]], "stopened") <- TRUE
                                }
                            }
                        }
                    }
                }
            }
            return(tree)
        },
        get_selected_conditions = function(conditions) {
            return(
                get_selected(conditions, "classid") |>
                    unlist() |>
                    tibble() |>
                    set_names("selected") |>
                    distinct()
            )
        },
        get_selected_condition_list = function(conditions) {
            return(
                self$get_selected_conditions(conditions) |>
                    arrange() |>
                    summarise(text = str_c(selected, collapse = "<br />")) |>
                    pull()
            )
        }
    )
)

# BMI inputs - no spefici overrides, but create class for future BMI
# specific input handling if needed
#' @export
InputsManagerBMI <- R6Class(
    "InputsManagerBMI",
    inherit = InputsManagerBase,
    private = list(),
    active = list(
    ),
    public = list(
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        }
    )
)

# cell types - source karyotype counts from remote files, include params field
#' @export
InputsManagerCellTypes <- R6Class(
    "InputsManagerCellTypes",
    inherit = InputsManagerBase,
    private = list(),
    active = list(
        Karyotypes = function(value) {
            karyotypes <- self$input_config$karyotypes
            return(
                self$remote_files$get_remote_file_data("input") |>
                    pluck("whole_blood_karyotype_counts") |>
                    as.data.frame() |>
                    mutate(
                        sort = case_when(
                            Karyotype == "Trisomy 21" ~ 1,
                            TRUE ~ 99
                        ),
                        choiceNames = glue("{Karyotype} (n={n})"),
                        choiceValues = Karyotype
                    ) |>
                    bind_rows(
                        make_comparison_row(
                            karyotypes,
                            "Test for differences in trajectories between Trisomy 21 & Controls"
                        )
                    ) |>
                arrange(sort)
            )
        },
        params = function(value) {
            return(
                ifelse(
                    is.null(self$Covariates),
                    "none",
                    str_c(self$Covariates, collapse = ";")
                )
            )
        }
    ),
    public = list(
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        }
    )
)
