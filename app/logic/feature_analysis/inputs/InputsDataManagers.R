box::use(
    R6[R6Class],
    glue[glue],
    tibble[tibble, as_tibble, enframe],
    dplyr[select, mutate, group_by, summarise, ungroup, rename_with, distinct, n,
            pull, arrange, dense_rank, row_number, filter, bind_rows, case_when,
            n_distinct, if_else],
    purrr[pmap, pluck, set_names],
    stringr[str_split_1, str_c],
    rlang[sym],
    shinyTree[get_selected]
)

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
        Karyotypes = function(value) {
            karyotypes <- self$input_config$karyotypes

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

            # Case 1: Karyotype namespace - simple comparison option
            if (self$namespace == "Karyotype") {
                return(tibble(
                    choiceNames = make_comparison_html(
                        karyotypes,
                        "Test for differences between Trisomy 21 & Controls"
                    ),
                    choiceValues = str_c(karyotypes, collapse = ";")
                ))
            }
            # Case 2: Comorbidity namespace - first karyotype only
            if (self$namespace == "Comorbidity") {
                return(tibble(
                    choiceNames = karyotypes[1],
                    choiceValues = karyotypes[1]
                ))
            }
            # Case 3: Default - calculate counts from StudyData
            karyotype_counts <- self$StudyData |>
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

            # Add comparison option for continuous analysis types
            if (self$analysisType == "Continuous") {
                comparison_row <- tibble(
                    Karyotype = str_c(karyotypes, collapse = ";"),
                    n = NA,
                    sort = 999,
                    choiceNames = make_comparison_html(
                        karyotypes,
                        glue("Test for differences in {self$analysisVariable} \\
                        trajectories between Trisomy 21 & Controls")
                    ),
                    choiceValues = str_c(karyotypes, collapse = ";")
                )
                karyotype_counts <- bind_rows(karyotype_counts, comparison_row) |>
                    arrange(sort)
            }
            return(karyotype_counts)
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
        ConditionChoices = function(value) {
            return(
                self$input_config$ConditionChoices |>
                select(ConditionClass, Condition)
            )
        },
        SelectedConditionList = function(value) {
            return(
                get_selected(self$Conditions, "classid") |>
                    unlist() |>
                    tibble() |>
                    set_names("selected") |>
                    distinct() |>
                    arrange() |>
                    summarise(text = str_c(selected, collapse = "<br />")) |>
                    pull()
            )
        },
        CovariateChoices = function(value) {
            return(
                setdiff(c("Age", "Sex"), self$analysisVariable)
            )
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
        Conditions = NULL,
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

#' @export
FeatureAnalysisInputsManager <- R6Class(
    "FeatureAnalysisInputsManager",
    inherit = InputsManagerBase,
    private = list(),
    active = list(),
    public = list(
        initialize = function(app_config, analysis_config, input_config) {
            super$initialize(app_config, analysis_config, input_config)
        },

        # getGetDataButtonClass = function() {
        # if (is.null(self$Study)) {
        #     return("refresh-btn shinyjs-disabled")
        # } else {
        #     if (self$namespace == "Comorbidity" & is.null(self$Conditions)) {
        #     return("refresh-btn shinyjs-disabled")
        #     } else {
        #     return("refresh-ready-btn shinyjs-enabled")
        #     }
        # }
        # },

        setConditionTreeAttributes = function(tree) {

            # tree <- conditions |>
            #   CUSOMShinyHelpers::dfToTree()

            if (!is.null(self$Conditions)) {
                selected_nodes <- get_selected(self$Conditions, format = "classid") |>
                    unlist() |>
                    as_tibble() |>
                    pull()

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
            return(
                tree
            )
        }
    )
)

#' @export
PreCalculatedFeatureAnalysisInputsManager <- R6Class(
    "PreCalculatedFeatureAnalysisInputsManager",
    inherit = InputsManagerBase,
    private = list(),
    active = list(
        Karyotypes = function(value) {
            if (self$namespace == "Karyotype") {
                return(
                    tibble(
                        choiceNames = glue(
                            '<div>
                                {str_c(self$input_config$karyotypes, collapse = " vs. ")}
                                    <span
                                    data-toggle="tooltip"
                                    data-placement="auto right"
                                    title=""
                                    class="fas fa-info-circle gtooltip info-tooltip"
                                    data-original-title="Test for differences between Trisomy 21 & Controls">
                                    </span>
                            </div>'
                        ),
                        choiceValues = str_c(self$input_config$karyotypes, collapse = ";")
                    )
                )
            } else {
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
                            tibble(
                                Karyotype = str_c(self$input_config$karyotypes, collapse = ","),
                                n = NA,
                                sort = 999,
                                choiceNames =
                                    glue(
                                        '<div>{str_c(self$input_config$karyotypes, collapse = " vs. ")}
                                            <span
                                                data-toggle="tooltip"
                                                data-placement="auto right"
                                                title=""
                                                class="fas fa-info-circle gtooltip info-tooltip"
                                                data-original-title="Test for differences in \\
                                                trajectories between Trisomy 21 & Controls">
                                            </span>
                                        </div>'
                                    ),
                                choiceValues = str_c(self$input_config$karyotypes, collapse = ";")
                            )
                        ) |>
                    arrange(sort)
                )
            }
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
