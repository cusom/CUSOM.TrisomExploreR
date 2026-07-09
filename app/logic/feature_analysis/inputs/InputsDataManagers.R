box::use(
    app/view/custom_ui/input_widgets[dfToTree],
)

box::use(
    R6[R6Class],
    glue[glue],
    tibble[tibble],
    dplyr[select, mutate, group_by, summarise, distinct,
            pull, arrange, row_number, filter, bind_rows,
            n_distinct, if_else, inner_join, join_by, cross_join],
    purrr[pluck, set_names],
    stringr[str_split_1, str_c],
    stats[median],
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

make_collapsed_karyotype_choices <- function(karyo_list, tooltip_text) {
    tibble(
        choiceNames = make_comparison_html(karyo_list, tooltip_text),
        choiceValues = str_c(karyo_list, collapse = ";")
    )
}

# Helper to set shared karyotype sorting/label fields
build_karyotype_choices <- function(.data) {
    .data |>
        mutate(
            sort = if_else(Karyotype == "Trisomy 21", 1, 99),
            choiceNames = glue("{Karyotype} (n={n})"),
            choiceValues = Karyotype
        ) |>
        arrange(sort)
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
                    build_karyotype_choices()
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
        }
    )
)

# karyotype inuts - only show karyotypes collapsed
#' @export
InputsManagerKaryotype <- R6Class(
    "InputsManagerKaryotype",
    inherit = InputsManagerBase,
    active = list(
        Karyotypes = function(value) {
            make_collapsed_karyotype_choices(
                self$input_config$karyotypes,
                "Test for differences between Trisomy 21 & Controls"
            )
        }
    )
)

# karyotype inuts - only show karyotypes collapsed
#' @export
InputsManagerPrecalculatedKaryotype <- R6Class(
    "InputsManagerPrecalculatedKaryotype",
    inherit = InputsManagerBase,
    active = list(
        StudyData = function(value) {
            return(
                self$remote_files$get_pre_calculated_data(self$analysisVariable)
            )
        },
        Karyotypes = function(value) {
            make_collapsed_karyotype_choices(
                self$input_config$karyotypes,
                "Test for differences between Trisomy 21 & Controls"
            )
        },
        StatTestNames = function(value) {
            return(
                "DESeq2 model"
            )
        },
        StatTestValues = function(value) {
            return(
                "linear model"
            )
        },
        AdjustmentMethodNames = function(value) {
            return(
                "Benjamini-Hochberg (FDR)"
            )
        },
        AdjustmentMethodValues = function(value) {
            return(
                "BH"
            )
        }
    )
)

# Age inputs - do not show age as covariate
#' @export
InputsManagerAge <- R6Class(
    "InputsManagerAge",
    inherit = InputsManagerBase,
    active = list(
        CovariateChoices = function(value) {
            return(c("Sex"))
        }
    )
)

# Sex inputs - do not show sex as covariate
#' @export
InputsManagerSex <- R6Class(
    "InputsManagerSex",
    inherit = InputsManagerBase,
    active = list(
        CovariateChoices = function(value) {
            return(c("Age"))
        }
    )
)

# Comorbidity inputs - add comorbidity fields
#' @export
InputsManagerComorbidity <- R6Class(
    "InputsManagerComorbidity",
    inherit = InputsManagerBase,
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
        getConditionTree = function(conditions = NULL) {
            tree <- conditions |>
                dfToTree()

            if (is.null(self$Conditions)) {
                return(tree)
            }

            selected_nodes <- self$get_selected_conditions(self$Conditions) |>
                pull(selected)

            if (length(selected_nodes) == 0) {
                return(tree)
            }

            for (i in seq_along(tree)) {
                if (!is.list(tree[i])) {
                    next
                }

                available_nodes <- names(tree[[i]][[1]])
                for (node in intersect(available_nodes, selected_nodes)) {
                    attr(tree[[i]][[node]], "stselected") <- TRUE
                    attr(tree[[i]][[node]], "stopened") <- TRUE
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
    inherit = InputsManagerBase
)

# cell types - source karyotype counts from remote files, include params field
#' @export
InputsManagerCellTypes <- R6Class(
    "InputsManagerCellTypes",
    inherit = InputsManagerBase,
    active = list(
        Karyotypes = function(value) {
            karyotypes <- self$input_config$karyotypes
            return(
                self$remote_files$get_remote_file_data("input") |>
                    pluck("whole_blood_karyotype_counts") |>
                    as.data.frame() |>
                    build_karyotype_choices() |>
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
    )
)

# TOFA inputs
#' @export
InputsManagerTOFA <- R6Class(
    "InputsManagerTOFA",
    private = list(
        analysis_config = NULL
    ),
    active = list(
        remote_files = function(value) {
            return(private$analysis_config$remote_files)
        },
        Karyotypes = function(value) {
            return(
                self$participant_data |>
                    select(DownSyndromeStatus) |>
                    distinct() |>
                    pull()
            )
        },
        Sexes = function(value) {
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
        Age_Groups = function(value) {
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
        },
        StudyData = function(value) {
            return(
                self$participant_data |>
                inner_join(
                    self$visit_extended_data, join_by(Internal_ParticipantID, External_ParticipantID)
                )
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
        initialize = function(app_config, analysis_config, input_config, dataset, ...) {

            private$analysis_config <- analysis_config
            self$dataset <- dataset
            self$input_config <- input_config
            self$participant_data <- app_config$participant_data
            self$visit_data <- app_config$encounter_data |>
                mutate(
                    Age_at_visit_in_days = as.numeric(Age_at_visit_in_days),
                    Height_cm = as.numeric(Height_cm),
                    Weight_kg = as.numeric(Weight_kg)
                )
        }
    )
)

