box::use(
  R6[R6Class],
  config[get],
  glue[glue],
  tools[toTitleCase],
  yaml[read_yaml],
  arrow[read_parquet],
  dplyr[select, arrange, distinct, pull, filter, left_join, mutate, 
    group_by, collect, summarise, n, n_distinct, reframe, case_when,
    bind_rows, rename, rename_with],
  tibble[tibble, deframe],
  tidyr[drop_na, separate_rows],
  purrr[pmap, map]
)

box::use(
  app/logic/shared/global_utils[`%||%`],
  app/logic/shared/ui_utils,
  app/logic/app_resources/data_services[ODBCQueryManager],
  app/logic/app_resources/catalog_services[CatalogRegistry, LocalPackageResolver, FeatureAssociationPlanner]
)

resolve_env_tokens <- function(value) {
  if (!is.character(value) || length(value) != 1 || is.na(value)) {
    return(value)
  }

  matches <- regmatches(value, gregexpr("\\$\\{[A-Za-z_][A-Za-z0-9_]*\\}", value, perl = TRUE))[[1]]

  if (length(matches) == 0 || identical(matches, character(0))) {
    return(value)
  }

  resolved <- value

  for (token in unique(matches)) {
    env_name <- sub("^\\$\\{", "", sub("\\}$", "", token))
    env_value <- Sys.getenv(env_name, unset = token)
    resolved <- gsub(token, env_value, resolved, fixed = TRUE)
  }

  resolved
}

resolve_existing_packages_root <- function(candidate_root, catalog_root = NULL) {
  candidates <- character(0)

  if (!is.null(candidate_root) && nzchar(candidate_root)) {
    candidates <- c(candidates, candidate_root)
  }

  if (!is.null(catalog_root) && nzchar(catalog_root)) {
    app_scoped_root <- file.path(dirname(catalog_root), "packages")
    candidates <- c(candidates, app_scoped_root)
  }

  candidates <- c(candidates, "app/app_config/packages")
  candidates <- unique(candidates[nzchar(candidates)])

  for (path in candidates) {
    if (dir.exists(path)) {
      return(path)
    }
  }

  if (length(candidates) > 0) {
    return(candidates[[1]])
  }

  "app/app_config/packages"
}

sanitize_choice_vector <- function(values) {
  values <- as.character(values)
  values <- trimws(values)
  unique(values[!is.na(values) & nzchar(values)])
}

normalize_application_config <- function(app_definition) {
  runtime <- app_definition$runtime %||% list()
  app_settings <- app_definition$app_settings %||% list()

  list(
    application_name = app_definition$application$name %||% "TrisomExplorer",
    entry_point_path = runtime$entry_point_path %||% "app/",
    entry_point = runtime$entry_point %||% "",
    parent_namespace = runtime$parent_namespace %||% "",
    app_settings_class_module = app_settings$class_module %||% "app/logic/app_resources/app_configs",
    app_settings_class = app_settings$class_name %||% "TrisomExplorerAppManager",
    app_settings_args = app_settings$args %||% list(),
    app_definition = app_definition
  )
}

#' @export
load_application_config <- function(application_id, apps_root = "app/app_config/apps") {
  exact_file <- file.path(apps_root, application_id, "app.yml")

  if (file.exists(exact_file)) {
    app_definition <- read_yaml(exact_file)
    return(normalize_application_config(app_definition))
  }

  config_files <- list.files(
    path = apps_root,
    pattern = "^app\\.yml$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  if (length(config_files) == 0) {
    stop(sprintf("No app.yml files found under '%s'.", apps_root), call. = FALSE)
  }

  matched_file <- NULL

  for (cfg in config_files) {
    cfg_data <- tryCatch(read_yaml(cfg), error = function(e) NULL)

    if (is.null(cfg_data)) {
      next
    }

    cfg_app_id <- cfg_data$application$id %||% ""
    folder_id <- sub(":+$", "", basename(dirname(cfg)))

    if (identical(cfg_app_id, application_id) || identical(folder_id, application_id)) {
      matched_file <- cfg
      break
    }
  }

  if (is.null(matched_file)) {
    stop(sprintf("No app.yml found for application_id '%s'.", application_id), call. = FALSE)
  }

  app_definition <- read_yaml(matched_file)
  normalize_application_config(app_definition)
}

#' @export
create_app_settings <- function(application_id,
                                app_config = NULL,
                                config_file_name = "config.yml") {

  class_name <- "TrisomExplorerAppManager"
  class_module <- "app/logic/app_resources/app_configs"

  if (!is.null(app_config$app_settings_class) && nzchar(app_config$app_settings_class)) {
    class_name <- app_config$app_settings_class
  } else if (!is.null(app_config$app_settings$class_name) && nzchar(app_config$app_settings$class_name)) {
    class_name <- app_config$app_settings$class_name
  }

  if (!is.null(app_config$app_settings_class_module) && nzchar(app_config$app_settings_class_module)) {
    class_module <- app_config$app_settings_class_module
  } else if (!is.null(app_config$app_settings$class_module) && nzchar(app_config$app_settings$class_module)) {
    class_module <- app_config$app_settings$class_module
  }

  class_generator <- NULL

  if (identical(class_name, "TrisomExplorerAppManager") &&
      identical(class_module, "app/logic/app_resources/app_configs")) {
    class_generator <- TrisomExplorerAppManager
  } else {
    eval(parse(text = glue("box::use({class_module}[{class_name}])")), envir = environment())
    class_generator <- get(class_name, envir = environment(), inherits = TRUE)
  }

  init_args <- app_config$app_settings_args

  if (is.null(init_args) && !is.null(app_config$app_settings$args)) {
    init_args <- app_config$app_settings$args
  }

  if (is.null(init_args)) {
    init_args <- list()
  }

  if (!is.list(init_args)) {
    stop("app_settings_args must be a list.", call. = FALSE)
  }

  if (is.null(init_args$application_id)) {
    init_args$application_id <- application_id
  }

  if (is.null(init_args$config_file_name)) {
    init_args$config_file_name <- config_file_name
  }

  if (is.null(init_args$app_definition) && !is.null(app_config$app_definition)) {
    init_args$app_definition <- app_config$app_definition
  }

  do.call(class_generator$new, init_args)
}

#' @export
TrisomExplorerAppManager <- R6Class(
  "TrisomExplorerAppManager",
  private = list(
    read_local_package_parquet = function(package_id, rel_path) {
      if (is.null(package_id) || !nzchar(package_id) || is.null(rel_path) || !nzchar(rel_path)) {
        return(NULL)
      }

      path <- file.path(self$package_resolver$packages_root, package_id, rel_path)

      if (!file.exists(path)) {
        return(NULL)
      }

      tryCatch(
        read_parquet(path),
        error = function(e) NULL
      )
    },
    resolve_package_ids = function(dataset_ids = NULL) {
      if (is.null(dataset_ids) || length(dataset_ids) == 0) {
        dataset_defs <- self$catalog_registry$datasets
      } else {
        dataset_defs <- lapply(dataset_ids, function(dataset_id) {
          self$catalog_registry$get_dataset_by_any_id(dataset_id)
        })
      }

      unique(vapply(
        dataset_defs,
        function(dataset_def) dataset_def$package %||% dataset_def$id %||% "",
        FUN.VALUE = character(1)
      ))
    },
    load_dimension_rows = function(dimension_name, dataset_ids = NULL) {
      package_ids <- private$resolve_package_ids(dataset_ids)

      rows <- map(package_ids, function(package_id) {
        rel_path <- self$package_resolver$resolve_dimension_file(package_id, dimension_name)

        if (is.null(rel_path)) {
          return(NULL)
        }

        data <- private$read_local_package_parquet(package_id, rel_path)

        if (is.null(data)) {
          return(NULL)
        }

        data$PackageID <- package_id
        data
      })

      rows <- rows[!vapply(rows, is.null, FUN.VALUE = logical(1))]

      if (length(rows) == 0) {
        return(tibble())
      }

      bind_rows(rows)
    },
    load_measurement_rows = function(dataset_ids = NULL) {
      package_ids <- private$resolve_package_ids(dataset_ids)

      rows <- map(package_ids, function(package_id) {
        rel_path <- self$package_resolver$resolve_fact_file(package_id)

        if (is.null(rel_path)) {
          return(NULL)
        }

        data <- private$read_local_package_parquet(package_id, rel_path)

        if (is.null(data)) {
          return(NULL)
        }

        data$PackageID <- package_id
        data
      })

      rows <- rows[!vapply(rows, is.null, FUN.VALUE = logical(1))]

      if (length(rows) == 0) {
        return(tibble())
      }

      bind_rows(rows)
    },
    build_study_choices = function() {
      dataset_defs <- self$catalog_registry$datasets

      if (length(dataset_defs) == 0) {
        return(tibble())
      }

      choices <- map(dataset_defs, function(dataset_def) {
        package_id <- dataset_def$package %||% dataset_def$id
        manifest <- tryCatch(
          self$package_resolver$get_package_manifest(package_id),
          error = function(e) list()
        )

        tooltip <- manifest$helper_text %||% ""

        tibble(
          Values = dataset_def$id %||% package_id,
          Text = manifest$display_name %||% dataset_def$id %||% package_id,
          URL = manifest$url %||% NA_character_,
          TooltipText = tooltip,
          ShowTooltip = nzchar(tooltip),
          FieldSet = manifest$group %||% "Catalog Datasets",
          PackageID = package_id,
          Platform = manifest$platform %||% NA_character_
        )
      })

      bind_rows(choices)
    },
    build_default_stat_inputs = function() {
      tibble(
        Text = c("Linear Model", "Wilcoxon test"),
        URL = c(NA_character_, NA_character_),
        TooltipText = c("", ""),
        ShowTooltip = c(FALSE, FALSE)
      )
    },
    build_default_adjustment_inputs = function() {
      tibble(
        Text = c("Benjamini-Hochberg (FDR)", "none"),
        URL = c(NA_character_, NA_character_),
        TooltipText = c("", ""),
        ShowTooltip = c(FALSE, FALSE)
      )
    }
  ),
  active = list(
    inputs = function(value) {
      self$inputs_data
    },
    participant_data = function(value) {
      self$participant_data_cache
    },
    encounter_data = function(value) {
      self$encounter_data_cache
    },
    condition_data = function(value) {
      self$condition_data_cache
    }
  ),
  public = list(
    application_id = NULL,
    config_file_name = NULL,
    app_definition = NULL,
    remote_db = NULL,
    catalog_registry = NULL,
    package_resolver = NULL,
    feature_association_planner = NULL,
    inputs_data = NULL,
    participant_data_cache = NULL,
    encounter_data_cache = NULL,
    condition_data_cache = NULL,
    package_measurements_cache = NULL,
    namespace_config = NULL,

    app_config = list(
      application_id = NULL,
      applicationTitle = NULL,
      applicationLabel = NULL,
      applicationURL = NULL,
      applicationLinks = NULL,
      tutorials = NULL,
      Namespaces = tibble()
    ),

    module_config = NULL,
    analysis_config = NULL,

    input_config = list(
      statTests = c("Linear Model", "Wilcoxon test"),
      statTestTibble = NULL,
      statTestschoiceNames = NULL,
      adjustmentMethods = c("Benjamini-Hochberg (FDR)", "none"),
      adjustmentMethodsTibble = NULL,
      adjustmentMethodsNames = NULL,

      platforms = NULL,
      PlatformExperiments = NULL,
      Queryplatforms = NULL,
      Comparisonplatforms = NULL,
      experimentIDs = NULL,
      studies = NULL,
      studiesTibble = NULL,
      studyChoiceNames = NULL,
      studyNames= NULL,
      LabIDs = NULL,
      karyotypes = NULL,
      sexes = NULL,
      ages = NULL,
      Conditions = NULL,
      ConditionClasses = NULL,
      ConditionChoices = NULL,
      CellTypes = NULL,
      Genes = NULL,
      Analytes = NULL
    ),

    #' @description
    #' Create a new instance of a TrisomExplorerAppManager
    #' @param ApplicationId - string - application id
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(
      application_id,
      config_file_name = "config.yml",
      app_definition = NULL,
      load_inputs = TRUE,
      load_participant_data = TRUE,
      load_encounter_data = TRUE,
      load_condition_data = TRUE,
      clear_data_dir = TRUE
    ) {
      self$application_id <- application_id
      self$config_file_name <- config_file_name

      if (!is.list(app_definition)) {
        app_definition <- list()
      }

      self$app_definition <- app_definition

      data_roots <- app_definition$data_roots %||% list()

      catalog_root <- data_roots$catalog_root %||% "app/app_config/catalog"
      packages_root <- data_roots$packages_root %||% "app/app_config/packages"

      catalog_root <- resolve_env_tokens(catalog_root)
      packages_root <- resolve_env_tokens(packages_root)
      packages_root <- resolve_existing_packages_root(
        candidate_root = packages_root,
        catalog_root = catalog_root
      )

      # Phase I cutover foundation: semantic catalog + local package resolver + planner.
      self$catalog_registry <- CatalogRegistry$new(
        catalog_root = catalog_root
      )

      self$package_resolver <- LocalPackageResolver$new(
        packages_root = packages_root
      )

      self$feature_association_planner <- FeatureAssociationPlanner$new(
        catalog_registry = self$catalog_registry,
        package_resolver = self$package_resolver
      )

      self$remote_db <- ODBCQueryManager$new(
        conn_args = get(file = self$config_file_name, "database")
      )

      self$refresh_local_inputs_cache()

      self$app_config$application_id <- application_id

      self$namespace_config <- self$remote_db$getQuery(
        "SELECT * FROM [te].[vw_ApplicationNamespaceConfig]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY DisplayOrder",
        tibble("ApplicationId" = application_id)
      )

      self$app_config$Namespaces <- self$namespace_config |>
        arrange(DisplayOrder) |>
        select(DisplayOrder, Namespace) |>
        deframe()

      self$app_config$applicationTitle <- self$namespace_config$applicationName[1]
      self$app_config$applicationLabel <-  self$namespace_config$applicationLabel[1]
      self$app_config$applicationURL <- ifelse(
        self$namespace_config$environment[1] == "Production",
        "https://www.trisome.org/explorer",
        "https://www.trisome.org/explorer-internal"
      )

      self$app_config$applicationLinks <- self$remote_db$getQuery(
        "SELECT [LinkedApplicationLabel] [label], [LinkedApplicationImageURL][imageURL],
          [LinkedApplicationURL] [link], [IsCurrentApplication]
          FROM [app].[vw_ShinyApplicationApplicationLinks]
          WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
          ORDER BY LinkDisplayOrder",
        tibble("ApplicationId" = application_id)
      )

      self$module_config <- self$namespace_config |>
        select(ApplicationId, Namespace, TabText, TabIcon,
        ModuleServerName, UseR6Class, R6ClassName)

      self$analysis_config <- self$namespace_config |>
        select(
          ApplicationId, Namespace, ExperimentIDs, UsesPreCalculatedData,
          AnalysisVariableName, AnalysisVariableLabel, AnalysisType,
          AnalysisVariableBaselineLabel, AnalysisVolcanoPlotTopAnnotation
        )

      if (load_inputs) {
        self$load_inputs()
      }

      if (load_participant_data) {
        self$load_participant_data()
      }

      if (load_encounter_data) {
        self$load_encounter_data()
      }

      if (load_condition_data) {
        self$load_condition_data()
      }

    },

    refresh_local_inputs_cache = function() {
      studies <- private$build_study_choices()

      platforms <- studies |>
        select(Platform) |>
        drop_na() |>
        distinct() |>
        pull(Platform)

      self$inputs_data <- list(
        study_choices = studies,
        stat_tests = private$build_default_stat_inputs(),
        adj_methods = private$build_default_adjustment_inputs(),
        platforms = platforms,
        experiment_ids = studies |>
          select(Values) |>
          distinct() |>
          pull(Values)
      )

      self$participant_data_cache <- private$load_dimension_rows("participants")
      self$encounter_data_cache <- private$load_dimension_rows("visits")
      self$condition_data_cache <- private$load_dimension_rows("conditions")
      self$package_measurements_cache <- private$load_measurement_rows()

      invisible(self)
    },

    load_inputs = function() {
      self$input_config$studies <- self$inputs$study_choices |>
          as.data.frame()

      self$input_config$statTestschoiceNames <- pmap(
        self$inputs$stat_tests,
        ui_utils$createTooltip
      )

      self$input_config$adjustmentMethodsNames <- pmap(
        self$inputs$adj_methods,
        ui_utils$createTooltip
      )

      self$input_config$platforms <- self$inputs$platforms

      self$input_config$experimentIDs <- self$inputs$experiment_ids
    },

    load_participant_data = function() {
      if (nrow(self$participant_data) == 0) {
        self$input_config$karyotypes <- character(0)
        self$input_config$sexes <- character(0)
        return(invisible(NULL))
      }

      self$input_config$karyotypes <- self$participant_data |>
        distinct(Karyotype) |>
        pull() |>
        sanitize_choice_vector()

      self$input_config$sexes <- self$participant_data |>
        distinct(Sex) |>
        pull() |>
        sanitize_choice_vector()
    },

    load_encounter_data = function() {
      if (nrow(self$encounter_data) == 0 || !"AgeAtTimeOfVisit" %in% names(self$encounter_data)) {
        self$input_config$ages <- integer(0)
        return(invisible(NULL))
      }

      valid_ages <- self$encounter_data |>
        drop_na(AgeAtTimeOfVisit)

      if (nrow(valid_ages) == 0) {
        self$input_config$ages <- integer(0)
        return(invisible(NULL))
      }

      self$input_config$ages <- valid_ages |>
        summarise(
          min = round(min(AgeAtTimeOfVisit)),
          max = round(max(AgeAtTimeOfVisit)) + 1
        ) |>
        reframe(
          age = seq(min, max, 1)
        ) |>
        pull()
    },

    load_condition_data = function() {
      if (nrow(self$condition_data) == 0) {
        self$input_config$Conditions <- character(0)
        self$input_config$ConditionClasses <- character(0)
        self$input_config$ConditionChoices <- tibble()
        return(invisible(NULL))
      }

      self$input_config$Conditions <- self$condition_data |>
        distinct(Condition) |>
        pull()

      self$input_config$ConditionClasses <- self$condition_data |>
        distinct(ConditionClass) |>
        separate_rows(sep = ";", "ConditionClass", convert = TRUE) |>
        drop_na() |>
        select(ConditionClass) |>
        distinct() |>
        pull()

      self$input_config$ConditionChoices <- self$condition_data |>
        filter(HasCondition == "True") |>
        select(LabID, ConditionClass, Condition) |>
        select(LabID, ConditionClass, Condition) |>
        group_by(ConditionClass, Condition) |>
        summarise(n = n_distinct(LabID), .groups = "drop")  |>
        filter(n >= 5) |>
        left_join(
          self$condition_data |>
            filter(!is.na(ConditionCensorshipAgeGroup)) |>
            distinct(Condition, ConditionCensorshipAgeGroup)

          , by = "Condition"
        ) |>
        mutate(
          AgeCensor = case_when(
            !is.na(ConditionCensorshipAgeGroup) ~ ConditionCensorshipAgeGroup,
            TRUE ~ ""
          )
        ) |>
        select(-ConditionCensorshipAgeGroup, n) |>
        separate_rows(sep = ";", "ConditionClass", convert = TRUE)
    },

    get_module_config = function(namespace) {
      self$app_module_config |>
          filter(tolower(Namespace) == tolower(namespace))
    },

    get_analysis_config = function(namespace) {
      return(
        self$analysis_config |>
          mutate(applicationName = self$app_config$applicationTitle) |>
          filter(
            tolower(Namespace) == tolower(namespace) |
              tolower(AnalysisVariableLabel) == tolower(namespace) |
              tolower(AnalysisVariableName) == tolower(namespace)
          ) |>
          distinct()
      )
    },

    get_input_config = function(namespace) {
      return(self$input_config)
    },

    get_catalog_analysis_definition = function(analysis_id = "feature_association") {
      self$catalog_registry$get_analysis(analysis_id)
    },

    get_catalog_feature_definition = function(feature_id) {
      self$catalog_registry$get_feature(feature_id)
    },

    get_catalog_dataset_definition = function(dataset_id) {
      self$catalog_registry$get_dataset_by_any_id(dataset_id)
    },

    get_catalog_feature_datasets = function(feature_id, analysis_id = "feature_association") {
      datasets <- self$catalog_registry$get_datasets_for_feature(analysis_id, feature_id)

      tibble(
        dataset_id = vapply(datasets, function(x) x$id %||% NA_character_, FUN.VALUE = character(1)),
        package_id = vapply(datasets, function(x) x$package %||% NA_character_, FUN.VALUE = character(1))
      )
    },

    get_dataset_statistic_ids = function(dataset_id) {
      dataset <- self$get_catalog_dataset_definition(dataset_id)
      package_id <- dataset$package %||% dataset$id

      self$package_resolver$get_available_statistics(package_id)
    },

    get_package_measurements_data = function(dataset_ids = NULL) {
      if (is.null(dataset_ids) || length(dataset_ids) == 0) {
        return(self$package_measurements_cache %||% tibble())
      }

      requested <- unique(as.character(dataset_ids))

      package_rows <- lapply(requested, function(dataset_id) {
        dataset <- self$catalog_registry$get_dataset_by_any_id(dataset_id)
        package_id <- dataset$package %||% dataset$id

        rel_path <- self$package_resolver$resolve_fact_file(package_id)
        if (is.null(rel_path)) {
          return(NULL)
        }

        data <- private$read_local_package_parquet(package_id, rel_path)

        if (is.null(data)) {
          return(NULL)
        }

        data$PackageID <- package_id
        data
      })

      package_rows <- package_rows[!vapply(package_rows, is.null, FUN.VALUE = logical(1))]

      if (length(package_rows) == 0) {
        return(tibble())
      }

      bind_rows(package_rows)
    },

    get_local_dataset_data = function(dataset_id) {
      package_id <- dataset_id

      if (!is.null(self$catalog_registry$datasets[[dataset_id]])) {
        dataset <- self$catalog_registry$get_dataset_by_any_id(dataset_id)
        package_id <- dataset$package %||% dataset$id
      }

      rel_path <- self$package_resolver$resolve_fact_file(package_id)

      if (is.null(rel_path)) {
        stop(sprintf("No local fact file found for dataset '%s'.", dataset_id), call. = FALSE)
      }

      data <- private$read_local_package_parquet(package_id, rel_path)

      if (is.null(data)) {
        stop(sprintf("Unable to read local fact file '%s' for dataset '%s'.", rel_path, dataset_id), call. = FALSE)
      }

      data
    },

    plan_feature_association = function(
      feature_id,
      dataset_id,
      statistic_id = NULL,
      filters = list(),
      covariates = character(0),
      visualization = list()
    ) {
      context <- self$feature_association_planner$create_context(
        feature_id = feature_id,
        dataset_id = dataset_id,
        statistic_id = statistic_id,
        filters = filters,
        covariates = covariates,
        visualization = visualization,
        analysis_id = "feature_association"
      )

      self$feature_association_planner$plan(context)
    }
  )
)


#' @export
TOFAAppManager <- R6Class(
  "TOFAAppManager",
  inherit = TrisomExplorerAppManager,
  private = list(
    tofa_analysis_id = "tofa_feature_association",
    tofa_feature_id = "timepoint",
    build_tofa_dataset_choices = function() {
      analysis <- self$catalog_registry$get_analysis(private$tofa_analysis_id)
      feature_cfg <- analysis$features[[private$tofa_feature_id]]

      if (is.null(feature_cfg)) {
        return(tibble(Values = character(0), Text = character(0), URL = character(0), TooltipText = character(0), ShowTooltip = logical(0), FieldSet = character(0)))
      }

      dataset_refs <- unlist(feature_cfg$datasets %||% list(), use.names = FALSE)

      if (length(dataset_refs) == 0) {
        return(tibble(Values = character(0), Text = character(0), URL = character(0), TooltipText = character(0), ShowTooltip = logical(0), FieldSet = character(0)))
      }

      rows <- lapply(dataset_refs, function(ref) {
        dataset_def <- self$catalog_registry$get_dataset_by_any_id(ref)
        package_id <- dataset_def$package %||% dataset_def$id
        manifest <- tryCatch(
          self$package_resolver$get_package_manifest(package_id),
          error = function(e) list()
        )

        tooltip <- manifest$helper_text %||% ""

        tibble(
          Values = dataset_def$id %||% package_id,
          Text = manifest$display_name %||% dataset_def$id %||% package_id,
          URL = manifest$url %||% NA_character_,
          TooltipText = tooltip,
          ShowTooltip = nzchar(tooltip),
          FieldSet = manifest$group %||% "TOFA Datasets"
        )
      })

      bind_rows(rows)
    },
    build_tofa_analysis_config = function() {
      analysis_def <- self$catalog_registry$get_analysis(private$tofa_analysis_id)
      feature_def <- self$catalog_registry$get_feature(private$tofa_feature_id)

      analysis_type <- case_when(
        identical(tolower(feature_def$data_type %||% ""), "categorical") ~ "Categorical",
        identical(tolower(feature_def$data_type %||% ""), "continuous") ~ "Continuous",
        TRUE ~ "Categorical"
      )

      tibble(
        Namespace = toTitleCase(private$tofa_feature_id),
        ExperimentIDs = paste(self$datasets$Values, collapse = "|"),
        UsesPreCalculatedData = TRUE,
        AnalysisVariableName = feature_def$column %||% "Event_Name",
        AnalysisVariableLabel = feature_def$display_name %||% "Event",
        AnalysisType = analysis_type,
        AnalysisVariableBaselineLabel = "Baseline",
        AnalysisVolcanoPlotTopAnnotation = "Up Compared to Baseline",
        ApplicationName = self$app_config$applicationTitle
      )
    }
  ),
  active = list(
    datasets = function(value) {
      tryCatch(
        private$build_tofa_dataset_choices(),
        error = function(e) {
          tibble(Values = character(0), Text = character(0), URL = character(0), TooltipText = character(0), ShowTooltip = logical(0), FieldSet = character(0))
        }
      )
    },
    all_data = function(value) {
      return(
          self$participant_data |>
              inner_join(self$encounter_data, join_by(Internal_ParticipantID, External_ParticipantID)) |>
              left_join(self$datasets, join_by(Internal_ParticipantID, External_ParticipantID,
                  RecordID, TOFA_LabID, HTP_LabID, External_VisitID, Event_Name))
      )
    }
  ),
  public = list(
    initialize = function(app_config) {
      
      super$initialize(
        application_id = app_config$application_id,
        config_file_name = "config.yml",
        app_definition = if (!is.null(app_config$app_definition)) app_config$app_definition else NULL,
        load_inputs = TRUE,
        load_participant_data = FALSE,
        load_encounter_data = FALSE,
        load_condition_data = FALSE,
        clear_data_dir = FALSE
      )

      self$analysis_config <- private$build_tofa_analysis_config()

    },
    get_analysis_config = function(namespace) {
      return(
        self$analysis_config
      )
    },
    load_participant_data = function() {
      self$input_config$karyotypes <- self$participant_data |>
        distinct(DownSyndromeStatus) |>
        pull() |>
        sanitize_choice_vector()

      self$input_config$sexes <- self$participant_data |>
        distinct(Sex) |>
        pull() |>
        sanitize_choice_vector()
    },
    load_encounter_data = function() {
      self$input_config$ages <- self$encounter_data |>
        drop_na() |>
        summarise(
          min = round(min(Age_at_visit_in_days)),
          max = round(max(Age_at_visit_in_days)) + 1
        ) |>
        reframe(
          age = seq(min, max, 1)
        ) |>
        pull()
    }
  )
)
