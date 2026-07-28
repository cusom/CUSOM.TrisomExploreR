options(box.path = unique(c(
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  getOption("box.path") %||% character(0)
)))

box::use(
  R6[R6Class],
  config[get],
  glue[glue],
  rlang[sym],
  tools[toTitleCase],
  yaml[read_yaml],
  arrow[open_dataset, read_parquet],
  dplyr[select, arrange, distinct, pull, filter, left_join, mutate, 
    group_by, collect, summarise, n, n_distinct, reframe, case_when,
    bind_rows, rename, rename_with],
  tibble[tibble, deframe],
  tidyr[drop_na, separate_rows],
  purrr[pmap, map],
  readr[read_csv]
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

read_local_artifact <- function(path, feature_id = NULL, analyte_id = NULL) {
  normalize_filter_values <- function(values) {
    if (is.null(values) || length(values) == 0) {
      return(character(0))
    }

    values <- unique(as.character(values))
    values[!is.na(values) & nzchar(values)]
  }

  normalize_feature_key <- function(x) {
    tolower(gsub("[^a-zA-Z0-9]+", "", as.character(x)))
  }

  apply_arrow_filters <- function(dataset_obj, analyte_id = NULL) {
    if (is.null(dataset_obj)) {
      return(NULL)
    }

    analyte_values <- normalize_filter_values(analyte_id)

    analyte_col <- intersect(c("AnalyteID", "AnalyteKey", "Analyte", "AnalyteName", "analyte", "feature", "Feature"), names(dataset_obj))

    filtered <- dataset_obj

    if (length(analyte_values) > 0 && length(analyte_col) > 0) {
      analyte_symbol <- sym(analyte_col[[1]])
      filtered <- filtered |> filter(!!analyte_symbol %in% analyte_values)
    }

    tryCatch(collect(filtered), error = function(e) NULL)
  }

  filter_by_feature <- function(data, feature_id = NULL) {
    if (is.null(feature_id) || !nzchar(feature_id) || is.null(data) || nrow(data) == 0) {
      return(data)
    }

    feature_col <- intersect(c("feature", "Feature"), names(data))

    if (length(feature_col) == 0) {
      return(data)
    }

    target <- normalize_feature_key(feature_id)
    matched <- normalize_feature_key(data[[feature_col[[1]]]]) == target
    data[matched, , drop = FALSE]
  }

  filter_by_analyte <- function(data, analyte_id = NULL) {
    if (is.null(analyte_id) || length(analyte_id) == 0 || is.null(data) || nrow(data) == 0) {
      return(data)
    }

    values <- unique(as.character(analyte_id))
    values <- values[!is.na(values) & nzchar(values)]

    if (length(values) == 0) {
      return(data)
    }

    analyte_col <- intersect(c("AnalyteID", "AnalyteKey", "Analyte", "AnalyteName", "analyte", "feature", "Feature"), names(data))

    if (length(analyte_col) == 0) {
      return(data)
    }

    matched <- as.character(data[[analyte_col[[1]]]]) %in% values
    data[matched, , drop = FALSE]
  }

  if (is.null(path) || !nzchar(path) || !(file.exists(path) || dir.exists(path))) {
    return(NULL)
  }

  if (dir.exists(path)) {
    dataset_attempt <- tryCatch(open_dataset(path), error = function(e) NULL)

    if (is.null(dataset_attempt)) {
      return(NULL)
    }

    data <- apply_arrow_filters(
      dataset_attempt,
      analyte_id = analyte_id
    )

    if (!is.null(data)) {
      data <- filter_by_feature(data, feature_id = feature_id)
      return(data)
    }

    data <- tryCatch(collect(dataset_attempt), error = function(e) NULL)
    data <- filter_by_feature(data, feature_id = feature_id)
    return(filter_by_analyte(data, analyte_id = analyte_id))
  }

  if (grepl("\\.parquet$", path, ignore.case = TRUE)) {
    dataset_attempt <- tryCatch(open_dataset(path), error = function(e) NULL)

    if (!is.null(dataset_attempt)) {
      data <- apply_arrow_filters(
        dataset_attempt,
        analyte_id = analyte_id
      )

      if (!is.null(data)) {
        data <- filter_by_feature(data, feature_id = feature_id)
        return(data)
      }
    }
  }

  parquet_attempt <- tryCatch(read_parquet(path), error = function(e) NULL)

  if (!is.null(parquet_attempt)) {
    parquet_attempt <- filter_by_feature(parquet_attempt, feature_id = feature_id)
    return(filter_by_analyte(parquet_attempt, analyte_id = analyte_id))
  }

  csv_attempt <- tryCatch(
    read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )

  csv_attempt <- filter_by_feature(csv_attempt, feature_id = feature_id)
  filter_by_analyte(csv_attempt, analyte_id = analyte_id)
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

      package_root <- self$package_resolver$get_package_root(package_id)
      path <- file.path(package_root, rel_path)

      if (!(file.exists(path) || dir.exists(path))) {
        return(NULL)
      }

      read_local_artifact(path)
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
    ensure_dimension_cache = function(cache_field, dimension_name) {
      if (is.null(self[[cache_field]])) {
        self[[cache_field]] <- private$load_dimension_rows(dimension_name)
      }

      self[[cache_field]] %||% tibble()
    },
    ensure_measurement_cache = function() {
      if (is.null(self$package_measurements_cache)) {
        self$package_measurements_cache <- private$load_measurement_rows()
      }

      self$package_measurements_cache %||% tibble()
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
        field_set <- manifest$group %||% "Catalog Datasets"
        field_set <- trimws(as.character(field_set))
        field_set <- gsub("&copy;", "©", field_set, fixed = TRUE)
        field_set <- gsub("\\s+", " ", field_set)

        tibble(
          Values = dataset_def$id %||% package_id,
          Text = manifest$display_name %||% dataset_def$id %||% package_id,
          URL = manifest$url %||% NA_character_,
          TooltipText = tooltip,
          ShowTooltip = nzchar(tooltip),
          FieldSet = field_set,
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
    },
    build_analysis_config_from_catalog = function() {
      fa <- tryCatch(
        self$catalog_registry$get_analysis("feature_association"),
        error = function(e) NULL
      )

      if (is.null(fa) || length(fa$features %||% list()) == 0) {
        return(tibble(
          ApplicationId                    = character(0),
          Namespace                        = character(0),
          AnalysisVariableName             = character(0),
          AnalysisVariableLabel            = character(0),
          AnalysisType                     = character(0),
          AnalysisVariableBaselineLabel    = character(0),
          AnalysisVolcanoPlotTopAnnotation = character(0)
        ))
      }

      feature_ids <- names(fa$features)

      rows <- lapply(feature_ids, function(feature_id) {
        feature <- tryCatch(
          self$catalog_registry$get_feature(feature_id),
          error = function(e) NULL
        )

        tibble(
          ApplicationId                    = self$application_id,
          Namespace                        = feature_id,
          AnalysisVariableName             = feature$column %||% NA_character_,
          AnalysisVariableLabel            = feature$label %||% feature$display_name %||% NA_character_,
          AnalysisType                     = toTitleCase(feature$data_type %||% ""),
          AnalysisVariableBaselineLabel    = feature$baseline_label %||% NA_character_,
          AnalysisVolcanoPlotTopAnnotation = feature$volcano_top_annotation %||% NA_character_
        )
      })

      bind_rows(rows)
    },
    parse_app_links_from_definition = function() {
      links_list <- self$app_definition$ui$links %||% list()

      if (length(links_list) == 0) {
        return(tibble(
          label                = character(0),
          imageURL             = character(0),
          link                 = character(0),
          IsCurrentApplication = logical(0)
        ))
      }

      rows <- lapply(links_list, function(lnk) {
        tibble(
          label                = lnk$label %||% NA_character_,
          imageURL             = lnk$image_url %||% NA_character_,
          link                 = lnk$link %||% "",
          IsCurrentApplication = isTRUE(lnk$is_current_application)
        )
      })

      bind_rows(rows)
    }
  ),
  active = list(
    inputs = function(value) {
      self$inputs_data
    },
    participant_data = function(value) {
      private$ensure_dimension_cache("participant_data_cache", "participants")
    },
    encounter_data = function(value) {
      private$ensure_dimension_cache("encounter_data_cache", "visits")
    },
    condition_data = function(value) {
      private$ensure_dimension_cache("condition_data_cache", "conditions")
    },
    cell_types_data = function(value) {
      private$ensure_dimension_cache("cell_types_data_cache", "cell_types")
    },
    analytes_data = function(value) {
      private$ensure_dimension_cache("analytes_data_cache", "analytes")
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
    cell_types_data_cache = NULL,
    analytes_data_cache = NULL,
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

      platforms = character(0),
      PlatformExperiments = character(0),
      Queryplatforms = character(0),
      Comparisonplatforms = character(0),
      experimentIDs = character(0),
      studies = data.frame(),
      studiesTibble = tibble(),
      studyChoiceNames = list(),
      studyNames = character(0),
      LabIDs = character(0),
      karyotypes = character(0),
      sexes = character(0),
      ages = integer(0),
      Conditions = character(0),
      ConditionClasses = character(0),
      ConditionChoices = tibble(),
      CellTypes = character(0),
      Genes = character(0),
      Analytes = character(0)
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
      load_participant_data = FALSE,
      load_encounter_data = FALSE,
      load_condition_data = FALSE,
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

      metadata_src  <- app_definition$metadata_source %||% list()
      use_db_ns     <- isTRUE(metadata_src$use_database_namespace_config %||% TRUE)
      use_db_links  <- isTRUE(metadata_src$use_database_application_links %||% TRUE)

      self$remote_db <- ODBCQueryManager$new(
        conn_args = get(file = self$config_file_name, "database")
      )

      self$refresh_local_inputs_cache(
        load_dimensions = FALSE,
        load_measurements = FALSE
      )

      self$app_config$application_id <- application_id

      if (use_db_ns) {
        self$namespace_config <- self$remote_db$getQuery(
          "SELECT * FROM [te].[vw_ApplicationNamespaceConfig]
            WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
            ORDER BY DisplayOrder",
          tibble("ApplicationId" = application_id)
        )

        self$app_config$applicationTitle <- app_definition$ui$application_title %||%
          self$namespace_config$applicationName[1]
        self$app_config$applicationLabel <- app_definition$ui$application_label %||%
          self$namespace_config$applicationLabel[1]
        self$app_config$applicationURL <- app_definition$ui$application_url %||%
          ifelse(
            self$namespace_config$environment[1] == "Production",
            "https://www.trisome.org/explorer",
            "https://www.trisome.org/explorer-internal"
          )

        self$module_config <- self$namespace_config |>
          select(ApplicationId, Namespace, TabText, TabIcon,
          ModuleServerName, UseR6Class, R6ClassName)

        self$analysis_config <- self$namespace_config |>
          select(
            ApplicationId, Namespace,
            AnalysisVariableName, AnalysisVariableLabel, AnalysisType,
            AnalysisVariableBaselineLabel, AnalysisVolcanoPlotTopAnnotation
          )
      } else {
        self$app_config$applicationTitle <- app_definition$ui$application_title %||% ""
        self$app_config$applicationLabel <- app_definition$ui$application_label %||% ""
        self$app_config$applicationURL   <- app_definition$ui$application_url %||%
          "https://www.trisome.org/explorer-internal"
        self$analysis_config <- private$build_analysis_config_from_catalog()
      }

      if (use_db_links) {
        self$app_config$applicationLinks <- self$remote_db$getQuery(
          "SELECT [LinkedApplicationLabel] [label], [LinkedApplicationImageURL][imageURL],
            [LinkedApplicationURL] [link], [IsCurrentApplication]
            FROM [app].[vw_ShinyApplicationApplicationLinks]
            WHERE cast([ApplicationId] as nvarchar(256)) = CAST(? As nvarchar(256))
            ORDER BY LinkDisplayOrder",
          tibble("ApplicationId" = application_id)
        )
      } else {
        self$app_config$applicationLinks <- private$parse_app_links_from_definition()
      }

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

    refresh_local_inputs_cache = function(load_dimensions = TRUE, load_measurements = TRUE) {
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

      if (isTRUE(load_dimensions)) {
        self$participant_data_cache <- private$load_dimension_rows("participants")
        self$encounter_data_cache <- private$load_dimension_rows("visits")
        self$condition_data_cache <- private$load_dimension_rows("conditions")
        self$cell_types_data_cache <- private$load_dimension_rows("cell_types")
        self$analytes_data_cache <- private$load_dimension_rows("analytes")
      }

      if (isTRUE(load_measurements)) {
        self$package_measurements_cache <- private$load_measurement_rows()
      }

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
      age_col <- intersect(c("AgeAtTimeOfVisit", "Age", "Age_at_visit_in_days"), names(self$encounter_data))

      if (nrow(self$encounter_data) == 0 || length(age_col) == 0) {
        self$input_config$ages <- integer(0)
        return(invisible(NULL))
      }

      valid_ages <- self$encounter_data |>
        mutate(.encounter_age = suppressWarnings(as.numeric(.data[[age_col[[1]]]]))) |>
        filter(!is.na(.encounter_age), is.finite(.encounter_age))

      if (nrow(valid_ages) == 0) {
        self$input_config$ages <- integer(0)
        return(invisible(NULL))
      }

      self$input_config$ages <- valid_ages |>
        summarise(
          min = round(min(.encounter_age)),
          max = round(max(.encounter_age)) + 1
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

      id_col <- intersect(c("LabID", "record_id", "TOFA_LabID"), names(self$condition_data))

      if (length(id_col) == 0) {
        self$input_config$Conditions <- character(0)
        self$input_config$ConditionClasses <- character(0)
        self$input_config$ConditionChoices <- tibble()
        return(invisible(NULL))
      }

      has_condition <- rep(TRUE, nrow(self$condition_data))
      if ("HasCondition" %in% names(self$condition_data)) {
        values <- tolower(trimws(as.character(self$condition_data$HasCondition)))
        has_condition <- values %in% c("true", "t", "1", "yes", "y")
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
        mutate(.has_condition = has_condition) |>
        filter(.has_condition) |>
        select(all_of(c(id_col[[1]], "ConditionClass", "Condition"))) |>
        group_by(ConditionClass, Condition) |>
        summarise(n = n_distinct(.data[[id_col[[1]]]]), .groups = "drop") |>
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
      row <- self$analysis_config |>
        filter(
          tolower(Namespace) == tolower(namespace) |
            tolower(AnalysisVariableLabel) == tolower(namespace) |
            tolower(AnalysisVariableName) == tolower(namespace)
        ) |>
        distinct()

      if (nrow(row) > 0) {
        return(row |> mutate(applicationName = self$app_config$applicationTitle))
      }

      analysis_def <- tryCatch(
        self$catalog_registry$get_analysis(tolower(namespace)),
        error = function(e) NULL
      )

      if (is.null(analysis_def)) {
        return(tibble())
      }

      tibble(
        ApplicationId                    = self$application_id,
        Namespace                        = namespace,
        AnalysisVariableName             = NA_character_,
        AnalysisVariableLabel            = analysis_def$display_name %||% namespace,
        AnalysisType                     = toTitleCase(analysis_def$id %||% namespace),
        AnalysisVariableBaselineLabel    = NA_character_,
        AnalysisVolcanoPlotTopAnnotation = NA_character_,
        applicationName                  = self$app_config$applicationTitle
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
        return(private$ensure_measurement_cache())
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

    load_local_package_artifact = function(package_id, rel_path, feature_id = NULL, analyte_id = NULL) {
      if (is.null(package_id) || !nzchar(package_id) || is.null(rel_path) || !nzchar(rel_path)) {
        stop("Package ID and relative artifact path are required.", call. = FALSE)
      }

      package_root <- self$package_resolver$get_package_root(package_id)
      artifact_path <- file.path(package_root, rel_path)
      data <- read_local_artifact(artifact_path, feature_id = feature_id, analyte_id = analyte_id)

      if (is.null(data)) {
        stop(sprintf("Unable to read local artifact '%s' for package '%s'.", rel_path, package_id), call. = FALSE)
      }

      data
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

      self$load_local_package_artifact(package_id, rel_path)
    },

    get_filtered_fact_data = function(package_id, cell_types, analyte_name) {
      if (is.null(package_id) || !nzchar(package_id)) {
        return(tibble())
      }

      fact_rel_path <- self$package_resolver$resolve_fact_file(package_id)

      if (is.null(fact_rel_path) || !nzchar(fact_rel_path)) {
        return(tibble())
      }

      package_root <- self$package_resolver$get_package_root(package_id)
      fact_path <- file.path(package_root, fact_rel_path)

      if (!dir.exists(fact_path) && !file.exists(fact_path)) {
        return(tibble())
      }

      dataset <- tryCatch(open_dataset(fact_path), error = function(e) NULL)

      if (is.null(dataset)) {
        return(tibble())
      }

      # Store filter values as local variables and reference via .env$ so Arrow's
      # lazy expression evaluator captures the full vectors, not just the first element.
      .cell_types  <- as.character(cell_types)
      .analyte_id  <- as.character(analyte_name[[1]])

      filtered <- dataset

      if (length(.cell_types) > 0) {
        filtered <- filtered |> filter(cell_type %in% .env$.cell_types)
      }

      if (nzchar(.analyte_id)) {
        analyte_col <- intersect(
          c("AnalyteName", "Analyte", "Gene", "Gene_name", "Feature", "feature", "analyte"),
          names(dataset)
        )

        if (length(analyte_col) > 0) {
          filtered <- filtered |> filter((!!sym(analyte_col[[1]])) == .env$.analyte_id)
        }
      }

      tryCatch(collect(filtered), error = function(e) tibble())
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
