box::use(
  R6[R6Class],
  yaml[read_yaml],
  tools[file_path_sans_ext]
)

`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
}

read_yaml_required <- function(path, allow_empty = FALSE) {
  if (!file.exists(path)) {
    stop(sprintf("Missing required YAML file: %s", path), call. = FALSE)
  }

  parsed <- read_yaml(path)

  if (is.null(parsed) && !allow_empty) {
    stop(sprintf("YAML file is empty: %s", path), call. = FALSE)
  }

  parsed
}

load_yaml_index <- function(directory) {
  if (!dir.exists(directory)) {
    return(list())
  }

  yaml_files <- list.files(
    path = directory,
    pattern = "\\.(yml|yaml)$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )

  if (length(yaml_files) == 0) {
    return(list())
  }

  items <- lapply(yaml_files, function(x) read_yaml_required(x, allow_empty = TRUE))

  non_empty_index <- which(!vapply(items, is.null, FUN.VALUE = logical(1)))

  if (length(non_empty_index) == 0) {
    return(list())
  }

  yaml_files <- yaml_files[non_empty_index]
  items <- items[non_empty_index]

  item_ids <- vapply(
    seq_along(items),
    function(i) {
      item <- items[[i]]
      if (!is.null(item$id) && nzchar(item$id)) {
        return(item$id)
      }

      file_path_sans_ext(basename(yaml_files[[i]]))
    },
    FUN.VALUE = character(1)
  )

  names(items) <- item_ids
  items
}

#' @export
CatalogRegistry <- R6Class(
  "CatalogRegistry",
  private = list(
    section_directory = function(section_name) {
      section_cfg <- self$manifest$catalog[[section_name]]

      if (!is.null(section_cfg$path) && nzchar(section_cfg$path)) {
        return(file.path(self$catalog_root, section_cfg$path))
      }

      file.path(self$catalog_root, section_name)
    }
  ),
  public = list(
    catalog_root = NULL,
    manifest = NULL,
    analyses = NULL,
    features = NULL,
    datasets = NULL,
    statistics = NULL,
    initialize = function(catalog_root = "app/app_config/catalog") {
      self$catalog_root <- catalog_root
      self$load_catalog()
    },
    load_catalog = function() {
      self$manifest <- read_yaml_required(file.path(self$catalog_root, "manifest.yml"))
      self$analyses <- load_yaml_index(private$section_directory("analyses"))
      self$features <- load_yaml_index(private$section_directory("features"))
      self$datasets <- load_yaml_index(private$section_directory("datasets"))
      self$statistics <- load_yaml_index(private$section_directory("statistics"))

      invisible(self)
    },
    get_analysis = function(analysis_id) {
      analysis <- self$analyses[[analysis_id]]
      if (is.null(analysis)) {
        stop(sprintf("Analysis '%s' not found in catalog.", analysis_id), call. = FALSE)
      }
      analysis
    },
    get_feature = function(feature_id) {
      feature <- self$features[[feature_id]]
      if (is.null(feature)) {
        stop(sprintf("Feature '%s' not found in catalog.", feature_id), call. = FALSE)
      }
      feature
    },
    get_dataset_by_any_id = function(dataset_ref) {
      direct <- self$datasets[[dataset_ref]]

      if (!is.null(direct)) {
        return(direct)
      }

      match_index <- which(vapply(
        self$datasets,
        function(dataset_def) {
          identical(dataset_def$package %||% "", dataset_ref)
        },
        FUN.VALUE = logical(1)
      ))

      if (length(match_index) == 0) {
        stop(sprintf("Dataset '%s' not found in catalog.", dataset_ref), call. = FALSE)
      }

      self$datasets[[match_index[[1]]]]
    },
    get_enabled_features = function(dataset_ref) {
      dataset_def <- self$get_dataset_by_any_id(dataset_ref)
      available <- dataset_def$available_features

      if (is.null(available) || length(available) == 0) {
        return(character(0))
      }

      enabled <- names(available)[vapply(
        available,
        function(feature_cfg) {
          if (is.null(feature_cfg$enabled)) {
            return(TRUE)
          }
          isTRUE(feature_cfg$enabled)
        },
        FUN.VALUE = logical(1)
      )]

      enabled
    },
    get_datasets_for_feature = function(analysis_id, feature_id) {
      analysis <- self$get_analysis(analysis_id)

      if (is.null(analysis$features[[feature_id]])) {
        stop(
          sprintf("Feature '%s' is not configured for analysis '%s'.", feature_id, analysis_id),
          call. = FALSE
        )
      }

      refs <- unlist(analysis$features[[feature_id]]$datasets %||% list(), use.names = FALSE)

      resolved <- lapply(refs, function(ref) {
        self$get_dataset_by_any_id(ref)
      })

      names(resolved) <- vapply(resolved, function(x) x$id %||% x$package %||% "", FUN.VALUE = character(1))
      resolved
    }
  )
)

#' @export
LocalPackageResolver <- R6Class(
  "LocalPackageResolver",
  private = list(
    flatten_char_values = function(x) {
      if (is.null(x)) {
        return(character(0))
      }

      if (is.character(x)) {
        return(x)
      }

      unlist(x, use.names = FALSE)
    },
    normalize_rel_path = function(path) {
      gsub("^\\./", "", path)
    },
    candidate_artifact_paths = function(path) {
      p <- private$normalize_rel_path(path)
      candidates <- c(p)

      if (!grepl("^statistics/", p)) {
        candidates <- c(candidates, file.path("statistics", p))
      }

      base_has_ext <- grepl("\\.[A-Za-z0-9]+$", p)

      if (base_has_ext) {
        no_ext <- sub("\\.[A-Za-z0-9]+$", "", p)
        candidates <- c(candidates, no_ext)
        if (!grepl("^statistics/", no_ext)) {
          candidates <- c(candidates, file.path("statistics", no_ext))
        }
      } else {
        candidates <- c(candidates, paste0(p, ".parquet"), paste0(p, ".csv"))
        if (!grepl("^statistics/", p)) {
          candidates <- c(
            candidates,
            file.path("statistics", paste0(p, ".parquet")),
            file.path("statistics", paste0(p, ".csv"))
          )
        }
      }

      unique(private$normalize_rel_path(candidates))
    },
    first_existing_relative_path = function(package_id, candidates) {
      if (length(candidates) == 0) {
        return(NULL)
      }

      normalized <- unique(private$normalize_rel_path(candidates))
      existing <- normalized[file.exists(file.path(self$packages_root, package_id, normalized))]

      if (length(existing) == 0) {
        return(NULL)
      }

      existing[[1]]
    },
    discover_dim_file = function(package_id, dimension_name) {
      dim_dir <- file.path(self$packages_root, package_id, "dimensions")

      if (!dir.exists(dim_dir)) {
        return(NULL)
      }

      candidates <- list.files(
        path = dim_dir,
        pattern = sprintf("^%s(s)?\\.(parquet|csv)$", dimension_name),
        ignore.case = TRUE,
        full.names = FALSE
      )

      if (length(candidates) == 0) {
        return(NULL)
      }

      file.path("dimensions", candidates[[1]])
    },
    discover_fact_file = function(package_id) {
      fact_dir <- file.path(self$packages_root, package_id, "facts")

      if (dir.exists(fact_dir)) {
        candidates <- list.files(
          path = fact_dir,
          pattern = "^measurement(s)?\\.(parquet|csv)$",
          ignore.case = TRUE,
          full.names = FALSE
        )

        if (length(candidates) > 0) {
          return(file.path("facts", candidates[[1]]))
        }
      }

      data_candidates <- c("data.parquet", "data.csv")
      private$first_existing_relative_path(package_id, data_candidates)
    },
    find_existing_artifact = function(package_id, configured_path) {
      candidates <- private$candidate_artifact_paths(configured_path)
      existing <- candidates[file.exists(file.path(self$packages_root, package_id, candidates))]

      if (length(existing) == 0) {
        return(NULL)
      }

      existing[[1]]
    }
  ),
  public = list(
    packages_root = NULL,
    initialize = function(packages_root = "app/app_config/packages") {
      self$packages_root <- packages_root
    },
    list_packages = function() {
      dirs <- list.dirs(self$packages_root, recursive = FALSE, full.names = FALSE)
      dirs[dirs != ""]
    },
    get_package_manifest = function(package_id) {
      manifest_path <- file.path(self$packages_root, package_id, "manifest.yml")
      read_yaml_required(manifest_path)
    },
    resolve_statistic_support = function(package_id, statistic_id) {
      manifest <- self$get_package_manifest(package_id)
      statistics_cfg <- manifest$statistics %||% list()

      # New schema: statistics.<statistic_id>.implementation = generated|precalculated
      if (!is.null(statistics_cfg[[statistic_id]]) && is.list(statistics_cfg[[statistic_id]]) &&
          !is.null(statistics_cfg[[statistic_id]]$implementation)) {
        impl <- tolower(as.character(statistics_cfg[[statistic_id]]$implementation))
        return(list(
          generated = identical(impl, "generated"),
          precalculated = identical(impl, "precalculated")
        ))
      }

      # Legacy schema fallback.
      generated_cfg <- statistics_cfg$generated %||% list()
      precalculated_cfg <- statistics_cfg$precalculated %||% list()

      list(
        generated = isTRUE(generated_cfg[[statistic_id]]),
        precalculated = statistic_id %in% unlist(precalculated_cfg, use.names = FALSE)
      )
    },
    get_available_statistics = function(package_id) {
      manifest <- self$get_package_manifest(package_id)
      statistics_cfg <- manifest$statistics %||% list()

      # New schema: statistics.<statistic_id>.implementation
      direct_stat_ids <- names(statistics_cfg)[vapply(
        statistics_cfg,
        function(x) {
          is.list(x) && !is.null(x$implementation)
        },
        FUN.VALUE = logical(1)
      )]

      if (length(direct_stat_ids) > 0) {
        return(sort(unique(direct_stat_ids)))
      }

      # Legacy schema fallback.
      generated_ids <- names(which(vapply(statistics_cfg$generated %||% list(), isTRUE, FUN.VALUE = logical(1))))
      precalc_ids <- names(statistics_cfg$precalculated %||% list())

      sort(unique(c(generated_ids, precalc_ids)))
    },
    get_required_files = function(package_id) {
      manifest <- tryCatch(
        self$get_package_manifest(package_id),
        error = function(e) list()
      )

      dimension_names <- names(manifest$dimensions %||% list())
      dimension_files <- vapply(
        dimension_names,
        function(dimension_name) {
          resolved <- self$resolve_dimension_file(package_id, dimension_name)
          if (is.null(resolved)) "" else resolved
        },
        FUN.VALUE = character(1)
      )

      condition_file <- self$resolve_dimension_file(package_id, "conditions")
      if (!is.null(condition_file) && nzchar(condition_file)) {
        dimension_files <- c(dimension_files, conditions = condition_file)
      }

      fact_names <- names(manifest$facts %||% list())
      fact_files <- vapply(
        fact_names,
        function(fact_name) {
          resolved <- self$resolve_fact_file(package_id, fact_name)
          if (is.null(resolved)) "" else resolved
        },
        FUN.VALUE = character(1)
      )

      default_fact <- self$resolve_fact_file(package_id)
      if (!is.null(default_fact) && nzchar(default_fact)) {
        fact_files <- c(fact_files, default = default_fact)
      }

      list(
        dimensions = unique(unname(dimension_files[nzchar(dimension_files)])),
        facts = unique(unname(fact_files[nzchar(fact_files)]))
      )
    },
    resolve_dimension_file = function(package_id, dimension_name) {
      manifest <- tryCatch(
        self$get_package_manifest(package_id),
        error = function(e) list()
      )
      dim_def <- (manifest$dimensions %||% list())[[dimension_name]]

      configured <- dim_def$file %||% ""

      candidates <- c(
        configured,
        file.path("dimensions", paste0(dimension_name, ".parquet")),
        file.path("dimensions", paste0(dimension_name, "s.parquet")),
        file.path("dimensions", paste0(dimension_name, ".csv")),
        file.path("dimensions", paste0(dimension_name, "s.csv"))
      )

      resolved <- private$first_existing_relative_path(package_id, candidates)

      if (!is.null(resolved)) {
        return(resolved)
      }

      private$discover_dim_file(package_id, dimension_name)
    },
    resolve_fact_file = function(package_id, fact_name = "proteomics") {
      manifest <- tryCatch(
        self$get_package_manifest(package_id),
        error = function(e) list()
      )
      fact_def <- (manifest$facts %||% list())[[fact_name]]

      configured <- fact_def$file %||% ""

      candidates <- c(
        configured,
        file.path("facts", "measurements.parquet"),
        file.path("facts", "measurement.parquet"),
        file.path("facts", "measurements.csv"),
        file.path("facts", "measurement.csv"),
        "data.parquet",
        "data.csv"
      )

      resolved <- private$first_existing_relative_path(package_id, candidates)

      if (!is.null(resolved)) {
        return(resolved)
      }

      private$discover_fact_file(package_id)
    },
    resolve_precalculated_artifact = function(package_id, statistic_id = NULL, feature_id = NULL) {
      manifest <- self$get_package_manifest(package_id)
      statistics_cfg <- manifest$statistics %||% list()

      # New schema: statistics.<statistic_id>.file
      if (!is.null(statistic_id) && nzchar(statistic_id)) {
        stat_def <- statistics_cfg[[statistic_id]]
        if (is.list(stat_def) && !is.null(stat_def$file) && nzchar(stat_def$file)) {
          resolved <- private$find_existing_artifact(package_id, stat_def$file)
          if (!is.null(resolved)) {
            return(resolved)
          }
        }
      }

      # Legacy schema fallback.
      precalc_cfg <- statistics_cfg$precalculated %||% list()

      configured_paths <- character(0)

      if (!is.null(statistic_id) && nzchar(statistic_id) && !is.null(precalc_cfg[[statistic_id]])) {
        configured_paths <- private$flatten_char_values(precalc_cfg[[statistic_id]])
      }

      if (length(configured_paths) == 0) {
        configured_paths <- private$flatten_char_values(precalc_cfg)
      }

      configured_paths <- configured_paths[nzchar(configured_paths)]

      if (length(configured_paths) > 0) {
        for (path in configured_paths) {
          resolved <- private$find_existing_artifact(package_id, path)
          if (!is.null(resolved)) {
            return(resolved)
          }
        }
      }

      precalc_dir <- file.path(self$packages_root, package_id, "statistics", "precalculated")

      if (!dir.exists(precalc_dir)) {
        return(NULL)
      }

      candidate_files <- list.files(
        path = precalc_dir,
        pattern = "\\.(parquet|csv)$",
        full.names = FALSE,
        recursive = TRUE,
        ignore.case = TRUE
      )

      if (length(candidate_files) == 0) {
        return(NULL)
      }

      filtered <- candidate_files

      if (!is.null(statistic_id) && nzchar(statistic_id)) {
        filtered <- filtered[grepl(statistic_id, filtered, ignore.case = TRUE)]
      }

      if (!is.null(feature_id) && nzchar(feature_id) && length(filtered) > 0) {
        by_feature <- filtered[grepl(feature_id, filtered, ignore.case = TRUE)]
        if (length(by_feature) > 0) {
          filtered <- by_feature
        }
      }

      if (length(filtered) == 0) {
        return(NULL)
      }

      file.path("statistics", "precalculated", filtered[[1]])
    }
  )
)

#' @export
FeatureAssociationPlanner <- R6Class(
  "FeatureAssociationPlanner",
  public = list(
    catalog_registry = NULL,
    package_resolver = NULL,
    initialize = function(catalog_registry, package_resolver) {
      self$catalog_registry <- catalog_registry
      self$package_resolver <- package_resolver
    },
    create_context = function(
      feature_id,
      dataset_id,
      statistic_id = NULL,
      filters = list(),
      covariates = character(0),
      visualization = list(),
      analysis_id = "feature_association"
    ) {
      list(
        analysis_id = analysis_id,
        feature_id = feature_id,
        dataset_id = dataset_id,
        statistic_id = statistic_id,
        filters = filters,
        covariates = covariates,
        visualization = visualization
      )
    },
    plan = function(context) {
      analysis <- self$catalog_registry$get_analysis(context$analysis_id)

      if (!identical(analysis$id %||% "", "feature_association")) {
        stop("Phase I planner supports only 'feature_association'.", call. = FALSE)
      }

      feature <- self$catalog_registry$get_feature(context$feature_id)
      dataset <- self$catalog_registry$get_dataset_by_any_id(context$dataset_id)

      available_feature_cfg <- analysis$features[[context$feature_id]]
      if (is.null(available_feature_cfg)) {
        stop(
          sprintf("Feature '%s' is not supported by analysis '%s'.", context$feature_id, context$analysis_id),
          call. = FALSE
        )
      }

      allowed_datasets <- unlist(available_feature_cfg$datasets %||% list(), use.names = FALSE)
      dataset_key <- dataset$id %||% ""
      package_key <- dataset$package %||% ""

      if (!(dataset_key %in% allowed_datasets || package_key %in% allowed_datasets)) {
        stop(
          sprintf(
            "Dataset '%s' is not allowed for feature '%s' in analysis '%s'.",
            context$dataset_id,
            context$feature_id,
            context$analysis_id
          ),
          call. = FALSE
        )
      }

      enabled_features <- self$catalog_registry$get_enabled_features(dataset_key %||% package_key)
      if (!(context$feature_id %in% enabled_features)) {
        stop(
          sprintf("Feature '%s' is disabled for dataset '%s'.", context$feature_id, dataset_key %||% package_key),
          call. = FALSE
        )
      }

      package_id <- dataset$package %||% dataset$id

      if (is.null(package_id) || !nzchar(package_id)) {
        stop(sprintf("Dataset '%s' does not declare a package identifier.", context$dataset_id), call. = FALSE)
      }

      package_manifest <- self$package_resolver$get_package_manifest(package_id)

      statistic_id <- context$statistic_id
      execution_mode <- "generated"

      if (!is.null(statistic_id) && nzchar(statistic_id)) {
        support <- self$package_resolver$resolve_statistic_support(package_id, statistic_id)

        if (!support$generated && !support$precalculated) {
          stop(
            sprintf("Statistic '%s' is unavailable for package '%s'.", statistic_id, package_id),
            call. = FALSE
          )
        }

        execution_mode <- if (support$precalculated) "precalculated" else "generated"
      }

      precalculated_artifact <- NULL
      if (identical(execution_mode, "precalculated")) {
        precalculated_artifact <- self$package_resolver$resolve_precalculated_artifact(
          package_id = package_id,
          statistic_id = statistic_id,
          feature_id = context$feature_id
        )
      }

      list(
        analysis_id = context$analysis_id,
        feature_id = feature$id %||% context$feature_id,
        dataset_id = dataset$id %||% context$dataset_id,
        package_id = package_id,
        statistic_id = statistic_id,
        execution_mode = execution_mode,
        precalculated_artifact = precalculated_artifact,
        required_files = self$package_resolver$get_required_files(package_id),
        package_manifest = package_manifest,
        context = context
      )
    }
  )
)
