`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

first_non_empty <- function(...) {
  values <- list(...)

  for (value in values) {
    if (is.null(value) || length(value) == 0) {
      next
    }

    if (is.character(value) && !nzchar(trimws(value))) {
      next
    }

    return(value)
  }

  NULL
}

parse_args <- function(args) {
  parsed <- list(
    dry_run = FALSE,
    config_file = "config.yml",
    app_configs_file = "app/app_configs.yml",
    app_id = NULL,
    use_renv_lock = FALSE
  )

  if (length(args) == 0) {
    return(parsed)
  }

  for (arg in args) {
    if (identical(arg, "--dry-run")) {
      parsed$dry_run <- TRUE
      next
    }

    if (grepl("^--config=", arg)) {
      parsed$config_file <- sub("^--config=", "", arg)
      next
    }

    if (grepl("^--app-configs=", arg)) {
      parsed$app_configs_file <- sub("^--app-configs=", "", arg)
      next
    }

    if (grepl("^--app-id=", arg)) {
      parsed$app_id <- sub("^--app-id=", "", arg)
      next
    }

    if (identical(arg, "--use-renv-lock")) {
      parsed$use_renv_lock <- TRUE
      next
    }
  }

  parsed
}

sanitize_app_name <- function(x, fallback) {
  if (is.null(x) || !nzchar(x)) {
    return(fallback)
  }

  cleaned <- gsub("[^A-Za-z0-9_-]", "-", x)
  cleaned <- gsub("-+", "-", cleaned)
  cleaned <- gsub("^-|-$", "", cleaned)

  if (!nzchar(cleaned)) fallback else cleaned
}

collect_paths <- function(paths) {
  collected <- character(0)

  for (path in paths) {
    if (!file.exists(path)) {
      next
    }

    if (dir.exists(path)) {
      nested <- list.files(path,
        recursive = TRUE,
        all.files = TRUE,
        no.. = TRUE,
        include.dirs = FALSE
      )

      if (length(nested) > 0) {
        nested <- file.path(path, nested)
        collected <- c(collected, nested)
      }
    } else {
      collected <- c(collected, path)
    }
  }

  unique(gsub("\\\\", "/", collected))
}

stage_app_bundle <- function(files, target_server = "") {
  stage_dir <- file.path(tempdir(), paste0("rsconnect-stage-", as.integer(Sys.time())))
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

  for (file in files) {
    source_file <- file
    target_file <- file.path(stage_dir, file)

    dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)

    if (identical(file, "dependencies.R") && identical(target_server, "shinyapps.io")) {
      lines <- readLines(source_file, warn = FALSE)
      lines <- lines[!grepl("^\\s*library\\(odbc\\)\\s*$", lines)]
      writeLines(lines, target_file)
    } else if (identical(file, "app/logic/app_resources/data_services.R") && identical(target_server, "shinyapps.io")) {
      lines <- readLines(source_file, warn = FALSE)
      lines <- lines[!grepl("^\\s*odbc\\[odbc\\],\\s*$", lines)]
      lines <- gsub("\\bodbc\\(\\),", "getExportedValue(\"odbc\", \"odbc\")(),", lines)
      writeLines(lines, target_file)
    } else {
      copied <- file.copy(source_file, target_file, overwrite = TRUE)
      if (!copied) {
        stop(sprintf("Failed to stage file: %s", file), call. = FALSE)
      }
    }
  }

  stage_dir
}

normalize_app_id <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return("")
  }

  cleaned <- trimws(as.character(x))
  cleaned <- gsub("[{}]", "", cleaned)
  toupper(cleaned)
}

get_target_for_app_id <- function(deploy_targets, app_id) {
  app_id_normalized <- normalize_app_id(app_id)

  if (!nzchar(app_id_normalized)) {
    return(list())
  }

  target_names <- names(deploy_targets)

  if (is.null(target_names) || length(target_names) == 0) {
    return(list())
  }

  target_names_normalized <- vapply(target_names, normalize_app_id, character(1))
  matched_idx <- which(target_names_normalized == app_id_normalized)

  if (length(matched_idx) == 0) {
    return(list())
  }

  deploy_targets[[target_names[[matched_idx[[1]]]]]]
}

build_app_file_manifest <- function(use_renv_lock = FALSE) {
  include_paths <- c(
    "app.R",
    "config.yml",
    "dependencies.R",
    "rhino.yml",
    "app"
  )

  if (isTRUE(use_renv_lock)) {
    include_paths <- c(include_paths, "renv.lock")
  }

  files <- collect_paths(include_paths)

  excluded <- grepl("(^|/)app/logic/legacy_logic(/|$)", files) |
    grepl("(^|/)[^/]*__ignore__[^/]*(/|$)", files) |
    files %in% c("DESCRIPTION", "NAMESPACE", "app/.rscignore")

  files <- files[!excluded]

  required_paths <- c("app.R", "config.yml")
  if (isTRUE(use_renv_lock)) {
    required_paths <- c(required_paths, "renv.lock")
  }
  missing <- required_paths[!required_paths %in% files]
  if (length(missing) > 0) {
    stop(
      sprintf("Required deployment files missing from manifest: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  sort(files)
}

resolve_target <- function(runtime_config, app_meta, app_id) {
  deploy_targets <- runtime_config$deploy_targets %||% list()
  default_target <- deploy_targets$default %||% list()
  app_target <- get_target_for_app_id(deploy_targets, app_id) %||% list()

  target <- modifyList(default_target, app_target)

  app_name_fallback <- paste0("trisomexplorer-", tolower(gsub("[^A-Za-z0-9]", "", app_id)))

  list(
    server = first_non_empty(target$server, "shinyapps.io"),
    host_url = first_non_empty(target$hostUrl, ""),
    account = first_non_empty(target$account, target$username, ""),
    app_id = as.character(first_non_empty(target$appId, "")),
    app_name = sanitize_app_name(
      first_non_empty(target$appName, target$name, app_meta$application_name),
      app_name_fallback
    ),
    app_title = first_non_empty(target$appTitle, target$title, app_meta$application_name, "TrisomExplorer")
  )
}

deploy_rsconnect <- function(dry_run = FALSE,
                             config_file = "config.yml",
                             app_configs_file = "app/app_configs.yml",
                             app_id = NULL,
                             use_renv_lock = FALSE) {
  if (!requireNamespace("config", quietly = TRUE)) {
    stop("Package 'config' is required.", call. = FALSE)
  }

  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("Package 'rsconnect' is required.", call. = FALSE)
  }

  runtime_config <- config::get(file = config_file)

  application_id <- app_id %||% runtime_config$application_id

  if (is.null(application_id) || !nzchar(application_id)) {
    stop("No application_id found. Set it in config.yml or pass --app-id=...", call. = FALSE)
  }

  app_meta <- config::get(file = app_configs_file, config = application_id)
  target <- resolve_target(runtime_config, app_meta, application_id)

  app_files <- build_app_file_manifest(use_renv_lock = use_renv_lock)

  message(sprintf("Application ID: %s", application_id))
  message(sprintf("Server: %s", target$server))
  message(sprintf("Account: %s", target$account))
  message(sprintf("App Name: %s", target$app_name))
  message(sprintf("App Title: %s", target$app_title))
  message(sprintf("Config Target Name: %s", runtime_config$deploy_targets[[application_id]]$name %||% ""))
  if (nzchar(target$app_id)) {
    message(sprintf("App ID: %s", target$app_id))
  }
  message(sprintf("Manifest files: %s", length(app_files)))
  message(sprintf("Using renv.lock: %s", use_renv_lock))

  if (dry_run) {
    cat(paste(app_files, collapse = "\n"), "\n")
    return(invisible(app_files))
  }

  deploy_args <- list(
    appDir = ".",
    appFiles = app_files,
    appName = target$app_name,
    appTitle = target$app_title,
    logLevel = "normal"
  )

  if (nzchar(target$server)) {
    deploy_args$server <- target$server
  }

  if (identical(target$server, "shinyapps.io") && !nzchar(target$account)) {
    stop(
      "For shinyapps.io, account is required. Set deploy_targets.<appId>.account in config.yml or RSCONNECT_ACCOUNT.",
      call. = FALSE
    )
  }

  if (nzchar(target$account)) {
    deploy_args$account <- target$account
  }

  suppressWarnings({
    app_id_num <- as.numeric(target$app_id)
  })

  if (!is.na(app_id_num) && nzchar(target$app_id)) {
    deploy_args$appId <- app_id_num
  }

  stage_dir <- stage_app_bundle(app_files, target_server = target$server)
  deploy_args$appDir <- stage_dir
  deploy_args$appFiles <- app_files
  deploy_args$recordDir <- "."

  on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)

  do.call(rsconnect::deployApp, deploy_args)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  deploy_rsconnect(
    dry_run = args$dry_run,
    config_file = args$config_file,
    app_configs_file = args$app_configs_file,
    app_id = args$app_id,
    use_renv_lock = args$use_renv_lock
  )
}

if (sys.nframe() == 0) {
  main()
}
