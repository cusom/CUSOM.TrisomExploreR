box::use(
  R6[R6Class],
  DBI[dbConnect, dbDisconnect, dbSendQuery, dbBind, dbFetch, dbClearResult, Id, dbAppendTable, dbGetQuery],
  odbc[odbc],
  dplyr[mutate_if, mutate, across, filter, distinct, pull, arrange, case_when, select, bind_rows],
  tidyr[pivot_longer, separate],
  tidyselect[everything],
  stringr[str_detect, str_split, str_extract],
  glue[glue, glue_sql],
  AzureStor[storage_endpoint, storage_container, list_blobs, storage_download],
  purrr[map]
)

#' @export
ODBCConnectionManager <- R6Class(
  "ODBCConnectionManager",
  private = list(
    conn_args = NULL,
    dbhandle = NULL
  ),
  public = list(
    connection_open = FALSE,
    initialize = function(conn_args) {
      private$conn_args <<- conn_args
    },
    connect = function() {
      conn_args <- private$conn_args
      before <- getTaskCallbackNames()
      private$dbhandle <- dbConnect(
        odbc(),
        Driver   = conn_args$driver,
        Server   = conn_args$server,
        Database = conn_args$database,
        UID      = conn_args$uid,
        PWD      = conn_args$pwd,
        Port     = conn_args$port,
        TDS_Version = conn_args$tds_version # added for shinyapps.io
      )
      after <- getTaskCallbackNames()
      removeTaskCallback(which(!after %in% before))
      self$connection_open <- TRUE
    },

    disconnect = function() {
      dbDisconnect(private$dbhandle)
      self$connection_open <- FALSE
    }
  )
)

#' @export
ODBCQueryManager <- R6Class(
  "ODBCQueryManager",
  inherit = ODBCConnectionManager,
  private = list(
    setParameters = function(parameters) {
      if (!is.null(parameters)) {
        self$parameters <- parameters |>
          mutate_if(is.factor, as.character)
      } else {
        self$parameters <- parameters
      }

    },
    formatData = function() {
      if (self$convertFactorsToStrings) {
        self$data <- self$data |>
          mutate_if(is.factor, as.character)
      }
    }
  ),
  public = list(
    queryString = NULL,
    parameters = NULL,
    convertFactorsToStrings = TRUE,
    data = NULL,
    initialize = function(conn_args) {
      super$initialize(conn_args)
    },

    getQuery = function(queryString, parameters, convertFactorsToStrings = TRUE) {

      self$queryString <- queryString

      private$setParameters(parameters)

      self$convertFactorsToStrings <- convertFactorsToStrings

      self$connect()

      query <- dbSendQuery(private$dbhandle, self$queryString)

      dbBind(query, self$parameters)

      self$data <- dbFetch(query)

      dbClearResult(query)

      self$disconnect()

      private$formatData()

      return(self$data)

    },

    insertData = function(table_name, values) {

      stopifnot(class(table_name) %in% c("character"))
      stopifnot(any(class(values) %in% c("tbl_df", "tbl", "data.frame", "tibble", "tribble")))

      self$connect()

      tryCatch({

        if (str_detect(table_name, ".")) {

          table_names <- str_split(table_name, pattern = "\\.", simplify = TRUE)

          table_id <- Id(
            schema = table_names[1],
            table = table_names[2]
          )

          dbAppendTable(
            conn = private$dbhandle,
            name = table_id,
            value = values
          )
        } else {
          dbAppendTable(
            conn = private$dbhandle,
            name = table_name,
            value = values
          )
        }
      }, error = function(e) {
        print(glue("an error occured {e}"))
      })

      self$disconnect()

      return(TRUE)
    }

  )
)

SQLiteConnectionManager <- R6Class(
  "SQLiteConnectionManager",
  private = list(
    filepath = NULL,
    dbhandle = NULL
  ),
  public = list(
    connection_open = FALSE,

    initialize = function(filepath) {
      .Deprecated(new = "N/A", old = "SQLiteConnectionManager")
    },

    connect = function() {
      .Deprecated(new = "N/A", old = "SQLiteConnectionManager")
    },

    disconnect = function() {
      .Deprecated(new = "N/A", old = "SQLiteConnectionManager")
    }
  )
)

SQLiteQueryManager <- R6Class(
  "SQLiteQueryManager",
  inherit = SQLiteConnectionManager,
  private = list(

    set_parameters = function(parameters, e) {

      if (!is.null(parameters)) {

        self$parameters <- parameters |>
          mutate(across(everything(), as.character)) |>
          pivot_longer(cols = everything())

        # create / load env. object per parameter name / values
        sapply(
          unique(self$parameters$name),
          function(param_name) {
            param_vals <- self$parameters |>
              filter(name == param_name) |>
              distinct() |>
              pull()
            assign(
              param_name,
              param_vals,
              envir = e
            )
          }
        )

      } else {
        self$parameters <- parameters
      }
    },
    clear_parameters = function(e) {

      vals <- self$parameters$name
      rm(vals, envir = e)

    }
  ),
  public = list(
    queryString = NULL,
    parameters = NULL,
    data = NULL,

    initialize = function(filepath) {
      .Deprecated(new = "N/A", old = "SQLiteQueryManager")
    },

    getQuery = function(queryString, parameters = NULL) {

      self$queryString <- queryString

      self$connect()

      if (is.null(parameters)) {
        self$data <- dbGetQuery(private$dbhandle, self$queryString)
      } else {

        e <- new.env()

        private$set_parameters(parameters, e)

        q <- glue_sql(
          self$queryString,
          .con = private$dbhandle,
          .envir = e
        )

        pq <- dbSendQuery(private$dbhandle, q)

        self$data <- dbFetch(pq)

        dbClearResult(pq)

      }

      self$disconnect()

      return(self$data)

    }
  )
)

#' @export
AzureRemoteDataFileManager <- R6Class(
  "AzureRemoteDataFileManager",
  private = list(
    account_name = "",
    key = "",
    container_name = "",
    endpoint = NULL,
    container = NULL,
    resolve_target_file = function(candidates, query_context = "target") {
      if (length(candidates) == 0) {
        stop(glue("No files matched for {query_context}."), call. = FALSE)
      }

      if (length(candidates) > 1) {
        warning(
          glue("Multiple files matched for {query_context}; using the first match: {candidates[[1]]}"),
          call. = FALSE
        )
      }

      return(candidates[[1]])
    }
  ),
  active = list(
    uri = function(value) {
      return(
        glue("https://{private$account_name}.blob.core.windows.net")
      )
    },
    file_type = function(value) {
      return(
        self$blobs |>
          filter(name == self$targeted_file) |>
          pull(file_type)
      )
    },
    local_file_exists = function(value) {
      if (length(self$targeted_file) != 1 || is.na(self$targeted_file)) {
        return(FALSE)
      }

      return(
        any(
          grepl(
            self$targeted_file,
            list.files(self$local_data_directory, recursive = TRUE)
          )
        )
      )
    },
    local_file_path = function(value) {
      if (length(self$targeted_file) != 1 || is.na(self$targeted_file)) {
        stop("targeted_file must resolve to a single file path.", call. = FALSE)
      }

      return(
        glue("{self$local_data_directory}/{self$targeted_file}")
      )
    },
    file_read_method = function(value) {
      if (!self$file_type %in% names(self$file_type_read_method_map)) {
        return("unknown")
      }

      self$file_type_read_method_map[[self$file_type]]
    },
    default_read_method_args = function(value) {
      if (!self$file_read_method %in% names(self$read_method_default_args_map)) {
        return(list())
      }

      self$read_method_default_args_map[[self$file_read_method]]
    }
  ),
  public = list(
    local_data_directory = NULL,
    download_mode = NULL,
    files_downloaded = FALSE,
    targeted_file = NULL,
    blobs = NULL,
    file_type_read_method_map = list(
      json = "jsonlite::fromJSON",
      parquet = "arrow::read_parquet",
      txt = "readr::read_delim",
      csv = "readr::read_delim"
    ),
    read_method_default_args_map = list(
      "readr::read_delim" = list(show_col_types = FALSE, progress = FALSE)
    ),
    initialize = function(account_name, key, container_name,
      download_mode = c("on demand", "all"), local_data_directory = file.path(tempdir(), "TrisomExploreR_remote_cache"), 
      clear_data_dir = TRUE) {
      download_mode <- match.arg(download_mode)
      private$account_name <- account_name
      private$key <- key
      private$container_name <- container_name
      self$download_mode <- download_mode
      self$local_data_directory <- local_data_directory
      private$endpoint <- storage_endpoint(self$uri, private$key)
      private$container <- storage_container(private$endpoint, private$container_name)
      if (clear_data_dir) {
        unlink(self$local_data_directory, recursive = TRUE)
      }
      self$set_blob_metadata()
      if (self$download_mode == "all") {
        self$download_files()
      }
    },
    set_blob_metadata = function(ignore_archive = TRUE) {

      self$blobs <- list_blobs(private$container) |>
        separate(
          col = name,
          into = c("data_group", "sub_folder", "file_root"),
          sep = "\\/",
          remove = FALSE,
          extra = "drop",
          fill = "right"
        ) |>
        mutate(
          file_root = case_when(
            is.na(file_root) & grepl(".parquet", sub_folder) ~ sub_folder,
            is.na(file_root) & grepl(".json", data_group) ~ name,
            TRUE ~ file_root
          ),
          sub_folder = case_when(
            grepl(".parquet", sub_folder) ~ NA, 
            grepl(".json", sub_folder) ~ NA,
            TRUE ~ sub_folder
          ),
          file_type = str_extract(name, "(json|parquet|txt|csv)$"),
          is_archive = grepl("archive", data_group) | grepl("archive", sub_folder)
        ) |>
        separate(
          col = sub_folder,
          into = c("Remove", "ExperimentID"),
          sep = "\\=",
          remove = FALSE,
          fill = "right"
        ) |>
        mutate(
          namespace = ifelse(
            !is.na(ExperimentID) & Remove == "namespace",
            ExperimentID,
            NA
          ),
          ExperimentID = ifelse(
            !is.na(namespace), NA, ExperimentID
          )
        ) |>
        filter(
          if (ignore_archive) is_archive == FALSE else TRUE
        ) |>
        select(data_group, sub_folder, ExperimentID, namespace, name, file_root, file_type, size)

      return(invisible(self$blobs))
    },
    get_remote_file_data = function(file_name, read_method_args = list()) {

      target_files <- self$blobs |>
        filter(
          grepl(file_name, data_group) & grepl(file_name, name)
        ) |>
        bind_rows(
          self$blobs |>
            filter(
              sub_folder == file_name
            )
        ) |>
        pull(name)

      self$targeted_file <- private$resolve_target_file(
        target_files,
        glue("file_name pattern '{file_name}'")
      )

      self$read_file_data(read_method_args = read_method_args)
    },
    download_remote_file = function(file_name) {
      return(
        invisible(
          storage_download(
            private$container,
            src = file_name,
            dest = self$local_file_path
          )
        )
      )
    },
    read_file_data = function(read_method_args = list()) {
      if (self$download_mode == "on demand" && !self$local_file_exists) {
        self$download_remote_file(self$targeted_file)
      }

      if (!is.list(read_method_args)) {
        stop("read_method_args must be a list.", call. = FALSE)
      }

      if (!is.list(self$default_read_method_args)) {
        stop("default_read_method_args must resolve to a list.", call. = FALSE)
      }

      return(
        do.call(
          eval(parse(text = self$file_read_method)),
          c(list(self$local_file_path), self$default_read_method_args, read_method_args)
        )
      )
    },
    get_experiment_data = function(experiment_id, read_method_args = list()) {
      target_files <- self$blobs |>
        filter(
          ExperimentID == experiment_id
        ) |>
        pull(name)

      self$targeted_file <- private$resolve_target_file(
        target_files,
        glue("experiment_id '{experiment_id}'")
      )

      return(
        self$read_file_data(read_method_args = read_method_args)
      )
    },
    get_pre_calculated_data = function(target_namespace, read_method_args = list()) {
      target_files <- self$blobs |>
        filter(
          namespace == target_namespace
        ) |>
        pull(name)

      self$targeted_file <- private$resolve_target_file(
        target_files,
        glue("namespace '{target_namespace}'")
      )

      return(
        self$read_file_data(read_method_args = read_method_args)
      )
    },
    download_files = function(reload_files = TRUE) {
      self$files_downloaded <- FALSE
      if (reload_files) {
        unlink(self$local_data_directory, recursive = TRUE)
        tryCatch({
          self$blobs |>
            arrange(size) |>
            pull(name) |>
            map(function(x) {
              dest <- glue("{self$local_data_directory}/{x}")
              suppressMessages(
                storage_download(private$container, src = x, dest = dest)
              )
            })
          print(glue("{length(list.files(self$local_data_directory, recursive = TRUE))} files downloaded"))
          self$files_downloaded <- TRUE
        }, error = function(e) {
            print(glue("an error occured while downloading files: {e}"))
            self$files_downloaded <- TRUE
        })
      } else {
        print(glue("{length(list.files(self$local_data_directory, recursive = TRUE))} existing files found"))
      }
    },
    get_file_group_directory = function(file_group) {
      dirs <- list.dirs(self$local_data_directory)
      fqdn <- dirs[intersect(which(grepl(file_group, dirs)), which(!grepl("=", dirs)))]
      return(fqdn)
    }

  )

)
