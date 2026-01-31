#' R6 Class to manage ODBC Database connections
#' @description
#' Manage ODBC Database connections
#'
#' @field connection_open - logical - whether the connection is currently is open or not
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom odbc odbc
#' @export
ODBCConnectionManager <- R6::R6Class(
  "ODBCConnectionManager",
  private = list(
    conn_args = NULL,
    dbhandle = NULL
  ),
  public = list(
    connection_open = FALSE,

    #' @description
    #' Create a new instance of ODBCConnectionManager object
    #' @param conn_args list - list of connection arguments to connect to database
    #' @return A new `ODBCConnectionManager` object.
    initialize = function(conn_args){
      private$conn_args = conn_args
    },

    #' @description
    #' Connect to target database
    #' @return none
    connect = function() {
      conn_args <- private$conn_args
      before <- getTaskCallbackNames()
      private$dbhandle <- DBI::dbConnect(
        odbc::odbc(),
        Driver   = conn_args$driver,
        Server   = conn_args$server,
        Database = conn_args$database,
        UID      = conn_args$uid,
        PWD      = conn_args$pwd,
        Port     = conn_args$port
      )
      after <- getTaskCallbackNames()
      removeTaskCallback(which(!after %in% before))
      self$connection_open <- TRUE
    },

    #' @description
    #' Disconnect from target database
    #' @return none
    disconnect = function() {
      DBI::dbDisconnect(private$dbhandle)
      self$connection_open <- FALSE
    }
  )
)

#' R6 Class to manage ODBC Database queries - subclass of OBBCQueryManager
#' @description
#' Manage ODBC Database queries
#'
#' @field queryString - string - Parameterized SQL Query to execute against target database
#' @field parameters - tibble - tibble of parameter names and values
#' @field convertFactorsToStrings - logical - whether to convert all factors to strings
#' @field data - tibble - query result formatted as tibble
#' @import dplyr
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbBind
#' @importFrom DBI dbFetch
#' @importFrom DBI dbClearResult
#'
#' @export
ODBCQueryManager <- R6::R6Class(
  "ODBCQueryManager",
  inherit = ODBCConnectionManager,
  private = list(

    #' @description
    #' helper function to set parameters tibble
    setParameters = function(parameters) {
      if (!is.null(parameters)) {
        self$parameters <- parameters |>
          dplyr::mutate_if(is.factor, as.character)
      } else {
        self$parameters <- parameters
      }

    },

    #' @description
    #' helper function to properly format query result
    formatData = function() {
      if (self$convertFactorsToStrings) {
        self$data <- self$data |>
          dplyr::mutate_if(is.factor, as.character)
      }
    }
  ),
  public = list(
    queryString = NULL,
    parameters = NULL,
    convertFactorsToStrings = TRUE,
    data = NULL,

    #' @description
    #' Create a new instance of ODBCQueryManager object
    #' @param conn_args list - list of connection arguments to connect to database
    #' @return A new `ODBCQueryManager` object.
    initialize = function(conn_args){
      super$initialize(conn_args)
    },

    #' @description
    #' Execute parameterized query against target database
    #' @param queryString - string - parameterized sql query string
    #' @param parameters - tibble - parameter names and values
    #' @param convertFactorsToStrings - logical - whether to convert factors to strings
    #' @return tibble
    getQuery = function(queryString, parameters, convertFactorsToStrings = TRUE) {

      self$queryString <- queryString

      private$setParameters(parameters)

      self$convertFactorsToStrings <- convertFactorsToStrings

      self$connect()

      query <- DBI::dbSendQuery(private$dbhandle, self$queryString)

      DBI::dbBind(query, self$parameters)

      self$data <- DBI::dbFetch(query)

      DBI::dbClearResult(query)

      self$disconnect()

      private$formatData()

      return(self$data)

    },

    #' @description
    #' insert data to table in target database
    #' @param table_name - string - name of target table
    #' @param values - tibble - tibble of values to insert to table. Should match target table schema.
    insertData = function(table_name, values) {

      stopifnot(class(table_name) %in% c("character"))
      stopifnot(any(class(values) %in% c("tbl_df", "tbl", "data.frame", "tibble", "tribble")))

      self$connect()

      tryCatch({

      if (stringr::str_detect(table_name, ".")) {

        table_names <- stringr::str_split(table_name, pattern = "\\.", simplify = TRUE)

        table_id <- DBI::Id(
          schema = table_names[1],
          table = table_names[2]
        )

        DBI::dbAppendTable(
          conn = private$dbhandle,
          name = table_id,
          value = values
        )
      } else {
        DBI::dbAppendTable(
          conn = private$dbhandle,
          name = table_name,
          value = values
        )
      }
      }, error = function(e) {
        print(glue::glue("an error occured {e}"))
      })

      self$disconnect()

      return(TRUE)
    }

  )
)

#' R6 Class to manage SQLite Database connections
#' @description
#' Manage SQLite Database connections
#'
#' @field connection_open - logical - whether the connection is currently is open or not
#' @export
SQLiteConnectionManager <- R6::R6Class(
  "SQLiteConnectionManager",
  private = list(
    filepath = NULL,
    dbhandle = NULL
  ),
  public = list(
    connection_open = FALSE,

    #' @description
    #' Create a new instance of SQLiteConnectionManager object
    #' @param filepath string - path to `.sqlite` database file
    #' @return A new `SQLiteConnectionManager` object.
    initialize = function(filepath) {
      .Deprecated(new = "N/A", old = "SQLiteConnectionManager")
    },

    #' @description
    #' Connect to target database
    #' @return none
    connect = function() {
      .Deprecated(new = "N/A", old = "SQLiteConnectionManager")
    },

    #' @description
    #' Disconnect from target database
    #' @return none
    disconnect = function() {
      .Deprecated(new = "N/A", old = "SQLiteConnectionManager")
    }
  )
)

#' R6 Class to manage SQLite Database queries - subclass of SQLiteConnectionManager
#' @description
#' Manage SQLite Database queries
#'
#' @field queryString - string - Parameterized SQL Query to execute against target database
#' @field parameters - tibble - tibble of parameter names and values
#' @field data - tibble - query result formatted as tibble
#' @import dplyr
#' @import tidyr
#' @importFrom glue glue_sql
#' @export
SQLiteQueryManager <- R6::R6Class(
  "SQLiteQueryManager",
  inherit = SQLiteConnectionManager,
  private = list(

    #' @description
    #' helper function to set/format parameters tibble
    #' @param parameters tibble - tibble of parameter values
    #' @param e - environment - ephemeral environment to load parameter values
    set_parameters = function(parameters, e) {

      if (!is.null(parameters)) {

        self$parameters <- parameters |>
          dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) |>
          tidyr::pivot_longer(cols = tidyselect::everything())

        # create / load env. object per parameter name / values
        sapply(
          unique(self$parameters$name),
          function(param_name) {
            param_vals <- self$parameters |>
              dplyr::filter(name == param_name) |>
              dplyr::distinct() |>
              dplyr::pull()
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

    #' @description
    #' Create a new instance of SQLiteQueryManager object
    #' @param filepath string - path to `.sqlite` database file
    #' @return A new `SQLiteQueryManager` object.
    initialize = function(filepath) {
      .Deprecated(new = "N/A", old = "SQLiteQueryManager")
    },

    #' @description
    #' Execute parameterized query against target database
    #' @param queryString - string - parameterized sql query string
    #' @param parameters - tibble - parameter names and values
    getQuery = function(queryString, parameters = NULL) {

      self$queryString <- queryString

      self$connect()

      if (is.null(parameters)) {
        self$data <- DBI::dbGetQuery(private$dbhandle, self$queryString)
      } else {

        e <- new.env()

        private$set_parameters(parameters, e)

        q <- glue::glue_sql(
          self$queryString,
          .con = private$dbhandle,
          .envir = e
        )

        pq <- DBI::dbSendQuery(private$dbhandle, q)

        self$data <- DBI::dbFetch(pq)

        DBI::dbClearResult(pq)

      }

      self$disconnect()

      return(self$data)

    }
  )
)

#' R6 Class to download remote blob files from Azure Storage
#' @description
#' download remote blob files from Azure Storage
#'
#' @field local_data_directory - string - defaults to `data` - path to download remote files locally
#' @field files_downloaded - logical - are the remote files downloaded?
#' @importFrom glue glue
#' @importFrom AzureStor storage_endpoint storage_container storage_download
#' @export
AzureRemoteDataFileManager <- R6::R6Class(
  "AzureRemoteDataFileManager",
  private = list(
    account_name = "",
    key = "",
    container_name = "",
    endpoint = NULL,
    container = NULL
  ),
  active = list(
    uri = function(value) {
      return(
        glue::glue("https://{private$account_name}.blob.core.windows.net")
      )
    },
    file_type = function(value) {
      return(
        self$blobs |>
          dplyr::filter(name == self$targeted_file) |>
          dplyr::pull(file_type)
      )
    },
    local_file_exists = function(value) {
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
      return(
        glue::glue("{self$local_data_directory}/{self$targeted_file}")
      )
    },
    file_read_method = function(value) {
      if (self$file_type == "json") {
        return(
          "jsonlite::fromJSON"
        )
      } else if (self$file_type == "parquet") {
        return(
          "arrow::read_parquet"
        )
      } else {
        return(
          "unknown"
        )
      }
    }
  ),
  public = list(
    local_data_directory = NULL,
    download_mode = NULL,
    files_downloaded = FALSE,
    targeted_file = NULL,
    blobs = NULL,
    initialize = function(account_name, key, container_name,
      download_mode = c("on demand", "all"), local_data_directory = "Remote_Data") {
      match.arg(download_mode)
      private$account_name <- account_name
      private$key <- key
      private$container_name <- container_name
      self$download_mode <- download_mode
      self$local_data_directory <- local_data_directory
      private$endpoint <- AzureStor::storage_endpoint(self$uri, private$key)
      private$container <- AzureStor::storage_container(private$endpoint, private$container_name)
      unlink(self$local_data_directory, recursive = TRUE)
      self$set_blob_metadata()
      if (self$download_mode == "all") {
        self$download_files()
      }
    },
    set_blob_metadata = function() {

      self$blobs <- AzureStor::list_blobs(private$container) |>
        tidyr::separate(
          col = name,
          into = c("data_group", "sub_folder"),
          sep = "\\/",
          remove = FALSE,
          extra = "drop"
        ) |>
        dplyr::mutate(
            sub_folder = dplyr::case_when(
                grepl(".parquet", sub_folder) ~ NA,
                TRUE ~ sub_folder
            ),
            file_type = ifelse(grepl("json", name), "json", "parquet")
        ) |>
        tidyr::separate(
          col = sub_folder,
          into = c("Remove", "ExperimentID"),
          sep = "\\=",
          remove = FALSE
        ) |>
        dplyr::mutate(
          namespace = ifelse(
            !is.na(ExperimentID) & Remove == "namespace", 
            ExperimentID, 
            NA
          ),
          ExperimentID = ifelse(
            !is.na(namespace), NA, ExperimentID
          )
        ) |>
        dplyr::select(data_group, sub_folder, ExperimentID, namespace, name, file_type, size)
      return(invisible(self$blobs))
    },
    get_remote_file_data = function(file_name) {
      self$targeted_file <- self$blobs |>
        dplyr::filter(grepl(file_name, name)) |>
        dplyr::pull(name)

      self$read_file_data()
    },
    download_remote_file = function(file_name) {
      return(
        invisible(
          AzureStor::storage_download(
            private$container,
            src = file_name,
            dest = self$local_file_path
          )
        )
      )
    },
    read_file_data = function() {
      if (self$download_mode == "on demand" && !self$local_file_exists) {
        self$download_remote_file(self$targeted_file)
      }
      return(
        do.call(
          eval(parse(text = self$file_read_method)),
          list(self$local_file_path)
        )
      )
    },
    get_experiment_data = function(experiment_id) {
      self$targeted_file <- self$blobs |>
        dplyr::filter(
          ExperimentID == experiment_id
        ) |>
        dplyr::pull(name)
      return(
        self$read_file_data()
      )
    },
    get_pre_calculated_data = function(target_namespace) {
      self$targeted_file <- self$blobs |>
        dplyr::filter(
          namespace == target_namespace
        ) |>
        dplyr::pull(name)
      return(
        self$read_file_data()
      )
    },
    download_files = function(reload_files = TRUE) {
      self$files_downloaded <- FALSE
      if (reload_files) {
        unlink(self$local_data_directory, recursive = TRUE)
        tryCatch({
          self$blobs |>
            dplyr::arrange(size) |>
            dplyr::pull(name) |>
            purrr::map(function(x) {
              dest <- glue::glue("{self$local_data_directory}/{x}")
              suppressMessages(
                AzureStor::storage_download(container, src = x, dest = dest)
              )
            })
          print(glue::glue("{length(list.files(self$local_data_directory, recursive = TRUE))} files downloaded"))
          self$files_downloaded <- TRUE
        }, error = function(e) {
            print(glue::glue("an error occured while downloading files: {e}"))
            self$files_downloaded <- TRUE
        })
      } else {
        print(glue::glue("{length(list.files(self$local_data_directory, recursive = TRUE))} existing files found"))
      }
    },
    get_file_group_directory = function(file_group) {
      dirs <- list.dirs(self$local_data_directory)
      fqdn <- dirs[intersect(which(grepl(file_group, dirs)), which(!grepl("=", dirs)))]
      return(fqdn)
    }

  )

)
