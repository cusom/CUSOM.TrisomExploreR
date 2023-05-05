#' R6 Class to manage ODBC Database connections
#' @description
#' Manage ODBC Database connections
#'
#' @field connection_open - logical - whether the connection is currently is open or not
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
      private$filepath = filepath
    },

    #' @description
    #' Connect to target database
    #' @return none
    connect = function() {
      filepath <- private$filepath
      private$dbhandle <- DBI::dbConnect(
        RSQLite::SQLite(),
        filepath
      )
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

#' R6 Class to manage SQLite Database queries - subclass of SQLiteConnectionManager
#' @description
#' Manage SQLite Database queries
#'
#' @field queryString - string - Parameterized SQL Query to execute against target database
#' @field parameters - tibble - tibble of parameter names and values
#' @field data - tibble - query result formatted as tibble
#' @export
SQLiteQueryManager <- R6::R6Class(
  "SQLiteQueryManager",
  inherit = SQLiteConnectionManager,
  private = list(

    #' @description
    #' helper function to set/format parameters tibble
    #' @field parameters tibble - tibble of parameter values
    #' @field e - environment - ephemeral environment to load parameter values
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

      }
      else {
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
      super$initialize(filepath)
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
      }

      else {

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
