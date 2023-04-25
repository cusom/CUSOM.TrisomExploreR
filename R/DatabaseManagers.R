#' @export
ODBCConnectionManager <- R6::R6Class(
  "ODBCConnectionManager",
  private = list(
    conn_args = NULL,
    dbhandle = NULL
  ),
  public = list(
    connection_open = FALSE,
    initialize = function(conn_args){
      private$conn_args = conn_args
    },
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
    disconnect = function() {
      DBI::dbDisconnect(private$dbhandle)
      self$connection_open <- FALSE
    }
  )
)

#' @export
ODBCQueryManager <- R6::R6Class(
  "ODBCQueryManager",
  inherit = ODBCConnectionManager,
  private = list(
    setParameters = function(parameters) {
      if(!is.null(parameters)) {
        self$parameters <- parameters |>
          dplyr::mutate_if(is.factor, as.character)
      } else {
        self$parameters <- parameters
      }

    },
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
    initialize = function(conn_args){
      super$initialize(conn_args)
    },
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

#' @export
SQLiteConnectionManager <- R6::R6Class(
  "SQLiteConnectionManager",
  private = list(
    filepath = NULL,
    dbhandle = NULL
  ),
  public = list(
    connection_open = FALSE,
    initialize = function(filepath){
      private$filepath = filepath
    },
    connect = function() {
      filepath <- private$filepath
      private$dbhandle <- DBI::dbConnect(
        RSQLite::SQLite(),
        filepath
      )
      self$connection_open <- TRUE
    },
    disconnect = function() {
      DBI::dbDisconnect(private$dbhandle)
      self$connection_open <- FALSE
    }
  )
)

#' @export
SQLiteQueryManager <- R6::R6Class(
  "SQLiteQueryManager",
  inherit = SQLiteConnectionManager,
  private = list(
    setParameters = function(parameters) {
      if (!is.null(parameters)) {
        params <- parameters |>
          dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) |>
          tidyr::pivot_longer(cols = tidyselect::everything()) |>
          dplyr::select(value)
        self$parameters <- lapply(seq_len(nrow(params)), function(i) unlist(params[i, 1], use.names=FALSE))
      } else {
        self$parameters <- parameters
      }
    }
  ),
  public = list(
    queryString = NULL,
    parameters = NULL,
    data = NULL,
    initialize = function(filepath){
      super$initialize(filepath)
    },
    getQuery = function(queryString, parameters = NULL) {

      self$queryString <- queryString

      self$connect()

      if (is.null(parameters)) {
        self$data <- DBI::dbGetQuery(private$dbhandle, self$queryString)
      }

      else {

        params <- parameters |>
          dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) |>
          tidyr::pivot_longer(cols = tidyselect::everything())

        sapply(
          unique(params$name),
          function(param_name) {
            param_vals <- params |>
              dplyr::filter(name == param_name) |>
              dplyr::distinct() |>
              dplyr::pull()
            assign(
              param_name,
              param_vals,
              inherits = TRUE
            )
          }
        )

        q <- glue::glue_sql(
          self$queryString,
          .con = private$dbhandle
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
