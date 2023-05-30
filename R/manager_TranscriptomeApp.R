#' R6 Class to manage TrisomExploreR Transcriptome application
#' @description
#' subclass of TrisomExplorerAppManager
#' @export
TranscriptomeAppManager <- R6::R6Class(
  "TranscriptomeAppManager",
  inherit = TrisomExplorerAppManager,
  private = list(),
  public = list(

    #' @description
    #' Create a new instance of a TranscriptomeAppManager
    #' @param ApplicationId - string - application id
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(ApplicationId, remoteDB, localDB){
      super$initialize(ApplicationId, remoteDB, localDB)

      self$input_config$CellTypes <- localDB$getQuery(
        "SELECT distinct [CellType] FROM CellTypes"
        ) |>
        dplyr::pull()

      self$input_config$Genes <- localDB$getQuery(
        "SELECT distinct [Analyte] FROM Genes"
        ) |>
        dplyr::arrange(Analyte) |>
        data.table::as.data.table()

      self$input_config$Analytes <- localDB$getQuery(
        "SELECT distinct g.Analyte
          FROM PrecalculatedDESeq2 s
          INNER JOIN Genes g ON AnalyteID = s.Geneid"
        ) |>
        dplyr::arrange(Analyte) |>
        data.table::as.data.table()

    }
  )
)
