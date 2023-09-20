#' R6 Class to manage TrisomExploreR Transcriptome application  -- DEPRECATED --
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
    initialize = function(ApplicationId, remoteDB, localDB) {
      .Deprecated(new = "TrisomExplorerAppManager", old = "TranscriptomeAppManager")
      super$initialize(ApplicationId, remoteDB, localDB)
    }
  )
)
