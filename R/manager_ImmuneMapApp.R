#' R6 Class to manage the Immune Map application - subclass of the core TrisomExplorerAppManager  -- DEPRECATED --
#' @description
#' subclass of the core TrisomExplorerAppManager - inherits core attributes and methods
#' @field analysis - string -
#' @field analysisMetadata - tibble -
#' @field CellType - tibble - analysis choices for UI input
#' @export
ImmuneMapAppManager <- R6::R6Class(
  "ImmuneMapAppManager",
  inherit = TrisomExplorerAppManager,
  private = list(),
  public = list(
    analysis = NULL,
    analysisMetadata = NULL,
    CellType = NULL,

    #' @description
    #' Create a new instance of a ImmuneMapAppManager
    #' @param ApplicationId - string - application id
    #' @param remoteDB R6 class - query manager for remote database queries
    #' @param localDB R6 class - query manager for local database queries
    initialize = function(ApplicationId, remoteDB, localDB){
      .Deprecated(new = "TrisomExplorerAppManager", old = "ImmuneMapAppManager")
      super$initialize(ApplicationId, remoteDB, localDB)
    }
  )
)
