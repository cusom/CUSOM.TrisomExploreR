#' R6 Class to 
#' @description 
#' 
#' @field analysis
#' @field analysisMetadata
#' @field CellType - 
#' @export
ImmuneMapAppManager <- R6::R6Class(
  "ImmuneMapAppManager",
  inherit = TrisomExplorerAppManager,
  private = list(),
  public = list(
    analysis = NULL,
    analysisMetadata = NULL,
    CellType = NULL,
    initialize = function(ApplicationId, remoteDB, localDB){
      super$initialize(ApplicationId, remoteDB, localDB)

      self$input_config$Queryplatforms <- self$input_config$Queryplatforms[grepl("Mass", self$input_config$Queryplatforms)]

    }
  )
)
