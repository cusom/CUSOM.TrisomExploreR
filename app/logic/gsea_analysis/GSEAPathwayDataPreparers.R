box::use(
    R6[R6Class],
)

#' @export
GSEADataPreparerBase <- R6Class(
    "GSEADataPreparerBase",
    private = list(),
    active = list(),
    public = list(
        initialize = function() {

        },
        prepare = function(data) {
            return(data)
        }
    )
)