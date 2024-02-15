
#' @rdname accessors-MSnSet
#' @import MSnbase
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#'
setMethod("GetSlotMetadata", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              fData(object)},
              warning = function(w) NULL,
              error = function(e) NULL)
          })


setMethod("GetSlotQdata", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              exprs(object)
            },
            warning = function(w) NULL,
            error = function(e) NULL)
          })

setMethod("GetSlotMetacell", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              #if ('names_metacell' %in% names(object@experimentData@other))
                fData(object)[, object@experimentData@other$names_metacell]
            },
            warning = function(w) NULL,
            error = function(e) NULL)
          })


setMethod("GetSlotColID", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              colID <- object@experimentData@other$keyId
              if(is.null(colID)) colID <- ''

              colID
            },
            warning = function(w) '',
            error = function(e) '')
          })



setMethod("GetSlotType", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              type <- object@experimentData@other$typeOfData
              if(is.null(type)) type <- ''

              type
            },
            warning = function(w) '',
            error = function(e) '')
          })




setMethod("GetSlotProteinID", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              proteinID <- object@experimentData@other$proteinId
              if(is.null(proteinID)) proteinID <- ''

              proteinID
            },
            warning = function(w) '',
            error = function(e) '')
          })



setMethod("GetSlotConds", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              conds <- MSnbase::pData(object)$Condition
              if(is.null(conds)) conds <- ''

              conds
            },
            warning = function(w) '',
            error = function(e) '')
          })
