#' @title Accessors fucntions
#' @description xxx
#' @return See individual method description for the return value. 
#' @param object An object of 
#' @name accessors
#' @examples
#' 
#' ## -----------------------------------
#' ## Accessing slots from a MSnSet dataset
#' ## -----------------------------------
#' data(Exp1_R25_prot, package = 'DAPARdata')
#' obj <- Exp1_R25_prot
#' GetSlotMetadata(obj)
#' GetSlotQdata(obj)
#' GetSlotMetacell(obj)
#' GetSlotColID(obj)
#' GetSlotType(obj)
#' GetSlotProteinID(obj)
#' GetSlotConds(obj)
#' 
NULL


#' @exportMethod GetSlotMetadata
#' 
#' @rdname accessors
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' @return A data.frame containing the metadata of the dataset
#' 
setMethod("GetSlotMetadata", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
                SummarizedExperiment::rowData(object)},
                warning = function(w) NULL,
                error = function(e) NULL)
          })

#' @rdname accessors
#' @import MSnbase
#'
setMethod("GetSlotMetadata", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              MSnbase::fData(object)},
              warning = function(w) NULL,
              error = function(e) NULL)
          })


#' @exportMethod GetSlotQdata
#' 
#' @rdname accessors
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotQdata", signature = "ANY",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            NULL
          })


#' @exportMethod GetSlotQdata
#' 
#' @rdname accessors
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotQdata", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
              SummarizedExperiment::assay(object)
              },
              warning = function(w) NULL,
              error = function(e) NULL)
          })

#' @rdname accessors
#' @import MSnbase
#'
setMethod("GetSlotQdata", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              MSnbase::exprs(object)
            },
            warning = function(w) NULL,
            error = function(e) NULL)
          })




#' @exportMethod GetSlotMetacell
#' 
#' @rdname accessors
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotMetacell", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
              (SummarizedExperiment::rowData(object))$qMetacell
            },
            warning = function(w) NULL,
            error = function(e) NULL)
          })

#' @rdname accessors
#' @import MSnbase
#'
setMethod("GetSlotMetacell", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          function(object) {
            tryCatch({
              #if ('names_metacell' %in% names(object@experimentData@other))
              MSnbase::fData(object)[, object@experimentData@other$names_metacell]
            },
            warning = function(w) NULL,
            error = function(e) NULL)
          })


 
#' @rdname accessors
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotColID", signature = "SummarizedExperiment",
          #' @title xxx
          #' @description xxx
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
              colID <- SummarizedExperiment::metadata(object)$idcol
              if(is.null(colID)) colID <- ''
              
              colID
            },
            warning = function(w) '',
            error = function(e) '')
          })

#' @import MSnbase
#' @rdname accessors
#' 
setMethod("GetSlotColID", signature = "MSnSet",
          #' @title xxx
          #' @description xxx
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


#' @exportMethod GetSlotType
#' 
#' @rdname accessors
#' @import SummarizedExperiment
#' 
setMethod("GetSlotType", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
              type <- SummarizedExperiment::metadata(object)$typeDataset
              if(is.null(type)) type <- ''
              
              type
            },
            warning = function(w) '',
            error = function(e) '')
          })

#' @import MSnbase
#' @rdname accessors
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


#' @exportMethod GetSlotProteinID
#' 
#' @rdname accessors
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotProteinID", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
              proteinID <- SummarizedExperiment::metadata(object)$parentProtId
              if(is.null(proteinID)) proteinID <- ''
              
              proteinID
            },
            warning = function(w) '',
            error = function(e) '')
          })

#' @import MSnbase
#' @rdname accessors
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


#' @exportMethod GetSlotConds
#' 
#' @rdname accessors
#' @import MultiAssayExperiment
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotConds", signature = "MultiAssayExperiment",
          #' @param object An instance of class `MultiAssayExperiment`.
          function(object) {
            tryCatch({
              conds <- SummarizedExperiment::colData(object)$Condition
              if(is.null(conds)) conds <- ''
              
              conds
            },
            warning = function(w) '',
            error = function(e) '')
          })

#' @rdname accessors
#' 
setMethod("GetSlotConds", signature = "ANY",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            ''
          })

#' @rdname accessors
#' @import MSnbase
#'
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


#' @title xxx
#' @description xxx
#' @param object An object that inherits MultiAssayExperiment class
#' (e.g. MultiAssayExperiment, QFeatures)
#' 
#' @return An object of class DaparViz.
#' 
#' @examples
#' data(vData_ft)
#' convertMAEtype(vData_ft)
#' 
convertMAEtype <- function(object){
ll <- list()
for (i in 1:length(object)){
  cat(paste0('Processing assay ', i, '...\n'))
  
  
  args <- list(qdata = matrix(),
               metacell = data.frame(),
               metadata = data.frame(),
               colID = '',
               proteinID = '',
               conds = '',
               type = '',
               adjMat = matrix(),
               cc = list()
               )

  
  if (inherits(object[[i]], 'SummarizedExperiment')){
    args <- list(
      qdata = GetSlotQdata(object[[i]]),
      metacell = GetSlotMetacell(object[[i]]),
      metadata = GetSlotMetadata(object[[i]]),
      colID =  GetSlotColID(object[[i]]),
      proteinID = GetSlotProteinID(object[[i]]),
      conds = GetSlotConds(object[[i]]),
      type = GetSlotType(object[[i]]),
      adjMat = matrix(),
      cc = list()
    )
  
    if (!is.null(args$metacell))
      args$metadata <- args$metadata[, -which(names(args$metadata)=="qMetacell")]
  
    # Delete adjacency Matrix from whole metadata
    if ("adjacencyMatrix" %in% names(args$metadata)){
      .ind <- which(names(args$metadata)=="adjacencyMatrix")
      args$metadata <- args$metadata[, -.ind]
    }
  
    if (!is.null(args$proteinID) && args$proteinID != ''){
     args$adjMat <- PSMatch::makeAdjacencyMatrix(args$metadata[, args$proteinID])
     rownames(args$adjMat) <- rownames(args$metadata)
    
      # Create the connected components
      args$cc <- PSMatch::ConnectedComponents(args$adjMat)@adjMatrices
    }
  
    # Fix typos
  
    args$metadata <- as.data.frame(args$metadata)
  }
  
  
  ll[[names(object)[i]]] <- do.call(DaparViz, args)
  
  }
return(ll)
}