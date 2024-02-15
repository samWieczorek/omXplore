#' @exportMethod GetSlotMetadata
#' 
#' @rdname accessors_SE
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotMetadata", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
                SummarizedExperiment::rowData(object)},
                warning = function(w) NULL,
                error = function(e) NULL)
          })


#' @exportMethod GetSlotQdata
#' 
#' @rdname accessors_SE
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotQdata", signature = "ANY",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            NULL
          })

setMethod("GetSlotQdata", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            tryCatch({
              SummarizedExperiment::assay(object)
              },
              warning = function(w) NULL,
              error = function(e) NULL)
          })

#' @exportMethod GetSlotMetacell
#' 
#' @rdname accessors_SE
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


#' @exportMethod GetSlotColID
#' 
#' @rdname accessors_SE
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("GetSlotColID", signature = "SummarizedExperiment",
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



#' @exportMethod GetSlotType
#' 
#' @rdname accessors_SE
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




#' @exportMethod GetSlotProteinID
#' 
#' @rdname accessors_SE
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



#' @exportMethod GetSlotConds
#' 
#' @rdname accessors_SE
#' @import QFeatures
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


setMethod("GetSlotConds", signature = "ANY",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            ''
          })



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
    if ("adjacencyMatrix" %in% names(args$metadata))
      args$metadata <- args$metadata[, -which(names(args$metadata)=="adjacencyMatrix")]
  
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