

#' @exportMethod convert2Viz
#' @export
#' @rdname DaparViz-class
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("convert2Viz", signature = NULL,
          #' @param object An instance of class `QFeatures`.
          function(object) {
            NULL
          })


#' @exportMethod convert2Viz
#' @export
#' @rdname DaparViz-class
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("convert2Viz", signature = "MultiAssayExperiment",
          #' @param object An instance of class `MultiAssayExperiment`.
          function(object) {convertMAEtype(object)})


#' @exportMethod convert2Viz
#' @export
#' @rdname DaparViz-class
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("convert2Viz", signature = "QFeatures",
          #' @param object An instance of class `QFeatures`.
          function(object) {convertMAEtype(object)})


#' @exportMethod convert2Viz
#' @export
#' @rdname DaparViz-class
#' @import SummarizedExperiment
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("convert2Viz", signature = "SummarizedExperiment",
          #' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            pkgs.require('PSMatch')
            
            args <- list(
              qdata = GetSlotQdata(object),
              metacell = GetSlotMetacell(object),
              metadata = GetSlotMetadata(object),
              colID =  GetSlotColID(object),
              proteinID = GetSlotProteinID(object),
              conds = GetSlotConds(object),
              type = GetSlotType(object),
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
            
            ll[['original']] <- do.call(DaparViz, args)
            ll.tmp
            
          }
)

#' @exportMethod convert2Viz
#' @export
#' @rdname DaparViz-class
#' @import MSnbase
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("convert2Viz", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          #' @param ... xxx
          function(object, ...) {
            pkgs.require('PSMatch')
            
            args <- list(
              qdata = GetSlotQdata(object),
              metacell = GetSlotMetacell(object),
              metadata = GetSlotMetadata(object),
              colID =  GetSlotColID(object),
              proteinID = GetSlotProteinID(object),
              conds = GetSlotConds(object),
              type = GetSlotType(object),
              adjMat = matrix(),
              cc = list()
            )

            if (args$type == 'peptide'){
              args$adjMat <- PSMatch::makeAdjacencyMatrix(args$metadata[, args$proteinID])
              rownames(args$adjMat) <- rownames(args$metadata)
              connectedComp <- PSMatch::ConnectedComponents(X)
              args$cc <- connectedComp@adjMatrices
            }
            
            ll.tmp <- list()
            ll.tmp[['original']] <- do.call(DaparViz, args)
            ll.tmp
            
          }
)




#' @title Formats available in DaparViz
#' @description
#' A short description...
#' 
#' @export
#' 
FormatAvailables <- function()
  c('QFeatures', 'MSnSet', 'SummarizedExperiment', 'MultiAssayExperment', 'list')


#' #' @title Get the class of object
#' #' @description This function returns the class of the object passed as argument,
#' #' or the class of its items if the object is a `list`. If the class is not managed
#' #' by DaparViz, then return NA For a list of managed classes.
#' #' see `FormatAvailables()`.
#' #' @param ll An object.
#' #' @rdname Convert2VizList
#' #' 
#' CheckClass <- function(ll){
#'   ll.class <- NULL
#'   if (inherits(ll, 'QFeatures'))
#'     ll.class <- 'QFeatures'
#'   else if (inherits(ll, 'list')){
#'     if (sum(unlist(lapply(ll, function(x) inherits(x, 'MSnSet')))) == length(ll)
#'              && length(names(ll)) == length(ll))
#'       ll.class <- 'MSnSet'
#'     else
#'       ll.class <- 'list'
#'     }
#'   else if (inherits(ll, 'VizList'))
#'     ll.class <- 'VizList'
#'   else
#'     ll.class <- NA
#'   return(ll.class)
#' }

#' 
#' #' @title Convert a dataset to an instance of class `VizList`
#' #' @description
#' #' Actually, three types of dataset can be converted:
#' #' * A `list` which must be formatted in the correct format. See xxx
#' #' * A `list` of instances of class `MSnset`
#' #' * An instance of class `QFeatures` (which is already a list)
#' #' @param obj An instance of class `VizList`
#' #' @rdname Convert2VizList
#' #' @return An instance of class `VizList`
#' #' @export
#' convert2viz <- function(obj = NULL){
#'   
#'   if (is.null(obj) || length(obj) == 0)
#'     return(NULL)
#'   
#'   ll <- NULL
#'   stopifnot(CheckClass(obj) %in% FormatAvailables())
#'   stopifnot(!is.na(CheckClass(obj)))
#'   
#'   switch(CheckClass(obj),
#'          
#'          # Nothing to do
#'          VizList = ll <- obj,
#'          
#'          QFeatures = ll <- Convert2VizList(obj),
#'          
#'          MSnSet = {
#'            ll.tmp <- NULL
#'            for (i in 1:length(obj))
#'              ll.tmp[[names(obj)[i]]] <- Convert2DaparViz(obj[[i]])
#'            ll <- VizList(ll.tmp)
#'          },
#'          
#'          list = {
#'            ll.tmp <- NULL
#'            for (i in 1:length(obj))
#'              ll.tmp[[names(obj)[i]]] <- Convert2DaparViz(obj[[i]])
#'            ll <- VizList(ll.tmp)
#'          }
#'          )
#'   return(ll)
#'  }
