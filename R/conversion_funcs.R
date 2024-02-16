##' @title Convert datasets to `DaparViz` class
##' @description These functions are converters for different data formats
##' to an object of class `DaparViz` which is the format used by functions
##' in the package `DaparViz`.
##' 
##' @name DaparViz-converter
##' 
##' @return An object of class `DaparViz`.
##' 
##' @examples
##' 
##' # Conversion of an object of class `QFeatures`
##' data(vData_ft)
##' convert2Viz(vData_ft)
##' 
##' # Conversion of an object of class `MSnSet`
##' data(vData_ms)
##' convert2Viz(vData_ms)
##' 
##' # Conversion of an object of class `SummarizedExperiment`
##' data(miniACC, package='MultiAssayExperiment')
##' convert2Viz(miniACC[[1]])
##'
##' 
##' # Conversion of an object of class `MultiAssayExperiment`
##' data(miniACC, package='MultiAssayExperiment')
##' convert2Viz(miniACC)
##'
NULL




##' @rdname DaparViz-converter
##' @import QFeatures
##' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
##' @exportMethod convert2Viz
setMethod("convert2Viz", signature = NULL,
          ##' @param object An instance of class `QFeatures`.
          function(object) {
            NULL
          })


##' @rdname DaparViz-converter
##' @import QFeatures
##' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
##' @exportMethod convert2Viz 
setMethod("convert2Viz", signature = "MultiAssayExperiment",
          ##' @param object An instance of class `MultiAssayExperiment`.
          function(object) {convertMAEtype(object)})



##' @rdname DaparViz-converter
##' @import QFeatures
##' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
##' @exportMethod convert2Viz
setMethod("convert2Viz", signature = "QFeatures",
          ##' @param object An instance of class `QFeatures`.
          function(object) {convertMAEtype(object)})


##' @rdname DaparViz-converter
##' @import SummarizedExperiment
##' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
##' @exportMethod convert2Viz
setMethod("convert2Viz", signature = "SummarizedExperiment",
          ##' @param object An instance of class `SummarizedExperiment`.
          function(object) {
            pkgs.require('PSMatch')
            
            ll <- list()
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
            
            
            if (!is.null(args$metacell)){
              ind <- which(names(args$metadata)=="qMetacell")
              args$metadata <- args$metadata[, -ind]
            }
            
            # Delete adjacency Matrix from whole metadata
            if ("adjacencyMatrix" %in% names(args$metadata)){
              ind <- which(names(args$metadata)=="adjacencyMatrix")
              args$metadata <- args$metadata[, -ind]
            }
            
            if (!is.null(args$proteinID) && args$proteinID != ''){
              .args <- args$metadata[, args$proteinID]
              args$adjMat <- PSMatch::makeAdjacencyMatrix(.args)
              rownames(args$adjMat) <- rownames(args$metadata)
              
              # Create the connected components
              args$cc <- PSMatch::ConnectedComponents(args$adjMat)@adjMatrices
            }
            
            # Fix typos
            
            args$metadata <- as.data.frame(args$metadata)
            
            ll[['original']] <- do.call(DaparViz, args)
            ll
            
          }
)


##' @rdname DaparViz-converter
##' @import MSnbase
##' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
##' @exportMethod convert2Viz
setMethod("convert2Viz", signature = "MSnSet",
          ##' @param object An instance of class `MSnSet`.
          ##' @param ... xxx
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
              .dat <- args$metadata[, args$proteinID]
              args$adjMat <- PSMatch::makeAdjacencyMatrix(.dat)
              rownames(args$adjMat) <- rownames(args$metadata)
              connectedComp <- PSMatch::ConnectedComponents(X)
              args$cc <- connectedComp@adjMatrices
            }
            
            ll.tmp <- list()
            ll.tmp[['original']] <- do.call(DaparViz, args)
            ll.tmp
            
          }
)




##' @title Formats available in DaparViz
##' @description
##' Gives the list of classes for the datasets which be can converted by the 
##' function `convert2Viz()`.
##' 
##' @return A vector of classes compliant with the package DaparViz.
##' @examples
##' FormatAvailables()
##' 
##' @export
##' 
FormatAvailables <- function()
  c('QFeatures', 
    'MSnSet', 
    'SummarizedExperiment', 
    'MultiAssayExperment', 
    'list')


##' ##' @title Get the class of object
##' ##' @description This function returns the class of the object passed as 
##' ##' argument, or the class of its items if the object is a `list`. If the 
##' ##' class is not managed by DaparViz, then return NA For a list of managed 
##' ##' classes.
##' ##' see `FormatAvailables()`.
##' ##' @param ll An object.
##' ##' @rdname Convert2VizList
##' ##' 
##' CheckClass <- function(ll){
##'   ll.class <- NULL
##'   if (inherits(ll, 'QFeatures'))
##'     ll.class <- 'QFeatures'
##'   else if (inherits(ll, 'list')){
##'     if (sum(unlist(lapply(ll, function(x) inherits(x, 'MSnSet')))) == length(ll)
##'              && length(names(ll)) == length(ll))
##'       ll.class <- 'MSnSet'
##'     else
##'       ll.class <- 'list'
##'     }
##'   else if (inherits(ll, 'VizList'))
##'     ll.class <- 'VizList'
##'   else
##'     ll.class <- NA
##'   return(ll.class)
##' }

##' 
##' ##' @title Convert a dataset to an instance of class `VizList`
##' ##' @description
##' ##' Actually, three types of dataset can be converted:
##' ##' * A `list` which must be formatted in the correct format. See xxx
##' ##' * A `list` of instances of class `MSnset`
##' ##' * An instance of class `QFeatures` (which is already a list)
##' ##' @param obj An instance of class `VizList`
##' ##' @rdname Convert2VizList
##' ##' @return An instance of class `VizList`
##' ##' @export
##' convert2viz <- function(obj = NULL){
##'   
##'   if (is.null(obj) || length(obj) == 0)
##'     return(NULL)
##'   
##'   ll <- NULL
##'   stopifnot(CheckClass(obj) %in% FormatAvailables())
##'   stopifnot(!is.na(CheckClass(obj)))
##'   
##'   switch(CheckClass(obj),
##'          
##'          # Nothing to do
##'          VizList = ll <- obj,
##'          
##'          QFeatures = ll <- Convert2VizList(obj),
##'          
##'          MSnSet = {
##'            ll.tmp <- NULL
##'            for (i in 1:length(obj))
##'              ll.tmp[[names(obj)[i]]] <- Convert2DaparViz(obj[[i]])
##'            ll <- VizList(ll.tmp)
##'          },
##'          
##'          list = {
##'            ll.tmp <- NULL
##'            for (i in 1:length(obj))
##'              ll.tmp[[names(obj)[i]]] <- Convert2DaparViz(obj[[i]])
##'            ll <- VizList(ll.tmp)
##'          }
##'          )
##'   return(ll)
##'  }
