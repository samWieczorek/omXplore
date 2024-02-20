#' @title Convert datasets to `DaparViz` class
#' @description These functions are converters for different data formats
#' to an object of class `DaparViz` which is the format used by functions
#' in the package `DaparViz`.
#'
#' @name DaparViz-converter
#'
#' @param object An instance of class `QFeatures`.
#'
#' @return An object of class `DaparViz`.
#'
#' @examples
#'
#'
#' # Conversion of an object of class `MSnSet`
#' data(sub_Exp1_R2_pept_MSnSet)
#' convert2Viz(sub_Exp1_R2_pept_MSnSet)
#'
#' # Conversion of an object of class `QFeatures`
#' data(sub_Exp1_R2_pept_qf)
#' convert2Viz(sub_Exp1_R2_pept_qf)
#'
#' @exportMethod convert2Viz
#' @export
#' @aliases convert2Viz
#'
#' @return An instance of class `DaparViz`
#'
#' @import QFeatures
#' @import MSnbase
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#'
setGeneric(
  "convert2Viz",
  function(object, ...) standardGeneric("convert2Viz")
)


#' @exportMethod convert2Viz
#' @export
#' @return An object of class `DaparViz`.
#' @rdname DaparViz-converter
setMethod("convert2Viz",
  signature = NULL,
  #' @title xxx
  #' @param object An instance of class `MultiAssayExperiment`.
  function(object) {
    NULL
  }
)


#' @exportMethod convert2Viz
#' @export
#' @return An object of class `DaparViz`.
#' @rdname DaparViz-converter
setMethod("convert2Viz",
  signature = "MultiAssayExperiment",
  #' @title xxx
  #' @param object An instance of class `MultiAssayExperiment`.
  function(object) {
    convertMAEtype(object)
  }
)


#' @exportMethod convert2Viz
#' @export
#' @return An object of class `DaparViz`.
#' @rdname DaparViz-converter
setMethod("convert2Viz",
  signature = "QFeatures",
  #' @title xxx
  #' @param object An instance of class `MultiAssayExperiment`.
  function(object) {
    convertMAEtype(object)
  }
)



#' @exportMethod convert2Viz
#' @export
#' @rdname DaparViz-converter
#' @return An object of class `DaparViz`.
#' @import PSMatch
#'
setMethod("convert2Viz",
  signature = "SummarizedExperiment",
  #' @title xxx
  #' @param object An instance of class `MultiAssayExperiment`.
  function(object) {
    ll <- list()
    args <- list(
      qdata = GetSlotQdata(object),
      metacell = GetSlotMetacell(object),
      metadata = GetSlotMetadata(object),
      colID = GetSlotColID(object),
      proteinID = GetSlotProteinID(object),
      conds = GetSlotConds(object),
      type = GetSlotType(object),
      adjMat = matrix(),
      cc = list()
    )


    if (!is.null(args$metacell)) {
      ind <- which(names(args$metadata) == "qMetacell")
      args$metadata <- args$metadata[, -ind]
    }

    # Delete adjacency Matrix from whole metadata
    if ("adjacencyMatrix" %in% names(args$metadata)) {
      ind <- which(names(args$metadata) == "adjacencyMatrix")
      args$metadata <- args$metadata[, -ind]
    }

    if (!is.null(args$proteinID) && args$proteinID != "") {
      .args <- args$metadata[, args$proteinID]
      args$adjMat <- PSMatch::makeAdjacencyMatrix(.args)
      rownames(args$adjMat) <- rownames(args$metadata)

      # Create the connected components
      args$cc <- PSMatch::ConnectedComponents(args$adjMat)@adjMatrices
    }

    # Fix typos

    args$metadata <- as.data.frame(args$metadata)

    ll[["original"]] <- do.call(DaparViz, args)
    ll
  }
)



#' @exportMethod convert2Viz
#' @export
#' @rdname DaparViz-converter
#' @return An object of class `DaparViz`.
#' @import PSMatch
#'
setMethod("convert2Viz",
  signature = "MSnSet",
  #' @title xxx
  #' @param object An instance of class `MSnSet`.
  #' @param ... xxx
  function(object, ...) {
    args <- list(
      qdata = GetSlotQdata(object),
      metacell = GetSlotMetacell(object),
      metadata = GetSlotMetadata(object),
      colID = GetSlotColID(object),
      proteinID = GetSlotProteinID(object),
      conds = GetSlotConds(object),
      type = GetSlotType(object),
      adjMat = matrix(),
      cc = list()
    )

    if (args$type == "peptide") {
      .dat <- args$metadata[, args$proteinID]
      args$adjMat <- PSMatch::makeAdjacencyMatrix(.dat)
      rownames(args$adjMat) <- rownames(args$metadata)
      connectedComp <- PSMatch::ConnectedComponents(args$adjMat)
      args$cc <- connectedComp@adjMatrices
    }

    ll.tmp <- list()
    ll.tmp[["original"]] <- do.call(DaparViz, args)
    ll.tmp
  }
)




#' @title Formats available in DaparViz
#' @description
#' Gives the list of classes for the datasets which be can converted by the
#' function `convert2Viz()`.
#'
#' @return A vector of classes compliant with the package DaparViz.
#' @examples
#' FormatAvailables()
#'
#' @export
#'
FormatAvailables <- function() {
  c(
    "QFeatures",
    "MSnSet",
    "SummarizedExperiment",
    "MultiAssayExperment",
    "list"
  )
}


#' #' @title Get the class of object
#' #' @description This function returns the class of the object passed as
#' #' argument, or the class of its items if the object is a `list`. If the
#' #' class is not managed by DaparViz, then return NA For a list of managed
#' #' classes.
#' #' see `FormatAvailables()`.
#' #' @param ll An object.
#' #' @rdname Convert2VizList
#' #'
#' CheckClass <- function(ll){
#'   ll.class <- NULL
#'   if (inherits(ll, 'QFeatures'))
#'     ll.class <- 'QFeatures'
#'   else if (inherits(ll, 'list')){
#'     if (sum(unlist(lapply(ll,
#'     function(x) inherits(x, 'MSnSet')))) == length(ll)
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
