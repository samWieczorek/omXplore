##' @title VizList class
##'
##' @description
##'
##'  Conceptually, a `VizList` object is a standard representation of all
##'  elements from quantitative other structured data used in proteomics, such
##'  as `MSnset`, `QFeatures`, `SummarizedExperiment` or `MultAssayExperiment`.
##'  It allows to use it as a generic converter.
##'
##'  `VizData` objects are not usually created by hand but are created from
##'  the `VizList` which is the class accessible by the user.
##'
##'  The recommended way to create `VizData` objects is the use the
##' function `convert2viz()` of the class `VizList`
##'
##' @name VizList-class
##'
##' @param object An instance of the class `VizList`
##'
##' @param x An instance of class [QFeatures].
##'
##' @param i `character()`, `integer()`, `logical()` or `GRanges()`
##'     object for subsetting by rows.
##'
##' @param j `character()`, `logical()`, or `numeric()` vector for
##'     subsetting by `colData` rows.
##'
##' @param k `character()`, `logical()`, or `numeric()` vector for
##'     subsetting by assays
##'
##' @param l `character()`, `logical()`, or `numeric()` vector for
##'     subsetting by assays
##'
##' @param ... Additional parameters
##'
##' @param drop logical (default `TRUE`) whether to drop empty assay
##'     elements in the `ExperimentList`.
##'
##' @return See individual method description for the return value.
##'
##' @seealso
##' The `VizData()` constructor and the `convert2Viz()`
##' function. The *VizData* vignette provides an extended example.
##'
##' @exportClass VizList
##'
##' @author Samuel Wieczorek
##'
##' @examples
##' ## ------------------------
##' ## An empty VizData object
##' ## ------------------------
##'
##' VizList()
##'
##' ## -----------------------------------
##' ## Creating a VizData object manually
##' ## -----------------------------------
##' 
##'
NULL



##'
##' @rdname VizList-class
##' @export VizList
##' @exportClass VizList
##' @return An instance of the class `VizList`
VizList <- setClass(
  
  "VizList",
  ##' @slot ll.VizData A matrix containing the quantitative data

  representation(
    ll.VizData = "list"
  ),
  
  # Set the default values for the slots. (optional)
  prototype(
    ll.VizData = list()
  ),
  
  ##' @param object xxx
  validity = function(object) {
    errors <- character()
    
    if (inherits(object, 'list')){
      msg <- "The object is not a list"
      errors <- c(errors, msg)
    }

    if (!is.listOf(obj, 'VizData')) {
      msg <- "All the items contained in this list are not of class 'VizData'"
      errors <- c(errors, msg)
    }
    
    if (length(errors) == 0) TRUE else errors
  }
)


##' @exportMethod show
##' @rdname VizList-class
##' @return NA
##'
setMethod(
  "show", "VizList",
  ##' @param object An instance of the class `VizList`
  ##' @exportMethod show
  ##'
  function(object) {
    if(length(object@ll.VizData) > 0){
      lapply(seq(length(object@ll.VizData)), 
        function(x){
          cat(names(object)[x], '\n')
          showVizData(object@ll.VizData[[x]])
          })
    } else
      message("Empty object")
  }
)



##' @title Initialization method for the class `VizList`
##' @rdname VizList-class
##' @return An instance of the class `VizList`
##'
setMethod(
  "initialize", "VizList",
  ##' @param .Object xxx
  ##' @param ll.VizData xxx
  function(
    .Object,
    ll.VizData = list()) {

    .Object@ll.VizData <- list()
    
    if (is.null(ll.VizData)){
      message("The parameter 'object' is NULL")
      message("Initialization with an empty class")
    } else if (!inherits(ll.VizData, "list")){
      message("The parameter 'object' is not a list")
      message("Initialization with an empty class")
    } else if (is.listOf(ll.VizData, 'VizData')){
      message("The parameter 'object' is not a list of VizData objects")
      message("Initialization with an empty class")
      } else if (!inherits(ll.VizData, "VizData")){
        message("The parameter 'object' is not a VizData object")
        message("Initialization with an empty class")
        } else {
          if (inherits(ll.VizData, "VizData"))
            .Object@ll.VizData <- list(ll.VizData)
          else if (is.listOf(ll.VizData, 'VizData'))
            .Object@ll.VizData <- ll.VizData
          else if (inherits(ll.VizData, "list") && 
              is.listOf(ll.VizData, "list"))
            .Object <- convert2VizList(ll.VizData)
          }

    return(.Object)
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("[",
  signature = c("VizList", "ANY"),
  function(x, i) {
    return(x@ll.VizData[i])
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("[[",
  signature = c("VizList", "ANY"),
  function(x, i) {
    return(x@ll.VizData[[i]])
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("names",
  signature = c("VizList"),
  function(x) {
    return(names(x@ll.VizData))
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("length",
  signature = c("VizList"),
  function(x) {
    return(length(x@ll.VizData))
  }
)
