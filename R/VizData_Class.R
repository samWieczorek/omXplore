#' @title VizData class definition
#'
#' @description
#'
#' This class is used to store the configuration of any process
#' used with MagellanNTK It contains a validity function to ensure
#' that the format is correct.
#' 
#' Validity:
#' * The first step must be called 'Description', it is a mandatory step. Thus, 
#' the first item of the mandatory vector is TRUE.
#' To be continued...
#' 
#' ## Initialization
#'  ### Generic process
#'  
#'  A generic process
#'  * Generic pipeline : xxxx
#'  * Description pipeline: This case is for a process -called 'Description' which is 
#'  the first process module of a pipeline
#'
#' @name Viz_Classes
#' 
NULL




#' @slot qdata xxx
#' @slot metacell xxx
#' @slot conds xxx
#' @slot type xxx
#' 
#' @example examples/ex_VizData_Class.R
#'
#' @rdname Viz_Classes
#' @export VizData
#' @exportClass VizData
VizData <- setClass(
  
  "VizData",
  
  representation(
      qdata = "matrix",
      metacell = 'data.frame',
      metadata = 'data.frame',
      colID = "character",
      conds = "vector",
      type = "character"
      ),
  
  # Set the default values for the slots. (optional)
  prototype(
      qdata = matrix(),
      metacell = data.frame(),
      metadata = data.frame(),
      colID = NA_character_,
      conds = c(),
      type = NA_character_
    ),
  
    validity = function(object) {
      errors <- character()
      
      if (nrow(object@qdata) != nrow(object@metacell)){
        msg <- "The slots 'qdata' and 'metacell' must have the same number of rows."
        errors <- c(errors, msg)
      }
      
      if (nrow(object@qdata) != nrow(object@metadata)){
        msg <- "The slots 'qdata' and 'metadata' must have the same number of rows."
        errors <- c(errors, msg)
      }
      
      
      if (ncol(object@qdata) != length(object@conds)){
        msg <- "The slots 'qdata' must have the same number of columns as the length of 'conds'."
        errors <- c(errors, msg)
      }
      
      
      if (length(errors) == 0) TRUE else errors
    }

)



#' @title xxx
#' @description xxx
#' @param object xxx
#' @rdname Viz_Classes
#' 
setMethod("show", 'VizData',
          function(object){
            cat(crayon::green(paste0('\tdim(qdata): ', dim(object@qdata), '\n')))
            cat(crayon::green(paste0('\tdim(metacell): ', dim(object@metacell), '\n')))
            
            cat(crayon::green('\tconds: '))
            cat(crayon::green(object@conds))
            cat(crayon::green('\n'))
            cat(crayon::green('\ttype: '))
            cat(crayon::green(object@type))
            cat(crayon::green('\n'))
              }
)

#' @title Initialization method for the class `VizData`
#' @rdname Viz_Classes
#' 
setMethod("initialize" , "VizData" ,
    #' @param .Object xxx
    #' @param qdata xxx
    #' @param metacell xxx
    #' @param conds xxx
    #' @param type xxx
    function(.Object,
             qdata,
             metacell,
             metadata,
             colID,
             conds,
             type){
        
        # Basic init of slots
      if(is.null(qdata) || !inherits(qdata, 'matrix'))
        .Object@qdata <- matrix()
      else 
        .Object@qdata <- qdata
      
      if(is.null(metacell) || !inherits(metacell, 'data.frame'))
        .Object@metacell <- data.frame()
      else
        .Object@metacell <- metacell

      if(is.null(metadata) || !inherits(metadata, 'data.frame'))
        .Object@metadata <- data.frame()
      else
        .Object@metadata <- metadata
      
      
      if(is.null(colID) || length(colID)==0 || !inherits(colID, 'character'))
        .Object@colID <- ''
      else 
        .Object@colID <- colID
      
      
      
      if(is.null(conds) || !is.vector(conds))
        .Object@conds <- c()
      else 
        .Object@conds <- conds
      
      if(is.null(type) || length(type)==0 || !inherits(type, 'character'))
        .Object@type <- ''
      else 
        .Object@type <- type
      
      return(.Object)
    }
)



#' @title xxx
#' @description xxx
#' @exportMethod Convert2VizList
#' @rdname Viz_Classes
#' @return NA
setMethod("Convert2VizList", signature = "QFeatures",
  function(object) {
    ll <- list()
    for (i in 1:length(object)){
      metacell.backup <- qMetacell(object[[i]])
      mdata <- rowData(object[[i]])
      if ("qMetacell" %in% names(mdata))
        mdata <- mdata[, -which(names(mdata)=="qMetacell")]
    
      if ("adjacencyMatrix" %in% names(mdata))
        mdata <- mdata[, -which(names(mdata)=="adjacencyMatrix")]
  
      ll[[names(object[i])]] <- new(Class ="VizData",
                                    qdata = assay(object[[i]]),
                                    metacell = metacell.backup,
                                    metadata = as.data.frame(mdata),
                                    colID = idcol(object[[i]]),
                                    conds = colData(object)$Condition,
                                    type = typeDataset(object[[i]])
                                    )
    }
    #browser()
    
    ll.vizList <- VizList(ll)
    return(ll.vizList)
    })




#' @title xxx
#' @description xxx
#' @export Convert2VizData
#' @return An instance of 'VizData' class
setMethod("Convert2VizData", signature = "MSnSet",
  function(object, ...) {
    new(Class ="VizData",
        qdata = exprs(object),
        metacell = fData(object)[, object@experimentData@other$names_metacell], 
        metadata = fData(object),
        colID = object@experimentData@other$keyId,
        conds = pData(object)$Condition,
        type = object@experimentData@other$typeOfData
        )
  }
)

#' @title xxx
#' @description xxx
#' @export Convert2VizData
#' @return An instance of 'VizData' class
setMethod("Convert2VizData", signature = "list",
          function(object, ...) {
            new(Class ="VizData",
                qdata = object$qdata,
                metacell = object$metacell, 
                metadata = object$metadata,
                colID = object$colID,
                conds = object$conds,
                type = object$type
            )
          }
)


#' @title xxx
#' @description xxx
CheckClass <- function(ll){
  ll.class <- NULL
  if (inherits(ll, 'QFeatures'))
    ll.class <- 'QFeatures'
  else if (sum(unlist(lapply(ll, function(x) inherits(x, 'MSnSet')))) == length(ll)
           && length(names(ll)) == length(ll))
    ll.class <- 'MSnSet'
  else if (inherits(ll, 'list'))
    ll.class <- 'list'
  else
    ll.class <- NA
  return(ll.class)
}


#' @export
convert2viz <- function(obj){
  # Checks if each item is an instance of 'MSnSet' class
  ll.class <- CheckClass(obj)
  ll <- NULL
  switch(ll.class,
         QFeatures = ll <- Convert2VizList(obj),
         MSnSet = {
           ll.tmp <- NULL
           for (i in 1:length(obj))
             ll[[names(obj)[i]]] <- Convert2VizData(obj[[i]])
           #ll <- Convert2VizList(ll.tmp)
         },
         list = {
           for (i in 1:length(obj))
             ll[[names(obj)[i]]] <- Convert2VizData(obj[[i]])
         }
         )
  return(ll)
 }
