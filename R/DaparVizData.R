#' @title DaparVizData class definition
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
#' @name DaparVizData
#' 
NULL



#' 
#' @slot name xxx
#' @slot qdata xxx
#' @slot metacell xxx
#' @slot conds xxx
#' @slot type xxx
#' 
#' @example examples/example_DaparVizData_class.R
#'
#' @rdname DaparVizData
#' @export DaparVizData
#' @exportClass DaparVizData
DaparVizData <- setClass(
  
  "DaparVizData",
  
  representation(
      name = "character",
      qdata = "matrix",
      metacell = 'data.frame',
      metadata = 'data.frame',
      colID = "character",
      conds = "vector",
      type = "character"
      ),
  
  # Set the default values for the slots. (optional)
  prototype(
      name = NA_character_,
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
#' 
setMethod("show", 'DaparVizData',
          function(object){
            cat(crayon::green('\t ------- DaparVizData -------\n'))
            cat(crayon::green(paste0('\tname: ', object@name, '\n')))
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

#' @title Initialization method for the class `DaparVizData`
#' @rdname DaparVizData
#' 
setMethod("initialize" , "DaparVizData" ,
    #' @param .Object xxx
    #' @param name xxx
    #' @param qdata xxx
    #' @param metacell xxx
    #' @param conds xxx
    #' @param type xxx
    function(.Object,
             name,
             qdata,
             metacell,
             metadata,
             colID,
             conds,
             type){
        
        # Basic init of slots
      .Object@name <- name
      
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

#' @title Constructor of the `DaparVizData` class
#' 
#' @description Wrapper function to the constructor of the class
#' 
#' @rdname DaparVizData
#' 
#' @param fullname xxx
#' @param mode xxx
#' @param steps xxx
#' @param mandatory xxx
#' 

#' #' @exportMethod qMetacell
#' #' @rdname QFeatures-accessors
#' #' @return NA
setMethod("Build_DaparVizData", "list",
          function(object) {
            
            new(Class ="DaparVizData",
                name = object$name,
                qdata = object$qdata,
                metadata = object$metadata,
                colID = object$colID,
                metacell = object$metacell,
                conds = object$conds,
                type = object$type
            )
          }
)




#' @exportMethod Build_DaparVizData
#' @rdname DaparVizData-accessors
#' @return NA
setMethod("Build_DaparVizData", signature = "QFeatures",
  function(object, i) {
    mdata <- rowData(object[[i]])
    if ("qMetacell" %in% names(mdata))
      mdata <- mdata[, -which(names(mdata)=="qMetacell")]
    
    if ("adjacencyMatrix" %in% names(mdata))
      mdata <- mdata[, -which(names(mdata)==-"adjacencyMatrix")]
    
    new(Class ="DaparVizData",
        name =  names(object[i]) ,
        qdata = assay(object[[i]]),
        metacell = qMetacell(object[[i]]),
        metadata = mdata,
        colID = idcol(object[[i]]),
        conds = colData(object)$Condition,
        type = typeDataset(object[[i]])
        )
  }
)




#' @export Build_DaparVizData
#' @return NA
setMethod("Build_DaparVizData", signature = "MSnSet",
  function(object, ...) {
    new(Class ="DaparVizData",
        name = deparse(substitute(object)) ,
        qdata = exprs(object),
        metacell = fData(object)[, object@experimentData@other$names_metacell], 
        metadata = fData(object),
        colID = object@experimentData@other$keyId,
        conds = pData(object)$Condition,
        type = object@experimentData@other$typeOfData
        )
  }
)

