#' @title DaparViz class
#'
#' @description
#'
#'  Conceptually, a `DaparViz` object is a standard representation of all
#'  elements from quantitative other structured data used in proteomics, such
#'  as `MSnset` or `QFeatures`. It allows to use it as a generic converter.
#'  
#'  `DaparViz` objects are not usually created by hand but are created from the `VizList` 
#'  which is the class accessible by the user.
#'  
#'  The recommended way to create `DaparViz` objects is the use the
#' `xxxx()` function of the class `VizList`
#'
#' @name DaparViz-class
#' 
#'
#' @examples
#' NULL
#' 
NULL



#'
#' @rdname DaparViz-class
#' @export DaparViz
#' @exportClass DaparViz
DaparViz <- setClass(
  
  "DaparViz",
  #' @slot qdata xxx
  #' @slot metacell xxx
  #' @slot metadata xxx
  #' @slot colID xxx
  #' @slot proteinID xxx
  #' @slot conds xxx
  #' @slot type xxx
  #' @slot adjMat xxx
  #' @slot cc xxx
  
  representation(
      qdata = "matrix",
      metacell = 'data.frame',
      metadata = 'data.frame',
      colID = "character",
      proteinID = "character",
      conds = "vector",
      type = "character",
      adjMat = 'matrix',
      cc = 'list'
      ),
  
  # Set the default values for the slots. (optional)
  prototype(
      qdata = matrix(),
      metacell = data.frame(),
      metadata = data.frame(),
      colID = NA_character_,
      proteinID = NA_character_,
      conds = c(),
      type = NA_character_,
      adjMat = NULL,
      cc = list()
    ),
  
    #' @param object xxx
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
#' @param object An instance of the class `DaparViz`
#' @exportMethod show
#' @rdname DaparViz-class
#' 
setMethod("show", 'DaparViz',
          #' @title xxx
          #' @description xxx
          #' @param object An instance of the class `DaparViz`
          #' @exportMethod show
          #' 
          function(object){
            pkgs.require('crayon')
            cat(crayon::green(paste0('\tdim(qdata): ', 
                                     dim(object@qdata)[1], ' x ',
                                     dim(object@qdata)[2], '\n')))
            
            cat(crayon::green(paste0('\tdim(metacell): ', 
                                     dim(object@metacell)[1],  ' x ',
                                     dim(object@metacell)[2], '\n')))
            
            cat(crayon::green('\tconds: '))
            cat(crayon::green(object@conds))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\ttype: '))
            cat(crayon::green(object@type))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tcolID: '))
            cat(crayon::green(object@colID))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tproteinID: '))
            cat(crayon::green(object@proteinID))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tDimensions of adjacency matriX: '))
            cat(crayon::green(paste0(dim(object@adjMat)[1], ' x ', dim(object@adjMat)[2])))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tNumber of connected components: '))
            cat(crayon::green(length(object@cc)))
            cat(crayon::green('\n'))
            
            if (length(object@cc) > 0){
              cc.infos <- GetCCInfos(object@cc)
              #cat(crayon::green('\t\tDetails:\n'))
              cat(crayon::green('\t\tOne -> One: '))
              cat(crayon::green(length(cc.infos$One_One)))
              cat(crayon::green('\n'))
              cat(crayon::green('\t\tOne -> Multi: '))
              cat(crayon::green(length(cc.infos$One_Multi)))
              cat(crayon::green('\n'))
              cat(crayon::green('\t\tMulti -> Multi: '))
              cat(crayon::green(length(cc.infos$Multi_Multi)))
              cat(crayon::green('\n'))
              
            }

              }
)

#' @title Initialization method for the class `DaparViz`
#' @rdname DaparViz-class
#' 
setMethod("initialize" , "DaparViz" ,
          #' @param .Object xxx
          #' @param qdata xxx
          #' @param metacell xxx
          #' @param metadata xxx
          #' @param colID xxx
          #' @param proteinID xxx
          #' @param conds xxx
          #' @param type xxx
          #' @param adjMat xxx
          #' @param cc xxx     
    function(.Object,
             qdata,
             metacell,
             metadata,
             colID,
             proteinID,
             conds,
             type,
             adjMat,
             cc){
        

      .Object@qdata <- if(is.null(qdata) || !inherits(qdata, 'matrix')) matrix() else qdata
      
      .Object@metacell <- if(is.null(metacell) || !inherits(metacell, 'data.frame')) data.frame() else metacell

      .Object@metadata <-  if(is.null(metadata) || !inherits(metadata, 'data.frame')) data.frame() else metadata
      
      .Object@colID <- if(is.null(colID) || colID=="" || length(colID)==0 || !inherits(colID, 'character')) '' else colID
      
      .Object@proteinID <- if(is.null(proteinID) || length(proteinID)==0 || !inherits(proteinID, 'character')) '' else proteinID
      
      .Object@conds <- if(is.null(conds) || !is.vector(conds)) c() else conds
      
      .Object@type <-  if(is.null(type) || length(type)==0 || !inherits(type, 'character')) '' else type
     
      .Object@cc <- if(is.null(cc) || !inherits(cc, 'list')) list() else cc
    
      .Object@adjMat <- if(is.null(adjMat) || !inherits(adjMat, 'matrix')) matrix() else adjMat
      
      return(.Object)
    }
)


#' @title xxx
#' @description xxx
#' @param x xxx
#' @param i xxx
#' @param j xxx
#' @param k xxx
#' @param l xxx
#' @param ... xxx
#' @param drop xxx
#' 
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod('[', signature = c('DaparViz', "ANY", "ANY", "ANY"),
          function(x, i, j = NA, k = NA, l = NA, ..., drop) {
            if (length(i) == 1){
              tmp <- as.matrix(t((x@qdata)[i, ]))
              rownames(tmp) <- (rownames(x@qdata))[i]
              x@qdata <- tmp
              x@metadata <- as.data.frame((x@metadata)[i, ])
              x@metacell <- as.data.frame((x@metacell)[i, ])
            } else if (length(i) > 1){
              x@qdata <- (x@qdata)[i, ]
            }
            
             x@metadata <- as.data.frame((x@metadata)[i, ])
             x@metacell <- as.data.frame((x@metacell)[i, ])
             
             # Update adjacency matrix and connected components
             if (x@type == 'peptide'){
               
               X <- PSMatch::makeAdjacencyMatrix((x@metadata)[, x@proteinID])
               rownames(X) <- rownames(x@metadata)
               
               # Create the connected components
               cc <- PSMatch::ConnectedComponents(X)@adjMatrices
               
               x@adjMat <- as.matrix(X)
               x@cc <- as.list(cc)
             }
            return(x)  
})


#' @exportMethod convert2Viz
#' 
#' @rdname DaparViz-class
#' @import QFeatures
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("convert2Viz", signature = "QFeatures",
  #' @param object An instance of class `QFeatures`.
  function(object) {
    
    ll <- list()
    for (i in 1:length(object)){
      #metacell.backup <- qMetacell(object[[i]])
      mdata <- rowData(object[[i]])
      X <- matrix()
      cc <- list()
      
      if ("qMetacell" %in% names(mdata)){
        metacell.backup <- rowData(object)[[i]]$qMetacell
        mdata <- mdata[, -which(names(mdata)=="qMetacell")]
      }
      
      if ("adjacencyMatrix" %in% names(mdata))
        mdata <- mdata[, -which(names(mdata)=="adjacencyMatrix")]


      if (metadata(object[[i]])$typeDataset == 'peptide'){
        # Create the adjacency matrix
        parentProt <- metadata(object[[i]])$parentProtId
        X <- PSMatch::makeAdjacencyMatrix(
          rowData(object[[i]])[, parentProt])
        rownames(X) <- rownames(rowData(object[[i]]))
        
        # Create the connected components
        cc <- PSMatch::ConnectedComponents(X)@adjMatrices
      }

      ll[[names(object)[i]]] <- DaparViz(qdata = assay(object[[i]]),
                                        metacell = rowData(object)[[i]]$qMetacell,
                                        metadata = as.data.frame(mdata),
                                        colID = metadata(object[[i]])$idcol,
                                        proteinID = metadata(object[[i]])$parentProtId,
                                        conds = colData(object)$Condition,
                                        type = metadata(object[[i]])$typeDataset,
                                        adjMat = as.matrix(X),
                                        cc = as.list(cc)
      )
      }
    ll
    })






#' 
#' @rdname DaparViz-class
#' @import MSnbase
#' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
#' 
setMethod("convert2Viz", signature = "MSnSet",
          #' @param object An instance of class `MSnSet`.
          #' @param ... xxx
  function(object, ...) {
    pkgs.require('PSMatch')
    
    X <- matrix()
    cc <- list()
    
    if (object@experimentData@other$typeOfData == 'peptide'){
      X <- PSMatch::makeAdjacencyMatrix(fData(object)[, object@experimentData@other$proteinId])
      rownames(X) <- rownames(fData(object))
      connectedComp <- PSMatch::ConnectedComponents(X)
      cc <- connectedComp@adjMatrices
    }
    
    ll.tmp <- list()
    ll.tmp[['original']] <- new(Class ="DaparViz",
                                qdata = exprs(object),
                                metacell = fData(object)[, object@experimentData@other$names_metacell],
                                metadata = fData(object),
                                colID = object@experimentData@other$keyId,
                                proteinID = object@experimentData@other$proteinId,
                                conds = pData(object)$Condition,
                                type = object@experimentData@other$typeOfData,
                                adjMat = as.matrix(X),
                                cc = as.list(cc)
                                )
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
  c('QFeatures', 'MSnSet', 'list')


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
