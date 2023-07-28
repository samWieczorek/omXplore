#' @title VizData class
#'
#' @description
#'
#'  Conceptually, a `VizData` object is a standard representation of all
#'  elements from quantitative other structured data used in proteomics, such
#'  as `MSnset` or `QFeatures`. It allows to use it as a generic converter.
#'  
#'  `VizData` objects are not usually created by hand but are created from the `VizList` 
#'  which is the class accessible by the user.
#'  
#'  The recommended way to create `VizData` objects is the use the
#' `xxxx()` function of the class `VizList`
#' 
#' @params vData An instance of class [VizData]
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
#'
#' @name VizData-class
#' 
NULL


#' @slot qdata xxx
#' @slot metacell xxx
#' @slot metadata xxx
#' @slot colID xxx
#' @slot proteinID xxx
#' @slot conds xxx
#' @slot type xxx
#' @slot adjMat xxx
#' @slot cc xxx
#' 
#' @example examples/ex_VizData_Class.R
#'
#' @rdname VizData-class
#' @export VizData
#' @exportClass VizData
VizData <- setClass(
  
  "VizData",
  
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




#' @exportMethod show
#' @rdname VizData-class
#' 
setMethod("show", 'VizData',
          function(vData){
            require(crayon)
            cat(crayon::green(paste0('\tdim(qdata): ', dim(vData@qdata), '\n')))
            
            cat(crayon::green(paste0('\tdim(metacell): ', dim(vData@metacell), '\n')))
            
            cat(crayon::green('\tconds: '))
            cat(crayon::green(vData@conds))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\ttype: '))
            cat(crayon::green(vData@type))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tcolID: '))
            cat(crayon::green(vData@colID))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tproteinID: '))
            cat(crayon::green(vData@proteinID))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tDimensions of adjacency matriX: '))
            cat(crayon::green(paste0(dim(vData@adjMat)[1], ' x ', dim(vData@adjMat)[2])))
            cat(crayon::green('\n'))
            
            cat(crayon::green('\tNumber of connected components: '))
            cat(crayon::green(length(vData@cc)))
            cat(crayon::green('\n'))

              }
)

#' @title Initialization method for the class `VizData`
#' @rdname VizData-class
#' 
setMethod("initialize" , "VizData" ,
          
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
      
      .Object@colID <- if(is.null(colID) || length(colID)==0 || !inherits(colID, 'character')) '' else colID
      
      .Object@proteinID <- if(is.null(proteinID) || length(proteinID)==0 || !inherits(proteinID, 'character')) '' else proteinID
      
      .Object@conds <- if(is.null(conds) || !is.vector(conds)) c() else conds
      
      .Object@type <-  if(is.null(type) || length(type)==0 || !inherits(type, 'character')) '' else type
     
      .Object@cc <- if(is.null(cc) || !inherits(cc, 'list')) list() else cc
    
      .Object@adjMat <- if(is.null(adjMat) || !inherits(adjMat, 'matrix')) matrix() else adjMat
      
      return(.Object)
    }
)

setMethod('[', signature = c('VizList', "ANY", "ANY", "ANY"),
          function(x, i, j = NA, k = NA, l = NA, ..., drop) {

            lapply(x@ll.vizData, function(obj){
              obj <- obj[i, ]
            })
          })


setMethod('[', signature = c('VizData', "ANY", "ANY", "ANY"),
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


#' @exportMethod Convert2VizList
#' 
#' @rdname Convert2VizList
#' 
setMethod("Convert2VizList", signature = "QFeatures",
          
  function(object) {
    require(PSMatch)
    require(QFeatures)
    require(SummarizedExperiment)
    
    ll <- list()
    for (i in 1:length(object)){
      metacell.backup <- qMetacell(object[[i]])
      mdata <- SummarizedExperiment::rowData(object[[i]])
      X <- matrix()
      cc <- list()
      
      if ("qMetacell" %in% names(mdata))
        mdata <- mdata[, -which(names(mdata)=="qMetacell")]
      if ("adjacencyMatrix" %in% names(mdata))
        mdata <- mdata[, -which(names(mdata)=="adjacencyMatrix")]


      if (typeDataset(object[[i]]) == 'peptide'){
        # Create the adjacency matrix
        X <- PSMatch::makeAdjacencyMatrix(
          SummarizedExperiment::rowData(object[[i]])[, parentProtId(object[[i]])])
        rownames(X) <- rownames(SummarizedExperiment::rowData(object[[i]]))
        
        
        # Create the connected components
        cc <- PSMatch::ConnectedComponents(X)@adjMatrices
      }
      
      ll[[names(object[i])]] <- new(Class ="VizData",
                                    qdata = SummarizedExperiment::assay(object[[i]]),
                                    metacell = metacell.backup,
                                    metadata = as.data.frame(mdata),
                                    colID = idcol(object[[i]]),
                                    proteinID = parentProtId(object[[i]]),
                                    conds = SummarizedExperiment::colData(object)$Condition,
                                    type = typeDataset(object[[i]]),
                                    adjMat = as.matrix(X),
                                    cc = as.list(cc)
                                    )

    }
    ll.vizList <- VizList(ll)
    return(ll.vizList)
    })






#' @exportMethod Convert2VizData
#' 
#' @rdname Convert2VizList
#' 
setMethod("Convert2VizData", signature = "MSnSet",
          #' @param object xxx
          #' @param ... xxx
  function(object, ...) {
    require(MSnbase)
    X <- matrix()
    cc <- list()
    
    if (object@experimentData@other$typeOfData == 'peptide'){
      X <- PSMatch::makeAdjacencyMatrix(fData(object)[, object@experimentData@other$proteinId])
      rownames(X) <- rownames(fData(object))
      connectedComp <- PSMatch::ConnectedComponents(X)
      cc <- connectedComp@adjMatrices
    }
    
    
    new(Class ="VizData",
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
  }
)




#' @rdname Convert2VizList
#' 
setMethod("Convert2VizData", signature = "list",
          #' @param object xxx
          #' @param ... xxxx
          
          function(object, ...) {
            new(Class ="VizData",
                qdata = object$qdata,
                metacell = object$metacell, 
                metadata = object$metadata,
                colID = object$colID,
                proteinID = object$proteinID,
                conds = object$conds,
                type = object$type,
                adjMat = object$adjMat,
                cc = object$cc
            )
          }
)


#' @title xxx
#' @description xxx
#' @param ll xxx
#' @rdname Convert2VizList
#' 
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


#' @title Convert a dataset to an instance of class [VizList]
#' @description
#' Actually, three types of dataset can be converted:
#' * A `list` which must be formatted in the correct format. See xxx
#' * A list of instances of class `MSnset`
#' * An instance of class `QFeatures` (which is already a list)
#' @param obj xxx
#' @rdname Convert2VizList
#' @return An instance of class [VizList]
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
             ll.tmp[[names(obj)[i]]] <- Convert2VizData(obj[[i]])
           ll <- VizList(ll.tmp)
         },
         
         list = {
           ll.tmp <- NULL
           for (i in 1:length(obj))
             ll.tmp[[names(obj)[i]]] <- Convert2VizData(obj[[i]])
           ll <- VizList(ll.tmp)
         }
         )
  return(ll)
 }
