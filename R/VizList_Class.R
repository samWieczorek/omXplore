#' @title DaparVizData class definition
#'
#' @description xxx
#'
#' @param ll.vizData xxx
#' 
#' @example inst/extdata/examples/ex_VizData_Class.R
#'
#' @name VizList-class
#' 
NULL




#' @rdname VizList-class
#' @exportClass VizList
#' 
VizList <- setClass(
 
  #' @slot ll.vizData xxx
  
  "VizList",
  
  representation(
    ll.vizData = "list"),
  
  # Set the default values for the slots. (optional)
  prototype(
    ll.vizData = list()),
  
  validity = function(object) {
    errors <- character()
    
    if (sum(unlist(lapply(ll, function(x) inherits(x, 'VizData')))) != length(object)){
      msg <- "All the items contented in the list are not instances of 'VizData' class."
      errors <- c(errors, msg)
    }
    
    
    if (length(names(object)) == length(object)){
      msg <- "All the items are not named."
      errors <- c(errors, msg)
    }
    
    
    if (length(errors) == 0) TRUE else errors
  }
  
)



#' @rdname VizList-class
#' 
setMethod("initialize" , "VizList" ,
          #' @param .Object xxx
          #' @param ll.vizData xxx
          function(.Object, ll.vizData){
            .Object@ll.vizData <- ll.vizData
            
            return(.Object)
          }
)




#' @export 
#' @rdname VizList-class
#' @return An instance of 'VizList' class
VizList <- function(ll.vizData = NULL){
  new(Class ="VizList",
      ll.vizData = ll.vizData)
}


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
#' @rdname VizList-class
#' 
setMethod('[', signature = c('VizList', "ANY", "ANY", "ANY"),
          function(x, i, j = NA, k = NA, l = NA, ..., drop) {
            x@ll.vizData[[i]]
          })


#' @title xxx
#' @description xxx
#' @param x xxx
#' @rdname VizList-class
#' 
setMethod('length', signature = c('VizList'),
          function(x) {
            length(x@ll.vizData)
          })


#' @title xxx
#' @description xxx
#' @param x xxx
#' @rdname VizList-class
#' 
setMethod('names', signature = c('VizList'),
          function(x) {
            names(x@ll.vizData)
          })