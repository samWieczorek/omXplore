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
#' @name Viz_Classes
#' 
NULL



#' 
#' @slot ll.vizData xxx
#' 
#' @example examples/ex_VizData_Class.R
#'
#' @rdname Viz_Classes
#' @export VizList
#' @exportClass VizList
VizList <- setClass(
  
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



#' @title xxx
#' @description xxx
#' @param object xxx
#' @rdname Viz_Classes
#' 
# setMethod("show", 'VizList',
#           function(object){
#             #browser()
#             lapply(object, function(x) x)
#           }
# )

#' @title Initialization method for the class `VizList`
#' @rdname Viz_Classes
#' 
setMethod("initialize" , "VizList" ,
          #' @param .Object xxx
          #' @param ll.vizData xxx
          function(.Object, ll.vizData){
            .Object@ll.vizData <- ll.vizData
            
            return(.Object)
          }
)


#' @title xxx
#' @description xxx
#' @export 
#' @return An instance of 'VizList' class
VizList <- function(ll.vizData = NULL){
  new(Class ="VizList",
      ll.vizData = ll.vizData)
}




#' @title xxx
#' @description xxx
#' @export GetVizData
#' @return NA
setMethod("GetVizData", signature = "VizList",
          function(object, i, ...) {
            return(object@ll.vizData[[i]])
          }
)

