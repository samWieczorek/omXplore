#' @title Convert lists of data to `VizList` class
#' @description These functions are converters for different data formats
#' to an object of class `VizList` which is the format used by functions
#' in the package `omXplore`.
#'
#' @rdname VizList-class-converter
#'
#' @param obj A list of lists.
#'
#' @examples
#' ll.data1 <- build_toylist_example('data1')
#'.ll.data2 <- build_toylist_example('data2')
#' data <- convert2VizList(list(ll.data1))
#' data <- convert2VizList(list(ll.data1, ll.data2))
#' 
#' @aliases convert2VizList
#'
#' @return An instance of class `VizList`
#' 
#' @export
#'
convert2VizList <- function(obj){
  
  
  stopifnot(inherits(obj, 'list'))
  stopifnot(is.listOf(obj, 'list'))
  
  
  convert.obj <- VizList()
  
  # Create an empty VizList object
  if(is.null(obj))
    convert.obj <- VizList()
  
  # Convert a list of MSnSet
  if (inherits(obj, 'list')){
    convert.obj <- VizList()
    for (i in seq(length(obj))){
      args <- obj[[i]]
      
      if(!is.null(names(obj)[i]))
        .name <- names(obj)[i]
      else
        .name <- paste0('object_', i)
      
      convert.obj@ll.VizData[[.name]] <- do.call(VizData, args)
    }
  }
  
  return(convert.obj)
}



is.listOf <- function(object, obj.class)
  all(unlist(lapply(object, function(x) inherits(x, obj.class)), 
    use.names=FALSE))





  # convert2VizList <- function(obj){
#   convert.obj <- VizList()
#   
#   # Create an empty VizList object
#   if(is.null(obj))
#     convert.obj <- VizList()
#   
#   
#   # Convert a list of MSnSet
#   if (inherits(obj, 'list')){
#     if (is.listOf(obj, 'MSnSet') || is.listOf(obj, 'SummarizedExperiment')){
#       convert.obj <- VizList()
#       for (i in seq(length(obj))){
#         args <- ExtractInfos(obj[[i]])
#         
#         if(!is.null(names(obj)[i]))
#           .name <- names(obj)[i]
#         else
#           .name <- paste0('object_', i)
#         
#         convert.obj@ll.VizData[[.name]] <- do.call(VizData, args)
#       }
#     } else if (is.listOf(obj, 'VizData')){
#       convert.obj <- VizList()
#       for (i in seq(length(obj))){
#         if(!is.null(names(obj)[i]))
#           .name <- names(obj)[i]
#         else
#           .name <- paste0('object_', i)
#         
#         convert.obj@ll.VizData[[.name]] <- obj[[i]]
#         }
#     } else if (is.listOf(obj, 'list')){
#       convert.obj <- VizList()
#       for (i in seq(length(obj))){
#         args <- obj[[i]]
#         
#         if(!is.null(names(obj)[i]))
#           .name <- names(obj)[i]
#         else
#           .name <- paste0('object_', i)
#         
#         convert.obj@ll.VizData[[.name]] <- do.call(VizData, args)
#       }
#     }
#   }
#   
#   if (inherits(obj, 'MSnSet')) {
#     args <- ExtractInfos(obj)
#     convert.obj <- VizList(list('original' = do.call(VizData, args)))
#   }
#   
# 
#   if (inherits(obj, 'MultiAssayExperiment') || inherits(obj, 'QFeatures')){
#     convert.obj <- VizList()
#     for (i in seq(length(obj))){
#       args <- ExtractInfos(obj[[i]])
#       
#       if(!is.null(names(obj)[i]))
#         .name <- names(obj)[i]
#       else
#         .name <- paste0('object_', i)
#       
#       convert.obj@ll.VizData[[.name]] <- do.call(VizData, args)
#     }
#   }








#' 
#' 
#' #' @title Formats available in omXplore
#' #' @description
#' #' Gives the list of classes for the datasets which be can converted by the
#' #' function `convert2VizList()`.
#' #'
#' #' @return A vector of classes compliant with the package omXplore
#' #' @examples
#' #' FormatAvailables()
#' #'
#' #' @export
#' #'
#' FormatAvailables <- function() {
#'   c(
#'     "QFeatures",
#'     "MSnSet",
#'     "SummarizedExperiment",
#'     "MultiAssayExperment",
#'     "list (of MSnSet)",
#'     "list (specific format)"
#'   )
#' }
