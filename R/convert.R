#' @title Convert datasets to `DaparViz` class
#' @description These functions are converters for different data formats
#' to an object of class `DaparViz` which is the format used by functions
#' in the package `DaparViz`.
#'
#' @name DaparViz-converter
#'
#' @param object An instance of class `QFeatures`.
#'
#' @examples
#'
#' # Conversion of an object of class `MSnSet`
#' data(sub_Exp1_R2_pept_MSnSet)
#' obj <- convert2VizList(sub_Exp1_R2_pept_MSnSet)
#' obj
#'
#' # Conversion of an object of class `QFeatures`
#' data(sub_Exp1_R2_pept_qf)
#' obj <- convert2VizList(sub_Exp1_R2_pept_qf)
#' obj
#' 
#' @aliases convert2VizList
#'
#' @return An instance of class `VizList`
#' 
#' @export
#'
convert2VizList <- function(obj){
  convert.obj <- VizList()
  
  # Create an empty VizList object
  if(is.null(obj))
    convert.obj <- VizList()
  
  
  # Convert a list of MSnSet
  if (inherits(obj, 'list')){
    if (is.listOf(obj, 'MSnSet') ||
        is.listOf(obj, 'SummarizedExperiment')){
      convert.obj <- VizList()
      for (i in seq(length(obj))){
        args <- ExtractInfos(obj[[i]])
        
        if(!is.null(names(obj)[i]))
        .name <- names(obj)[i]
        else
          .name <- paste0('object_', i)
        
        convert.obj@ll.daparViz[[.name]] <- do.call(DaparViz, args)
      }
    }
  }
  
  if (inherits(obj, 'MSnSet')) {
    args <- ExtractInfos(obj)
    convert.obj <- VizList(list('original' = do.call(DaparViz, args)))
  }
  

  if (inherits(obj, 'MultiAssayExperiment') ||
      inherits(obj, 'QFeatures')){
    
    convert.obj <- convertMAEtype(obj)
  }


  
  if (inherits(obj, 'SummarizedExperiment')){
    
    args <- ExtractInfos(obj)
    convert.obj <- VizList(list('original' = do.call(DaparViz, args)))
  }


 return(convert.obj)
}



is.listOf <- function(object, obj.class)
  all(unlist(lapply(object, function(x) inherits(x, obj.class)), 
    use.names=FALSE))





#' @title Formats available in DaparViz
#' @description
#' Gives the list of classes for the datasets which be can converted by the
#' function `convert2VizList()`.
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
