#' @title Modules functions
#' 
#' @description
#' xxxxx
#' * `listShinyApps()`: xxx
#' * `addImgPath()`: xxx
#' * `addModules()`: xxx
#' 
#' @param location xxx
#' 
#' @name DaparViz-modules
#' 
#' 
#' 
#' 
NULL

#' @title Add resource path for image
#' 
#' @description This line allows to extend the resource path to a different directory
#' than the one in 'inst/' which is given by default when loading a package.
#' 
#' @param prefix A `character()` vector which contains packages names
#' @param path xxx
#' 
#' @examples 
#' \dontrun{
#' addImgPath('mod_foo', system.file('.', package='DaparViz'))
#' }
#' 
#' @export
#' 
#' @rdname DaparViz-modules
#' 
#' @author Samuel Wieczorek
#' 
addImgPath <- function(prefix, path){
  myPath <- gsub('mod_', '', path)
  addResourcePath(prefix = paste0("img_", myPath),
                  directoryPath = "my_location")
}






#' @title Add shiny module
#' @description xxxx
#' @param addons A `list` in which each item is:
#' * is named by the name of a package
#' * contains th set of modules to integrate
#' 
#' @export
#' @rdname DaparViz-modules
#'
addModules <- function(addons){
  
  if (is.null(addons) || !inherits(addons, 'list')){
    warning('xxx')
    return(NULL)
  }
  
  f_assign <- function(fun, pkg, suffix){
    f_name <- paste0(fun, '_', suffix)
    f_fullname <- paste0(pkg, '::', fun, '_', suffix)
    require(pkg, character.only=TRUE)
    if (f_name %in% ls(paste0("package:", pkg)))
      assign(f_name, eval(parse(text = f_fullname)), envir = globalenv())
  }
  
  for (x in names(addons)){
    for (m in addons[[x]]){
      f_assign(m, x, 'ui')
      f_assign(m, x, 'server')
    }
  }
}



#' @export
#' @rdname DaparViz-modules
#' 
listShinyApps <- function(location = 'both'){
  
  stopifnot(location %in% c('both', 'external', 'builtin'))
  
  builtinApps <- gsub("mod_", "", listPlotModules('builtin'))
  builtinApps <- paste0(builtinApps, '()')
  
  externalApps <- listPlotModules('external')
  if (length(externalApps) > 0){
    externalApps <- gsub("mod_", "", )
    externalApps <- paste0(externalApps, '()')
  } else 
    externalApps <- NULL
  
  bothApps <- c(builtinApps, externalApps)
  
  value <- switch(location, 
                  both = bothApps,
                  external = externalApps,
                  builtin = builtinApps)
  
  value
}



#' @rdname DaparViz-modules
#' 
listPlotModules <- function(location = 'both'){
  
  stopifnot(location %in% c('both', 'external', 'builtin'))
  
  builtin <- ls("package:DaparViz")
  builtin <- builtin[grep("mod_ds_", builtin)]
  builtin <- gsub("_server", "", builtin)
  builtin <- gsub("_ui", "", builtin)
  builtin <- unique(builtin)
  
  # Lists module in the global environment
  external <- utils::lsf.str(envir = globalenv())
  external <- external[grep("mod_ds_", external)]
  external <- gsub("_server", "", external)
  external <- gsub("_ui", "", external)
  external <- unique(external)
  external <- setdiff(external, builtin)
  if (length(external) == 0)
    external <- NULL
  
  both <- c(builtin, external)
  
  value <- switch(location, 
                  both = both,
                  external = external,
                  builtin = builtin)
  
  value 
}

