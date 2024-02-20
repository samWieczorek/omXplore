#' @title Modules functions
#'
#' @description
#' xxxxx
#' * `listShinyApps()`: xxx
#' * `listPlotModules()`: xxx
#' * `addModules()`: Add shiny module
#'
#' @param location xxx
#' @param addons A `list` in which each item is:
#' * is named by the name of a package
#' * contains th set of modules to integrate
#'
#'
#' @name DaparViz-modules
#'
#' @examples
#' listShinyApps()
#' listPlotModules()
#'
NULL


#' @export
#' @rdname DaparViz-modules
#' @return NA
#' 
#' @examples
#' addons <- list(DaparViz = list(funcs = c("extFoo1"), imgDirPath = ''))
#' addModules(addons)
#'
addModules <- function(addons) {
  
  addon.isValid <- function(addons) {
    passed <- TRUE
    passed <- passed && inherits(addons, "list")
    
    for (x in addons){
      passed <- passed && inherits(x, "list")
      passed <- passed && "funcs" %in% names(x)
      passed <- passed && "imgDirPath" %in% names(x)
    }

    passed
  }
  
  
  
  if (is.null(addons) || !inherits(addons, "list")) {
    cat('No external module was found')
    return(NULL)
  } else if (!addon.isValid(addons)){
    warning('addons is not in a correct format')
    return(NULL)
  }

  f_assign <- function(fun, pkg, suffix) {
    f_original_name <- paste0(fun, "_", suffix)
    f_dest_name <- paste0("DaparViz_", fun, "_", suffix)
    f_fullname <- paste0(pkg, "::", f_original_name)
    
    if (f_original_name %in% ls(paste0("package:", pkg), envir = globalenv())) {
      assign(f_dest_name, eval(parse(text = f_fullname)), envir = globalenv())
    }
  }


  for (x in names(addons)) {
    for (m in addons[[x]]$funcs) {
      f_assign(m, x, "ui")
      f_assign(m, x, "server")
    }
    
    
    tryCatch({
        addResourcePath(
        prefix = paste0(x, "_imgDir"),
        directoryPath = addons[[x]]$imgDirPath
        )
      },
      warning = function(w) print(w),
      error = function(e) print(e)
      )
  }
}



#' @export
#' @rdname DaparViz-modules
#' @return A vector
#'
listShinyApps <- function(location = "both") {
  stopifnot(location %in% c("both", "external", "builtin"))

  builtinApps <- gsub("", "", listPlotModules("builtin"))
  builtinApps <- paste0(builtinApps, "()")

  externalApps <- listPlotModules("external")
  if (length(externalApps) > 0) {
    externalApps <- gsub("", "", )
    externalApps <- paste0(externalApps, "()")
  } else {
    externalApps <- NULL
  }

  bothApps <- c(builtinApps, externalApps)

  value <- switch(location,
    both = bothApps,
    external = externalApps,
    builtin = builtinApps
  )

  value
}


#' @export
#' @rdname DaparViz-modules
#' @return A vector
#'
listPlotModules <- function(location = "both") {
  stopifnot(location %in% c("both", "external", "builtin"))

  builtin <- ls("package:DaparViz")
  builtin <- builtin[grep("DaparViz_", builtin)]
  builtin <- gsub("_server", "", builtin)
  builtin <- gsub("_ui", "", builtin)
  builtin <- unique(builtin)

  # Lists module in the global environment
  external <- utils::lsf.str(envir = globalenv())
  external <- external[grep("DaparViz_", external)]
  external <- gsub("_server", "", external)
  external <- gsub("_ui", "", external)
  external <- unique(external)
  external <- setdiff(external, builtin)
  if (length(external) == 0) {
    external <- NULL
  }

  both <- c(builtin, external)

  value <- switch(location,
    both = both,
    external = external,
    builtin = builtin
  )

  value
}
