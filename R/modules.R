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
addModules <- function(addons) {
  if (is.null(addons) || !inherits(addons, "list")) {
    warning("xxx")
    return(NULL)
  }

  f_assign <- function(fun, pkg, suffix) {
    f_original_name <- paste0(fun, "_", suffix)
    f_dest_name <- paste0("DaparViz_", fun, "_", suffix)
    f_fullname <- paste0(pkg, "::", f_original_name)
    #require(pkg, character.only = TRUE)
    if (f_original_name %in% ls(paste0("package:", pkg))) {
      assign(f_dest_name, eval(parse(text = f_fullname)), envir = globalenv())
    }
  }

  for (x in names(addons)) {
    for (m in addons[[x]]$funcs) {
      f_assign(m, x, "ui")
      f_assign(m, x, "server")
    }
    addResourcePath(
      prefix = paste0(x, "_imgDir"),
      directoryPath = addons[[x]]$imgDirPath
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
