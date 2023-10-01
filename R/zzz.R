.onLoad <- function(libname, pkgname) {
    #library(shiny)
  
  shiny::addResourcePath(
        prefix = "images",
        directoryPath = system.file("images", package = "DaparViz")
    )
}

.onUnload <- function(libname, pkgname) {
    shiny::removeResourcePath("images")
}
