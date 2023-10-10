.onLoad <- function(libname, pkgname) {
  requireNamespace(c('shiny', 'MSnbase', 'QFeatures'))

  shiny::addResourcePath(
        prefix = "images",
        directoryPath = system.file("images", package = "DaparViz")
    )
}

.onUnload <- function(libname, pkgname) {
    shiny::removeResourcePath("images")
}
