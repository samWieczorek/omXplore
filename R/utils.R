
#' @title Customised contextual menu of highcharts plots
#'
#' @param hc A highcharter object
#' @param fname The filename under which the plot has to be saved
#' 
#' @return A contextual menu for highcharts plots
#'
#' @author Samuel Wieczorek
#'
#' @rdname customExportMenu_HC
#'
#' @examples
#' library(highcharter)
#' hc <- highchart() %>%
#'     hc_chart(type = "line") %>%
#'     hc_add_series(data = c(29, 71, 40))
#' hc <- customExportMenu(hc, fname = "foo")
#' hc
#'
#' @export
#'
customExportMenu <- function(hc, fname) {
  pkgs.require('highcharter')
    highcharter::hc_exporting(hc,
                              enabled = TRUE,
                              filename = fname,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG",
                                                   "downloadSVG",
                                                   "downloadPDF")
                                  )
                                )
                              )
  hc
}




#' @title Customised resetZoom Button of highcharts plots
#'
#' @param hc A highcharter object
#' @param chartType The type of the plot
#' @param zoomType The type of the zoom (one of "x", "y", "xy", "None")
#' @param width xxx
#' @param height xxx
#'
#' @return A highchart plot
#'
#' @author Samuel Wieczorek
#'
#' @examples
#' library("highcharter")
#' hc <- highchart()
#' hc_chart(hc, type = "line")
#' hc_add_series(hc, data = c(29, 71, 40))
#' customChart(hc)
#'
#' @export
#'
customChart <- function(hc,
                        chartType = 'scatter',
                        zoomType = "None",
                        width = 0,
                        height = 0) {
  pkgs.require('highcharter')
    hc %>%
        hc_chart(
            type = chartType,
            zoomType = zoomType,
            showAxes = TRUE,
            width = width,
            height = height,
            resetZoomButton = list(
                position = list(
                    align = "left",
                    verticalAlign = "top"
                )
            )
        )
}



#' @noRd
#' @export
.initComplete <- function() {
    return(DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({
        'background-color': 'darkgrey', 
        'color': 'black'});",
        "}"
    ))
}




#' @title Constructs a dataset suitable to use with the module format_DT.
#'
#' @description
#' This function builds the skeleton of a dataset which can be used by the module
#' format_DT. It creates additional columns to be used to style the table.
#' to colors cells.
#'
#' @param vizData An instance of the class `DaparViz`
#' @param digits An 'integer(1)' to specify the number of digits to display 
#' in the tables for numerical values. Default is 2.
#' 
#' @return A data.frame
#'
#' @export
#'
FormatDataForDT <- function(vizData, digits = 2) {

  test.table <- as.data.frame(round(vizData@qdata))
  if (!is.null(names(vizData@metacell))) {
    test.table <- cbind(round(vizData@qdata, digits = digits), vizData@metacell)
  } else {
    test.table <- cbind(
      test.table,
      as.data.frame(
        matrix(rep(NA, ncol(test.table) * nrow(test.table)),
               nrow = nrow(test.table))
      )
    )
  }
  return(test.table)
}





#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param vizData An instance of the class `DaparViz`
#'
#' @export
BuildColorStyles <- function(type) {
  mc <- metacell.def(type)
  colors <- as.list(setNames(mc$color, mc$node))
  colors
}



#' @title Loads packages
#' 
#' @description Checks if a package is available to load it
#' 
#' @param ll.deps A `character()` vector which contains packages names
#' 
#' @examples 
#' pkgs.require('DAPAR')
#' 
#' @export
#' 
#' @author Samuel Wieczorek
#' 
pkgs.require <- function(ll.deps){
  lapply(ll.deps, function(x) {
    if (!requireNamespace(x, quietly = TRUE)) {
      stop(paste0("Please install ", x, ": BiocManager::install('", x, "')"))
    }
  })
}


#' @title Add resource path for image
#' 
#' @description This line allows to extend the resource path to a different directory
#' than the one in 'inst/' which is given by default when loading a package.
#' 
#' @param prefix A `character()` vector which contains packages names
#' @param path xxx
#' 
#' @examples 
#' \donttest{
#' addImgPath('mod_foo', system.file('.', package='DaparViz'))
#' }
#' 
#' @export
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
addModules <- function(addons){
  
  stopifnot(inherits(addons, 'list'))
  
  
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


#' @title xxx
#' @description Lists built-in module in the package `DaparViz`
#' @importFrom utils lsf.str
#' @export
listPlotModules <- function() {
  
  ll.daparViz <- ls("package:DaparViz")
  ll.daparViz <- ll.daparViz[grep("mod_ds_", ll.daparViz)]
  ll.daparViz <- gsub("_server", "", ll.daparViz)
  ll.daparViz <- gsub("_ui", "", ll.daparViz)
  ll.daparViz <- unique(ll.daparViz)
  ll.daparViz
  
  # Lists module in the global environment
  ll.env <- utils::lsf.str(envir = globalenv())
  ll.env <- ll.env[grep("mod_ds_", ll.env)]
  ll.env <- gsub("_server", "", ll.env)
  ll.env <- gsub("_ui", "", ll.env)
  ll.env <- unique(ll.env)
  
  c(ll.daparViz, ll.env)
}

#' @title xxx
#' @description xxxx
#' @param cc xxx
#' @return A `list` of three items:
#' * `One_One`: the number of cc composed of one protein and one peptide
#' * `One_Multi`: the number of cc composed of one protein and several peptides
#' * `Multi_Multi`: the number of cc composed of several proteins and 
#' several (shared) peptides.
#' @export
GetCCInfos <- function(cc){
  cc.infos <- list(One_One = 0,
                   One_Multi = 0,
                   Multi_Multi = 0)
  
  for (x in cc){
    if (dim(x)[1] == 1 && dim(x)[2] == 1)
      cc.infos[['One_One']] <- 1 + cc.infos[['One_One']]
    else if (dim(x)[1] > 1 && dim(x)[2] == 1)
      cc.infos[['One_Multi']] <- 1 + cc.infos[['One_Multi']]
    else if (dim(x)[1] > 1 && dim(x)[2] > 1)
      cc.infos[['Multi_Multi']] <- 1 + cc.infos[['Multi_Multi']]
  }
  
  cc.infos
}
