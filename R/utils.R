#' @title Example dataset
#' @description This function builds an example dataset as an instance of the class
#' 'VizClass', which is the format to be used with functions in the DaparViz package.
#' 
#' @param type A 'character(1)' which is the type of original dataset to build an example from. 
#' Available values are 'QFeatures' (default value), 'MSnbase' and 'list'.
#' 
#' @return An instance of the class 'VizClass'.
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#' 
#' @import utils
#' 
BuildExampleDataset <- function(type='QFeatures'){
require(utils)
  
  dataset <- switch(type,
                    
         QFeatures = {
           data(Exp1_R25_pept, package = 'DaparToolshedData', envir = environment())
           convert2viz(Exp1_R25_pept)
           },
         
         MSnbase = {
           data(Exp1_R25_pept, package = 'DAPARdata', envir = environment())
           data(Exp1_R25_prot, package = 'DAPARdata', envir = environment())
           data(Exp1_R2_pept, package = 'DAPARdata', envir = environment())
           ll.tmp <- setNames(c(Exp1_R25_pept, Exp1_R25_prot, Exp1_R2_pept),
                              nm = c('Exp1_R25_pept', 'Exp1_R25_prot', 'Exp1_R2_pept'))
           
           convert2viz(ll.tmp)
         },

         list = {
           pkgs.require('Msnbase')
           data(Exp1_R25_pept, package = 'DAPARdata', envir = environment())
           msnset <- Exp1_R25_pept
           X <- PSMatch::makeAdjacencyMatrix(MSnbase::fData(msnset)[, msnset@experimentData@other$proteinId])
           rownames(X) <- rownames(MSnbase::fData(msnset))
           connectedComp <- PSMatch::ConnectedComponents(X)
           
           ll.Exp1_R25_pept <- list(
             qdata = MSnbase::exprs(msnset),
             metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
             mdata = MSnbase::fData(msnset),
             colID = msnset@experimentData@other$keyId,
             conds = MSnbase::pData(msnset)[, 'Condition'],
             type = msnset@experimentData@other$typeOfData,
             adjMat = as.matrix(X),
             cc = as.list(connectedComp@adjMatrices)
           )
           
           data(Exp1_R25_prot, package='DAPARdata', envir = environment())
           msnset <- Exp1_R25_prot
           ll.Exp1_R25_prot <- list(
             qdata = MSnbase::exprs(msnset),
             metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
             mdata = MSnbase::fData(msnset),
             colID = msnset@experimentData@other$keyId,
             conds = MSnbase::pData(msnset)[, 'Condition'],
             type = msnset@experimentData@other$typeOfData
           )
           
           data(Exp1_R2_pept, package='DAPARdata', envir = environment())
           msnset <- Exp1_R2_pept
           X <- PSMatch::makeAdjacencyMatrix(MSnbase::fData(msnset)[, msnset@experimentData@other$proteinId])
           rownames(X) <- rownames(fData(msnset))
           connectedComp <- PSMatch::ConnectedComponents(X)
           
           ll.Exp1_R2_pept <- list(
             qdata = MSnbase::exprs(msnset),
             metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
             mdata = MSnbase::fData(msnset),
             colID = msnset@experimentData@other$keyId,
             conds = MSnbase::pData(msnset)[, 'Condition'],
             type = msnset@experimentData@other$typeOfData,
             adjMat = as.matrix(X),
             cc = as.list(connectedComp@adjMatrices)
             
           )
           
           
           
           ll.tmp <- list(Exp1_R25_pept = ll.Exp1_R25_pept,
                          Exp1_R25_prot = ll.Exp1_R25_prot,
                          Exp1_R2_pept = ll.Exp1_R2_pept
                          )
           
           convert2viz(ll.tmp)
         },
         default = NULL
         )
  
  return(dataset)
  
}

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
#' @importFrom highcharter hc_exporting
#'
customExportMenu <- function(hc, fname) {
  require(highcharter)
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
#' @importFrom highcharter hc_chart
#'
customChart <- function(hc,
                        chartType = 'scatter',
                        zoomType = "None",
                        width = 0,
                        height = 0) {
  require(highcharter)
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
#' @param vizData An instance of the class 'VizData'
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
#' @param vizData An instance of the class 'VizData'
#'
#' @export
BuildColorStyles <- function(vizData) {
  styles <- list(tags = NULL,
                 colors = NULL)
  
  mc <- metacell.def(vizData@type)
  
  styles$tags <- mc$node
  styles$colors <- mc$color
  styles
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