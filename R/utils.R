#' @title xxx
#' @description xxx
#' @export
BuildExampleDataset <- function(type){
  
  return(
    switch(type,
         QFeatures = {
           data(Exp1_R25_pept, package='DaparToolshedData')
           convert2viz(Exp1_R25_pept)
           },
         MSnbase = {
           data(Exp1_R25_prot, package='DAPARdata')
           data(Exp1_R25_pept, package='DAPARdata')
           data(Exp1_R2_pept, package='DAPARdata')
           ll.tmp <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
                              nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))
           
           convert2viz(ll.tmp)
         },
         list = {
           data(Exp1_R25_pept, package='DAPARdata')
           msnset <- Exp1_R25_pept
           ll.Exp1_R25_pept <- list(
             qdata = exprs(msnset),
             metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
             mdata = fData(msnset),
             colID = msnset@experimentData@other$keyId,
             conds = pData(msnset)[, 'Condition'],
             type = msnset@experimentData@other$typeOfData
           )
           
           data(Exp1_R25_prot, package='DAPARdata')
           msnset <- Exp1_R25_prot
           ll.Exp1_R25_prot <- list(
             qdata = exprs(msnset),
             metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
             mdata = fData(msnset),
             colID = msnset@experimentData@other$keyId,
             conds = pData(msnset)[, 'Condition'],
             type = msnset@experimentData@other$typeOfData
           )
           
           data(Exp1_R2_pept, package='DAPARdata')
           msnset <- Exp1_R2_pept
           ll.Exp1_R2_pept <- list(
             qdata = exprs(msnset),
             metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
             mdata = fData(msnset),
             colID = msnset@experimentData@other$keyId,
             conds = pData(msnset)[, 'Condition'],
             type = msnset@experimentData@other$typeOfData
           )
           
           
           ll.tmp <- list(
             Exp1_R25_pept = ll.Exp1_R25_pept,
             Exp1_R25_prot = ll.Exp1_R25_prot,
             Exp1_R2_pept = ll.Exp1_R2_pept
           )
           
           convert2viz(ll.tmp)
         },
         default = NULL
         )
  )
}

#' @title Customised contextual menu of highcharts plots
#'
#' @param hc A highcharter object
#'
#' @param fname The filename under which the plot has to be saved
#'
#' @return A contextual menu for highcharts plots
#'
#' @author Samuel Wieczorek
#'
#' @rdname customExportMenu_HC
#'
#' @examples
#' hc <- highchart() %>%
#'     hc_chart(type = "line") %>%
#'     hc_add_series(data = c(29, 71, 40))
#' customExportMenu(hc, fname = "foo")
#' hc
#'
#' @export
#'
#' @importFrom highcharter hc_exporting
#'
customExportMenu <- function(hc, fname) {
    highcharter::hc_exporting(hc,
        enabled = TRUE,
        filename = fname,
        buttons = list(
            contextButton = list(
                menuItems = list(
                    "downloadPNG",
                    "downloadSVG",
                    "downloadPDF"
                )
            )
        )
    )
    hc
}




#' @title Customised resetZoom Button of highcharts plots
#'
#' @param hc A highcharter object
#'
#' @param chartType The type of the plot
#'
#' @param zoomType The type of the zoom (one of "x", "y", "xy", "None")
#'
#' @param width xxx
#'
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
#' customChart(hc, filename = "foo")
#'
#' @export
#'
#' @importFrom highcharter hc_chart
#'
customChart <- function(hc,
    chartType,
    zoomType = "None",
    width = 0,
    height = 0) {
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




#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param vizData xx
#' @param digits xxx
#'
#' @export
#'
getDataForExprs <- function(vizData, digits = NULL) {
  if (is.null(digits))  
    digits <- 2
  
  test.table <- as.data.frame(round(vizData@qdata))
  if (!is.null(names(vizData@metacell))) { # agregated dataset
    test.table <- cbind(round(vizData@qdata, digits = digits), vizData@metacell)
  } else {
    test.table <- cbind(
      test.table,
      as.data.frame(
        matrix(rep(NA, ncol(test.table) * nrow(test.table)), nrow = nrow(test.table))
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
#' @param obj xx
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