

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
#' hc_chart(type = "line") %>%
#' hc_add_series(data = c(29, 71, 40)) 
#' customExportMenu(hc, fname='foo')
#' hc
#' 
#' @export
#' 
#' @importFrom highcharter hc_exporting
#' 
customExportMenu <- function(hc, fname){
  highcharter::hc_exporting(hc, 
                            enabled = TRUE,
                            filename = fname,
                            buttons= list(
                              contextButton = list(
                                menuItems = list('downloadPNG', 
                                                'downloadSVG',
                                                'downloadPDF')
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
#' hc_chart(hc,type = "line") 
#' hc_add_series(hc,data = c(29, 71, 40))
#' customChart(hc,filename='foo')
#' 
#' @export
#' 
#' @importFrom highcharter hc_chart
#' 
customChart <- function(hc,
                        chartType, 
                        zoomType="None", 
                        width = 0, 
                        height = 0){
  hc %>% 
    hc_chart(type = chartType, 
             zoomType=zoomType,
             showAxes = TRUE,
             width = width,
             height = height,
             resetZoomButton= list(
               position = list(
                 align= 'left',
                 verticalAlign = 'top')
             ))
}



#' @noRd
#' @export
.initComplete <- function(){
  
  return (DT::JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
    "}"))
}