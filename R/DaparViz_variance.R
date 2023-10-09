#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name plot-variance
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj xxx
#' @param pal.name A `character(1)` which is the name of the palette from the 
#' package `RColorBrewer` from which the colors are taken. 
#' Default value is 'Set1'.
#' 
#' @return NA
#'
#' @examples
#' if(interactive()){
#' data(vData_ft)
#' DaparViz_variance(vDataz_ft[[1]])
#' }
#'
NULL


#' @importFrom shiny NS tagList
#' @rdname plot-variance
#' @export
DaparViz_variance_ui <- function(id) {
    ns <- NS(id)
    tagList(
      shinyjs::useShinyjs(),
      shinyjs::hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
      uiOutput(ns('helpTxt')),
      highcharter::highchartOutput(ns("viewDistCV"), width = 600, height = 600)
    )
}



#'
#' @importFrom shiny NS tagList
#' @rdname plot-variance
#' @export
DaparViz_variance_server <- function(id,
                                   obj,
                                   pal.name = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        rv <- reactiveValues(data = NULL)
        
        observe({
          if(inherits(obj(), "DaparViz"))
            rv$data <- obj()
          
          shinyjs::toggle('badFormatMsg', condition = is.null(rv$data))
        }, priority = 1000)

        output$viewDistCV <- renderHighchart({
          req(rv$data)
          withProgress(message = "Making plot", value = 100, {
                varDist <- CVDist(rv$data, pal.name)
            })
        })
        
        output$helpTxt <- renderUI({
          req(rv$data)
          tagList(
          helpText("Display the condition-wise distributions of the log-intensity 
        CV (Coefficient of Variation) of the protein/peptides."),
          helpText("For better visualization, it is possible to zoom in by
            click-and-drag.")
          )
        })
    })
}


#' @import shiny
#' @rdname plot-variance
#' @export
#' 
DaparViz_variance <- function(obj){
  ui <- fluidPage(DaparViz_variance_ui("plot"))

  server <- sfunction(input, output, session) 
    DaparViz_variance_server("plot", obj = reactive({obj}))
  
  shinyApp(ui, server)
  
}
