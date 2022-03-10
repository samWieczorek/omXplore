#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#' 
#' @description 
#' xxxx
#' 
#' @name plot-variance
#' 
#' @examples
#' library(QFeatures)
#' data(ft)
#' CVDist(assay(ft, 1), colData(ft)$Condition)
#' 
#' 
#' #------------------------------------------
#' # Shiny module
#' #------------------------------------------
#' if(interactive()){
#' data(ft)
#'  ui <- mod_ds_variance_ui('plot')
#' 
#'  server <- function(input, output, session) {
#'   mod_ds_variance_server('plot', 
#'   reactive({assay(ft, 1)})
#'   )}
#'  
#'  shinyApp(ui=ui, server=server)
#' }
#' 
#' 
#' 
#' 
NULL


#' @param id shiny id
#' @importFrom shiny NS tagList
#' @rdname plot-variance
#' @export
mod_ds_variance_ui <- function(id){
  ns <- NS(id)
  tagList(
    helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation)
               of the protein/peptides."),
    helpText("For better visualization, it is possible to zoom in by click-and-drag."),
    highchartOutput(ns("viewDistCV"),width = 600, height = 600)
  )
}



#' @param id shiny id
#' @param data xxx
#' @param conds A `character()` representings the condition for 
#' each sample of the [QFeatures] object.
#' @param pal.name xxx
#' 
#' @importFrom shiny NS tagList
#' @rdname plot-variance
#' @export
mod_ds_variance_server <- function(id,
                                   data,
                                   conds,
                                   pal.name = NULL){


  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      req(data())
      stopifnot(inherits(data(), "matrix"))
    })


    output$viewDistCV <- renderHighchart({
      withProgress(message = 'Making plot', value = 100, {
        varDist <- CVDist(data(), 
                          conds, 
                          pal.name)
      })
    })

  })


}
