#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#' 
#' @description 
#' xxxx
#' 
#' @name plot-mv
#' 
#' @examples
#' library(QFeatures)
#' data(ft_na)
#' mvPerLinesHisto(assay(ft_na, 1))
#' 
#' mvPerLinesHistoPerCondition(assay(ft_na, 1),
#'                             colData(ft_na)$Condition)
#' mvHisto(assay(ft_na, 1), colData(ft_na)$Condition)
#' 
#' #------------------------------------------
#' # Shiny module
#' #------------------------------------------
#' if(interactive()){
#'  data(ft_na)
#'  ui <- mod_ds_mv_ui('plot')
#' 
#'  server <- function(input, output, session) {
#'   mod_ds_mv_server('plot', 
#'                 reactive({assay(ft_na, 1)}),
#'                 colData(ft_na)$Condition)
#'                }
#'  
#'  shinyApp(ui=ui, server=server)
#' }
NULL


#' @param id xxx
#' @export
#' @importFrom shiny NS tagList
#' @importFrom highcharter highchartOutput
#' @rdname plot-mv
mod_ds_mv_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    tagList(
      fluidRow(
        column(width = 4, 
               highchartOutput(ns("histo_MV")), 
               height="600px"),
        column(width = 4, 
               highchartOutput(ns("histo_MV_per_lines"))),
        column(width = 4, 
               highchartOutput(ns("histo_MV_per_lines_per_conditions")))
        )
      )
    )
}

#' @param id xxx
#' @param data An instance of the class [matrix]
#' @param conds A `character()`
#' @param pal.name xxx
#'
#' @export
#' @importFrom highcharter renderHighchart
#' 
#' @rdname plot-mv
#'
mod_ds_mv_server <- function(id,
                          data,
                          conds,
                          pal.name = reactive({NULL})){

  moduleServer(id, function(input, output, session){
    ns <- session$ns


    observe({
      req(data())
      stopifnot (inherits(data(), "matrix"))
    })



    output$histo_MV <- renderHighchart({
      req(data())
      pal.name()

      withProgress(message = 'Making plot', value = 100, {
        tmp <- mvHisto(data(),
                       conds = conds,
                       pal.name = pal.name())
        })
      tmp
    })



    output$histo_MV_per_lines <- renderHighchart({
      req(data())
      
      isolate({
        withProgress(message = 'Making plot', value = 100, {
          tmp <- mvPerLinesHisto(data())
        })
      })
      tmp
    })




    output$histo_MV_per_lines_per_conditions <- renderHighchart({
      req(data())
      pal.name()

      withProgress(message = 'Making plot', value = 100, {
        tmp <- mvPerLinesHistoPerCondition(data = data(),
                                           conds = conds,
                                           pal.name = pal.name())
      })
      tmp
    })

  })


}
