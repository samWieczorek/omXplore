#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#' 
#' @description 
#' xxxx
#' 
#' @name heatmaps
#' 
#' @examples
#' library(QFeatures)
#' data(ft_na)
#' heatmapD(assay(ft_na, 1), colData(ft_na)$Condition)
#' mv.heatmap(assay(ft_na, 1))
#' 
#' #------------------------------------------
#' # Shiny module
#' #------------------------------------------
#' if(interactive()){
#' data(ft)
#'  ui <- mod_ds_heatmap_ui('plot')
#' 
#'  server <- function(input, output, session) {
#'   mod_ds_heatmap_server('plot', 
#'   reactive({assay(ft, 1)}),
#'   colData(ft)$Condition
#'   )}
#'  
#'  shinyApp(ui=ui, server=server)
#' }
#' 
#' 
#' 
#' 
NULL


#' @param id A `character(1)` which is the id of the shiny module.
#' 
#' @export
#' @importFrom shiny NS tagList
#' @rdname heatmaps
mod_ds_heatmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        selectInput(ns("distance"),"Distance",
                    choices = setNames(nm = c("euclidean",
                                              "manhattan",
                                              "maximum",
                                              "canberra",
                                              "binary",
                                              "minkowski")),
                    selected = "euclidean",
                    width="150px")
      ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        selectInput(ns("linkage"),"Linkage",
                    choices = setNames(nm = c("complete",
                                              "average",
                                              "ward.D",
                                              "ward.D2",
                                              "single",
                                              "centroid",
                                              "mcquitty",
                                              "median")),
                    selected = 'complete',
                    width = "150px")
      ),
      tags$hr(),
      uiOutput(ns("DS_PlotHeatmap"))
    )
  )
}



#' @param id xxx
#' @param data xxx
#' @param conds xxx
#' @param width xxx
#' @export
#' @rdname heatmaps
#'
mod_ds_heatmap_server <- function(id, 
                                  data,
                                  conds,
                                  width = 900){

  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(data())
      stopifnot (inherits(data(), "matrix"))
    })


    limitHeatmap <- 20000
    height <- paste0(2 * width / 3, "px")
    width <- paste0(width, "px")

    output$DS_PlotHeatmap <- renderUI({
      req(data())
      if (nrow(data()) > limitHeatmap){
        tags$p("The dataset is too big to compute the heatmap in a reasonable time.")
      }else {
        tagList(
          plotOutput(ns('heatmap_ui'), 
                     width = width, 
                     height = height
                     )
        )
      }
    })



    output$heatmap_ui <- renderPlot({
      req(input$linkage)
      req(input$distance)
      
      isolate({
        withProgress(message = 'Making plot', value = 100, {
          heatmapD(data = data(),
                   conds = conds,
                   distfun = input$distance,
                   hclustfun = input$linkage)
        })
      })
    })

  })

}

