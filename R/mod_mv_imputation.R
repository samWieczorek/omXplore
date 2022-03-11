#' @title Distribution of Observed values with respect to intensity values
#' 
#' @description 
#' 
#' This method shows density plots which represents the repartition of
#' Partial Observed Values for each replicate in the dataset.
#' The colors correspond to the different conditions (Condition in colData of
#' the object of class `QFeatures`).
#' The x-axis represent the mean of intensity for one condition and one
#' entity in the dataset (i. e. a protein) 
#' whereas the y-axis count the number of observed values for this entity
#' and the considered condition.
#' 
#' @section xxx:
#' 
#' xxxx
#' 
#' @section missing value image:
#' Plots a heatmap of the quantitative data. Each column represent one of
#' the conditions in the object of class \code{MSnSet} and 
#' the color is proportional to the mean of intensity for each line of
#' the dataset.
#' The lines have been sorted in order to visualize easily the different
#' number of missing values. A white square is plotted for missing values.
#' 
#' @seealso The package `DaparToolshed` for the 'qMetadata' metadata.
#' 
#' @name plot-mv
#' @return A plot
#' 
#' @author Samuel Wieczorek, Enora Fremy
#' 
#' @examples
#' data(ft_na)
#' data <- assay(ft_na, 1)
#' conds <- colData(ft_na)$Condition
#' 
#' #-----------------------------
#' # xxx
#' #-----------------------------
#' 
#' mv.density(ft_na[[1]], conds, pattern = 'missing MEC')
#' 
#' mv.mec.heatmap(ft_na[[1]], conds)
#' 
#' #-----------------------------
#' # xxx
#' #-----------------------------
#' 
#' if(interactive()){
#'  data(ft_na)
#'  
#'  ui <- mod_mv_imputation_ui('plot')
#' 
#'  server <- function(input, output, session) {
#'   mod_mv_imputation_server('plot',
#'                            se = reactive({ft_na[[1]]}),
#'                            conds = colData(ft_na)$Condition)
#'   }
#'  
#'  shinyApp(ui=ui, server=server)
#' }
NULL


#' @param id shiny id
#' @rdname plot-mv
#' @export
#' @importFrom shiny NS tagList plotOutput
#' @importFrom highcharter highchartOutput
#' 
mod_mv_imputation_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; 
                       padding-right: 20px;",
                highchartOutput(ns("nabyMean_ui"), width='600px')),
      tags$div( style="display:inline-block; vertical-align: top; 
                       padding-right: 20px;",
                plotOutput(ns("imageNA_ui"), width='600px'))
    )
  )
}

#' @param id A `character(1)` which is the 'id' of the shiny module.
#' @param se An instance of a class `SummarizedExperiment`.
#' @param conds A `character()` of the name of conditions (one condition per sample).
#' @param pal.name  A `character(1)` which is the name of the palette (from
#' the package [RColorBrewer] to use.
#' 
#' @rdname plot-mv
#' @export
#' @importFrom highcharter renderHighchart
#' 
mod_mv_imputation_server <- function(id,
                                     se = reactive({NULL}),
                                     conds = NULL,
                                     pal.name = reactive({NULL})
                                     ){
  if (! requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Please install SummarizedExperiment: BiocManager::install('SummarizedExperiment')")
  }
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialisation of the module
    observe({
      req(se())
      stopifnot(inherits(se(), "SummarizedExperiment"))
    })
    
    
  output$nabyMean_ui <- renderHighchart({
      req(se())

      withProgress(message = 'Making MV Intensity plot', value = 100, {
        mv.density(se = SummarizedExperiment::assay(se()),
                   conds = conds,
                   pal.name = pal.name(),
                   pattern = 'missing POV')
        })
    })


    output$imageNA_ui <- renderPlot({
      req(se())
      withProgress(message = 'Making MV Heatmap plot', value = 100, {
        mv.mec.heatmap(se = se(), 
                       conds = conds)
        })

    })
  })
}

