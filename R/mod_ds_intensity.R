#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#' 
#' @description 
#' xxxx
#' 
#' @name intensity-plots
#' 
#' @examples
#' data(ft)
#' conds <- colData(ft)$Condition
#' violinPlot(assay(ft, 1), conds)
#' violinPlot(assay(ft, 1), conds, subset = c(2,4))
#' 
#' boxPlot(assay(ft, 1), conds)
#' boxPlot(assay(ft, 1), conds, subset = c(2,4))
#' 
#' 
#' #------------------------------------------
#' # Shiny module
#' #------------------------------------------
#' if(interactive()){
#' data(ft)
#'  ui <- mod_ds_intensity_ui('iplot')
#' 
#'  server <- function(input, output, session) {
#'   mod_ds_intensity_server('iplot', 
#'                        reactive({ft[[1]]}),
#'                        colData(ft)$Condition
#'                        )}
#'  
#'  shinyApp(ui=ui, server=server)
#' }
#' 
#' 
#' 
#' 
NULL


#' @param id xxx
#' 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom stats setNames
#'
#' @return NA
#' @export
#' @rdname intensity-plots
mod_ds_intensity_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               uiOutput(ns("box_ui")),
               uiOutput(ns("violin_ui"))
      ),
      tags$div(style="display:inline-block; vertical-align: middle;",
               selectInput(ns("choosePlot"), 
                           "Choose plot",
                           choices = setNames(nm = c("violin", "box")),
                           width = '100px'),
               mod_seTracker_ui(ns('tracker'))
      )
    )
  )
}



#' @param id xxx
#' @param se xxx
#' @param conds xxx
#' @param ... xxx
#' 
#' @rdname intensity-plots
#'
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom shinyjs toggle hidden
#'
#'@return NA
#'
mod_ds_intensity_server <- function(id,
                                 se,
                                 conds,
                                 ...){
  if (! requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Please install SummarizedExperiment: BiocManager::install('SummarizedExperiment')")
  }
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    indices <- reactiveVal()

    indices <- mod_seTracker_server("tracker",
                                    se = reactive({se()})
                                    )

    output$box_ui <- renderUI({
      if (input$choosePlot == 'box')
        highchartOutput(ns('box'))
      else
        hidden(highchartOutput(ns('box')))
    })
    
    output$box <- renderHighchart({
       withProgress(message = 'Making plot', value = 100, {
        tmp <- boxPlot(data = SummarizedExperiment::assay(se()),
                        conds = conds,
                        subset = indices(),
                        ...)
      })

    })


    output$violin_ui <- renderUI({
      if (input$choosePlot == 'violin')
        imageOutput(ns('violin'))
      else
        hidden(imageOutput(ns('violin')))
    })
    
    output$violin <- renderImage({
      
      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext='.png')
      # Generate a png
      withProgress(message = 'Making plot', value = 100, {
        png(outfile)
        pattern <- paste0('test',".violinplot")
        tmp <- violinPlot(SummarizedExperiment::assay(se()),
                           conds,
                           subset = indices(),
                           ...)
        #future(createPNGFromWidget(tmp,pattern))
        dev.off()
      })
      tmp
      
      # Return a list
      list(src = outfile,
           alt = "This is alternate text")
    }, deleteFile = TRUE)


  })


}

## To be copied in the UI
# mod_plots_intensity_plots_ui("plots_intensity_plots_ui_1")

## To be copied in the server
# callModule(mod_plots_intensity_plots_server, "plots_intensity_plots_ui_1")

