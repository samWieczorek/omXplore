#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param vizData xxx
#' @param img xxx
#'
#' 
#' @return NA
#'
#' @example
#' NULL
#'
NULL


#' @export
#' @importFrom shiny NS tagList
#' 
mod_ds_plot_extern_ui <- function(id) {
    ns <- NS(id)
    tagList(
        plotOutput(ns("plot"), width = "600px", height = "500px")
    )
}

#' @export
#'
mod_ds_plot_extern_server <- function(id,
                                     vizData = reactive({NULL}),
                                     img = NULL) {
  
  
 
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        addResourcePath(prefix = "img_ds_plot_extern", 
                        directoryPath = "C:\\Users\\sw175264\\Documents\\")
        
        
        observe({
           req(vizData())
            stopifnot(inherits(vizData(), 'VizData'))
        })


        output$plot <- renderPlot({
            req(vizData())
            plot(1:10)
        })
    })
}
