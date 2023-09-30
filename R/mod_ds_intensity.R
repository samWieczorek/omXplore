#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param vizData A instance of the class `VizData`
#' @param track.indices xxx
#' 
#' @name intensity-plots
#'
#' @example inst/extadata/examples/ex_mod_ds_intensity.R
#'
NULL


#' @import shiny
#' @import shinyjs
#' @importFrom stats setNames
#' @export
#' @rdname intensity-plots
mod_ds_intensity_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        shinyjs::hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
        hidden(radioButtons(ns("choosePlot"), "",
                    choices = setNames(nm = c("violin", "box")))),
        highchartOutput(ns("box")),
        shinyjs::hidden(imageOutput(ns("violin")))
        )
}



#' @rdname intensity-plots
#'
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom shinyjs toggle hidden
#' @import highcharter
#'
#' @return NA
#'
mod_ds_intensity_server <- function(id, 
                                    vizData, 
                                    track.indices = reactive({NULL})
                                    ) {
    
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(data = NULL)
        
        observe({
          if(inherits(vizData(), "VizData"))
            rv$data <- vizData()
          
          shinyjs::toggle('badFormatMsg', condition = is.null(rv$data))
          shinyjs::toggle('choosePlot', condition = !is.null(rv$data))
        }, priority = 1000)
        
        
        observeEvent(input$choosePlot, {
          req(rv$data)
          shinyjs::toggle('violin', condition = input$choosePlot == "violin")
          shinyjs::toggle('box', condition = input$choosePlot == "box")
        })
        
        output$box <- renderHighchart({
          req(rv$data)
            #withProgress(message = "Making plot", value = 100, {
                boxPlot(data = rv$data@qdata, 
                        conds = rv$data@conds, 
                        subset = track.indices())
                
           # })
          
        })

        output$violin <- renderImage({
          req(rv$data)
          # A temp file to save the output. It will be deleted after 
          # renderImage sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = ".png")
          # Generate a png
          withProgress(message = "Making plot", value = 100, {
              png(outfile)
              pattern <- paste0("test", ".violinplot")
              tmp <- violinPlot(data = as.matrix(rv$data@qdata), 
                                conds = rv$data@conds, 
                                subset = track.indices())
              # future(createPNGFromWidget(tmp,pattern))
              dev.off()
          })
          tmp
          # Return a list
          list(src = outfile,
               alt = "This is alternate text")
        },
            deleteFile = TRUE
        )
    })
}
