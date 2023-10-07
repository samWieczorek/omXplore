#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param id A `character(1)` which is the id of the shiny module.
#' @param DaparViz xxx
#' @param width xxx
#'
#' @name ds_heatmap
#' 
#' @return NA
#'
#' @examples
#' data(vData_ft)
#' ds_heatmaps(vData_ft[[1]])
#'
NULL



#' @importFrom shiny NS tagList
#' @rdname ds_heatmap
mod_ds_heatmap_ui <- function(id) {
    ns <- NS(id)
    tagList(
      useShinyjs(),
      hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
      hidden(div( style = "display:inline-block; vertical-align: middle; padding-right: 20px;",
                selectInput(ns("distance"), "Distance",
                    choices = setNames(nm = c("euclidean", "manhattan")),
                    selected = "euclidean",
                    width = "150px")
            )),
      hidden(div(style = "display:inline-block; vertical-align: middle; padding-right: 20px;",
                selectInput(ns("linkage"), "Linkage",
                    choices = setNames(nm = c("complete", "ward.D", "average")),
                    selected = "complete",
                    width = "150px")
            )),
            tags$hr(),
            uiOutput(ns("DS_PlotHeatmap"))
        )
}




#' @rdname ds_heatmap
#'
mod_ds_heatmap_server <- function(id,
                                  obj = reactive({NULL}),
                                  width = 900) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(data = NULL)
        
        observe({
          if(inherits(obj(), "DaparViz"))
            rv$data <- obj()
          
          shinyjs::toggle('badFormatMsg', condition = !inherits(obj(), "DaparViz"))
          shinyjs::toggle('linkage', condition = !inherits(obj(), "DaparViz"))
          shinyjs::toggle('distance', condition = !inherits(obj(), "DaparViz"))
        }, priority = 1000)


        limitHeatmap <- 20000
        height <- paste0(2 * width / 3, "px")
        width <- paste0(width, "px")

        output$DS_PlotHeatmap <- renderUI({
            req(rv$data)
          if (nrow(rv$data@qdata) > limitHeatmap)
                tags$p("The dataset is too large to compute the heatmap in a reasonable time.")
            else
                plotOutput(ns("heatmap_ui"), width = width, height = height)
        })



        output$heatmap_ui <- renderPlot({
          req(rv$data)
            input$linkage
            input$distance

                withProgress(message = "Making plot", value = 100, {
                  
                    heatmapD(
                        vData = rv$data,
                        distance = input$distance,
                        cluster = input$linkage
                    )
            })
        })
    })
}



#' @import shiny
#' @rdname ds_heatmap
#' @export
ds_heatmap <- function(obj){
  ui <- fluidPage(
    mod_ds_heatmap_ui("plot")
  )
  
  server <- function(input, output, session)
    mod_ds_heatmap_server("plot", reactive({obj}))

  shinyApp(ui = ui, server = server)
}
