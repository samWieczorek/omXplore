#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param id A `character(1)` which is the id of the shiny module.
#' @param vizData xxx
#' @param width xxx
#'
#' @name heatmaps
#' 
#' @return NA
#'
#' @example inst/extdata/examples/ex_mod_ds_heatmap.R
#'
NULL


#'
#' @export
#' @importFrom shiny NS tagList
#' @rdname heatmaps
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




#' @export
#' @rdname heatmaps
#'
mod_ds_heatmap_server <- function(id,
                                  vizData = reactive({NULL}),
                                  width = 900) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(data = NULL)
        
        observe({
          if(inherits(vizData(), "VizData"))
            rv$data <- vizData()
          
          shinyjs::toggle('badFormatMsg', condition = !inherits(vizData(), "VizData"))
          shinyjs::toggle('linkage', condition = !inherits(vizData(), "VizData"))
          shinyjs::toggle('distance', condition = !inherits(vizData(), "VizData"))
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
                  
                    DaparViz::heatmapD(
                        vData = rv$data,
                        distance = input$distance,
                        cluster = input$linkage
                    )
            })
        })
    })
}
