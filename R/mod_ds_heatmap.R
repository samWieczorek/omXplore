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
#' @example examples/example_mod_ds_heatmap.R
#'
NULL


#'
#' @export
#' @importFrom shiny NS tagList
#' @rdname heatmaps
mod_ds_heatmap_ui <- function(id) {
    ns <- NS(id)
    tagList(
        div( style = "display:inline-block; vertical-align: middle; padding-right: 20px;",
                selectInput(ns("distance"), "Distance",
                    choices = setNames(nm = c("euclidean", "manhattan")),
                    selected = "euclidean",
                    width = "150px")
            ),
            div(style = "display:inline-block; vertical-align: middle; padding-right: 20px;",
                selectInput(ns("linkage"), "Linkage",
                    choices = setNames(nm = c("complete", "ward.D", "average")),
                    selected = "complete",
                    width = "150px")
            ),
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

        observe({
            req(vizData())
            stopifnot(inherits(vizData(), "VizData"))
        })


        limitHeatmap <- 20000
        height <- paste0(2 * width / 3, "px")
        width <- paste0(width, "px")

        output$DS_PlotHeatmap <- renderUI({
            req(vizData())
          #browser()
            if (nrow(vizData()@qdata) > limitHeatmap) {
                tags$p("The dataset is too big to compute the heatmap in a reasonable time.")
            } else {
                tagList(
                    plotOutput(ns("heatmap_ui"), width = width, height = height)
                )
            }
        })



        output$heatmap_ui <- renderPlot({
            input$linkage
            input$distance

                withProgress(message = "Making plot", value = 100, {
                  
                    DaparViz::heatmapD(
                        vData = vizData(),
                        distance = input$distance,
                        cluster = input$linkage
                    )
            })
        })
    })
}
