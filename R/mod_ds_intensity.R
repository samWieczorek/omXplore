#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#'@param id A `character(1)` which is the id of the shiny module.
#' @param vizData A instance of the class `VizData`
#' @name intensity-plots
#'
#' @example examples/example_mod_ds_intensity.R
#'
NULL


#' @import shiny
#' @import shinyjs
#' @importFrom stats setNames
#'
#' @return NA
#' @export
#' @rdname intensity-plots
mod_ds_intensity_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        tags$div(
            tags$div(style = "display:inline-block; vertical-align: middle;",
                uiOutput(ns("box_ui")),
                uiOutput(ns("violin_ui"))
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;",
                selectInput(ns("choosePlot"),
                    "Choose plot",
                    choices = setNames(nm = c("violin", "box")),
                    width = "100px"
                ),
                mod_seTracker_ui(ns("tracker"))
            )
        )
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
mod_ds_intensity_server <- function(id, vizData) {
    
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        indices <- reactiveVal()

        indices <- mod_seTracker_server("tracker",
                                        vizData = reactive({vizData()})
                                        )

        output$box_ui <- renderUI({
            if (input$choosePlot == "box") {
              highcharter::highchartOutput(ns("box"))
            } else {
                hidden(highcharter::highchartOutput(ns("box")))
            }
        })

        output$box <- renderHighchart({
            withProgress(message = "Making plot", value = 100, {
                tmp <- boxPlot(data = vizData()@qdata, 
                               conds = vizData()@conds, 
                               subset = indices())
            })
        })


        output$violin_ui <- renderUI({
            if (input$choosePlot == "violin") {
                imageOutput(ns("violin"))
            } else {
                hidden(imageOutput(ns("violin")))
            }
        })

        output$violin <- renderImage(
            {
                req(vizData()@qdata)
                # A temp file to save the output. It will be deleted after 
                # renderImage sends it, because deleteFile=TRUE.
                outfile <- tempfile(fileext = ".png")
                # Generate a png
                withProgress(message = "Making plot", value = 100, {
                    png(outfile)
                    pattern <- paste0("test", ".violinplot")
                    tmp <- violinPlot(data = as.matrix(vizData()@qdata), 
                                      conds = vizData()@conds, 
                                      subset = indices())
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
