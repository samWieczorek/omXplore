#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name corrmatrix
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param vizData xxx
#' @param rate xxx. Default value is 0.9
#' @param showValues Default is FALSE.
#'
#' 
#' @return NA
#'
#' @example examples/example_mod_ds_corrmatrix.R
#'
NULL

#' @export
#' @importFrom shiny NS tagList
#' @rdname corrmatrix
mod_ds_corrmatrix_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("showValues_ui")),
        uiOutput(ns("rate_ui")),
        highcharter::highchartOutput(ns("plot"), width = "600px", height = "500px")
    )
}

#' @export
#' @rdname corrmatrix
#'
mod_ds_corrmatrix_server <- function(id,
                                     vizData = reactive({NULL}),
                                     rate = reactive({0.5}),
                                     showValues = reactive({FALSE})) {
  
  
 
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        
        observe({
           req(vizData())
            stopifnot(inherits(vizData(), 'VizData'))
        })
        

        rv.corr <- reactiveValues(
          rate = NULL,
          showValues = FALSE
        )


        output$rate_ui <- renderUI({
            req(rv.corr$rate)
            sliderInput(ns("rate"),
                "Tune to modify the color gradient",
                min = 0,
                max = 1,
                value = rv.corr$rate,
                step = 0.01
            )
        })


        output$showValues_ui <- renderUI({
            checkboxInput(ns("showLabels"),
                "Show labels",
                value = rv.corr$showValues
            )
        })


        observeEvent(req(!is.null(input$showLabels)), {
            rv.corr$showValues <- input$showLabels
        })

        observeEvent(req(rate()), {
            rv.corr$rate <- rate()
        })

        observeEvent(req(input$rate), {
            rv.corr$rate <- input$rate
        })

        output$plot <- renderHighchart({
            req(vizData())

            withProgress(message = "Making plot", value = 100, {
                tmp <- corrMatrix(
                    data = vizData()@qdata,
                    rate = rv.corr$rate,
                    showValues = rv.corr$showValues
                )
            })

            tmp
        })
    })
}
