#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name corrmatrix
#' 
#' @return NA
#'
#' @example examples/example_mod_ds_corrmatrix.R
#'
NULL

#' @param id A `character(1)` which is the id of the shiny module.
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

#' @param id A `character(1)` which is the id of the shiny module.
#' @param object xxx
#' @param data xxx
#' @param rate xxx. Default value is 0.9
#' @param showValues Default is FALSE.
#'
#' @export
#' @rdname corrmatrix
#'
mod_ds_corrmatrix_server <- function(id,
                                     obj,
                                     data = reactive({NULL}),
                                     rate = reactive({0.5}),
                                     showValues = reactive({FALSE})) {
  
  

 
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        .data <- reactiveVal()
        
        observeEvent(id, {
          
          if (is.null(obj()) && is.null(data)){
            warning("is.null(obj()) && is.null(data)")
            return(NULL)
          }
          
          if (!is.null(obj()) && !is.null(data)){
            warning("!is.null(obj()) && !is.null(data)")
            return(NULL)
          }
          
          if (!is.null(obj)){
            if(inherits(data(), "MSnset") || inherits(data(), 'QFeatures'))
              rv.corr$data <- Build_GenericData(obj)
            else {
              warning('toto')
              return(NULL)
            }
          }
          if (!is.null(data) && !inherits(data(), "GenericData")){
            warning('toto')
            return(NULL)
          }

        })
        

        rv.corr <- reactiveValues(
          data = NULL,
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
            req(data())

            withProgress(message = "Making plot", value = 100, {
                tmp <- corrMatrix(
                    data = data(),
                    rate = rv.corr$rate,
                    showValues = rv.corr$showValues
                )
            })

            tmp
        })
    })
}
