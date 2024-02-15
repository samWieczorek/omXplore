#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name corrmatrix
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `DaparViz`
#' @param rate xxx. Default value is 0.9
#' @param showValues Default is FALSE.
#'
#' 
#' @return NA
#'
#' @examples
#' if(interactive()){
#' data(vData_ft)
#' DaparViz_corrmatrix(vData_ft[[1]])
#' }
#' 
#'
NULL

#' @importFrom shiny NS tagList
#' @import shinyjs
#' @rdname corrmatrix
#' @export
DaparViz_corrmatrix_ui <- function(id) {
    ns <- NS(id)
    tagList(
      shinyjs::useShinyjs(),
      shinyjs::hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
      uiOutput(ns("showValues_ui")),
      uiOutput(ns("rate_ui")),
      highcharter::highchartOutput(ns("plot"), width = "600px", height = "500px")
    )
}

#' @rdname corrmatrix
#' @export
#'
DaparViz_corrmatrix_server <- function(id,
                                     obj = reactive({NULL}),
                                     rate = reactive({0.5}),
                                     showValues = reactive({FALSE})) {
  
  
 
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv.corr <- reactiveValues(
          data = NULL,
          rate = NULL,
          showValues = NULL
        )

        observeEvent(id, { 
          rv.corr$rate <- rate()
          rv.corr$showValues <- showValues()
          })
        
        
        observe({
          if(inherits(obj(), "DaparViz"))
            rv.corr$data <- obj()
          
          shinyjs::toggle('badFormatMsg', condition = !inherits(obj(), "DaparViz"))
        }, priority = 1000)

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
          req(rv.corr$data)
          rv.corr$showValues
            checkboxInput(ns("showLabels"), "Show labels",
                value = rv.corr$showValues
            )
        })


        observeEvent(req(input$showLabels), {
          input$showLabels
          rv.corr$showValues <- input$showLabels
        })

        observeEvent(req(input$rate), {
            rv.corr$rate <- input$rate
        })

        output$plot <- renderHighchart({
            req(rv.corr$data)
          rv.corr$rate
          rv.corr$showValues
          
            withProgress(message = "Making plot", value = 100, {
                tmp <- corrMatrix(
                    data = rv.corr$data@qdata,
                    rate = rv.corr$rate,
                    showValues = rv.corr$showValues
                )
            })

            tmp
        })
    })
}




#' @export
#' @rdname corrmatrix
DaparViz_corrmatrix <- function(obj){
  ui <- DaparViz_corrmatrix_ui("plot")

  server <- function(input, output, session)
    DaparViz_corrmatrix_server("plot", reactive({obj}))

  shinyApp(ui = ui, server = server)
}
