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
#' ds_corrmatrix(vData_ft[[1]])
#' }
#' 
#'
NULL

#' @importFrom shiny NS tagList
#' @rdname corrmatrix
#' @export
mod_ds_corrmatrix_ui <- function(id) {
    ns <- NS(id)
    tagList(
      useShinyjs(),
      hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
      uiOutput(ns("showValues_ui")),
        uiOutput(ns("rate_ui")),
        highcharter::highchartOutput(ns("plot"), width = "600px", height = "500px")
    )
}

#' @rdname corrmatrix
#' @export
#'
mod_ds_corrmatrix_server <- function(id,
                                     obj = reactive({NULL}),
                                     rate = reactive({0.5}),
                                     showValues = reactive({FALSE})) {
  
  
 
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv.corr <- reactiveValues(
          data = NULL,
          rate = NULL,
          showValues = FALSE
        )

        observe({
          if(inherits(obj(), "DaparViz"))
            rv.corr$data <- obj()
          
          shinyjs::toggle('badFormatMsg', condition = !inherits(obj(), "DaparViz"))
        }, priority = 1000)

        output$rate_ui <- renderUI({
            req(rv.corr$rate, rv.corr$data)
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
            req(rv.corr$data)

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
ds_corrmatrix <- function(obj){
  ui <- mod_ds_corrmatrix_ui("plot")

  server <- function(input, output, session)
    mod_ds_corrmatrix_server("plot", reactive({obj}))

  shinyApp(ui = ui, server = server)
}
