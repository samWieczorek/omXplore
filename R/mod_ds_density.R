#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param vizData An instance of the class `VizData`.
#' @param pal.name It is a reactive value.
#'
#' @name density-plot
#' 
#' @return NA
#'
#' @example inst/extadata/examples/ex_mod_ds_density.R
#'
NULL

#' @export
#' @importFrom shiny NS tagList
#' @importFrom highcharter highchartOutput
#' @rdname density-plot
mod_ds_density_ui <- function(id) {
    ns <- NS(id)
    tagList(
      highcharter::highchartOutput(ns("plot_ui"))
    )
}


#' @export
#' @rdname density-plot
#'
#' @importFrom highcharter renderHighchart
#'
mod_ds_density_server <- function(id,
                                  vizData = reactive({NULL}),
                                  pal.name = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(
          data = NULL
        )
        
        observe({
            req(vizData())
            if(inherits(vizData(), "VizData"))
              rv$data <- vizData()
        })


        output$plot_ui <- renderHighchart({
            req(rv$data)
            tmp <- NULL
            isolate({
                withProgress(message = "Making plot", value = 100, {
                    tmp <- densityPlot(data = rv$data@qdata,
                                       conds = rv$data@conds,
                                       pal.name = pal.name()
                                       )
                })
            })
            tmp
        })
    })
}

