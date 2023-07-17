#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name density-plot
#' 
#' @return NA
#'
#' @example examples/example_mod_ds_density.R
#'
NULL

#' @param id A `character(1)` which is the id of the shiny module.
#'
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


#' @param id A `character(1)` which is the id of the shiny module.
#' @param data An instance of the class `matrix` or `data.frame`.
#' It must be a reactive value.
#' @param conds A `character()` of the name of conditions
#' (one condition per sample). It is not a reactive value.
#' @param pal.name It is a reactive value.
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

        observe({
            req(vizData())
            stopifnot(inherits(vizData(), "VizData"))
        })


        output$plot_ui <- renderHighchart({
            req(vizData())
            tmp <- NULL
            isolate({
                withProgress(message = "Making plot", value = 100, {
                    tmp <- densityPlot(data = vizData()@qdata,
                                       conds = vizData()@conds,
                                       pal.name = pal.name()
                                       )
                })
            })
            tmp
        })
    })
}

