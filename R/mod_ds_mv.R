#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name plot-mv
#'
#' @example examples/example_mod_ds_mv.R
NULL


#' @param id A `character(1)` which is the id of the shiny module.
#' @export
#' @importFrom shiny NS tagList
#' @importFrom highcharter highchartOutput
#' @rdname plot-mv
mod_ds_mv_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        tagList(
            fluidRow(
                column(
                    width = 4,
                    highcharter::highchartOutput(ns("histo_MV")),
                    height = "600px"
                ),
                column(
                    width = 4,
                    highcharter::highchartOutput(ns("histo_MV_per_lines"))
                ),
                column(
                    width = 4,
                    highcharter::highchartOutput(ns("histo_MV_per_lines_per_conditions"))
                )
            )
        )
    )
}

#' @param id A `character(1)` which is the id of the shiny module.
#' @param data An instance of the class [matrix]
#' @param conds A `character()` of the name of conditions
#' (one condition per sample). It is not a reactive value.
#' @param pal.name A `character(1)` which is the name of the palette from the 
#' package [RColorBrewer] from which the colors are taken. Default 
#' value is 'Set1'.
#'
#' @export
#' @importFrom highcharter renderHighchart
#'
#' @rdname plot-mv
#'
mod_ds_mv_server <- function(id,
    data,
    conds,
    pal.name = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns


        observe({
            req(data())
            stopifnot(inherits(data(), "matrix"))
        })



        output$histo_MV <- renderHighchart({
            req(data())
            pal.name()

            withProgress(message = "Making plot", value = 100, {
                tmp <- mvHisto(data(),
                    conds = conds,
                    pal.name = pal.name()
                )
            })
            tmp
        })



        output$histo_MV_per_lines <- renderHighchart({
            req(data())

            isolate({
                withProgress(message = "Making plot", value = 100, {
                    tmp <- mvPerLinesHisto(data())
                })
            })
            tmp
        })




        output$histo_MV_per_lines_per_conditions <- renderHighchart({
            req(data())
            pal.name()

            withProgress(message = "Making plot", value = 100, {
                tmp <- mvPerLinesHistoPerCondition(
                    data = data(),
                    conds = conds,
                    pal.name = pal.name()
                )
            })
            tmp
        })
    })
}
