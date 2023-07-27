#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name plot-variance
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param vData xxx
#' @param pal.name A `character(1)` which is the name of the palette from the 
#' package [RColorBrewer] from which the colors are taken. 
#' Default value is 'Set1'.
#' 
#' @return NA
#'
#' @example examples/ex_mod_ds_variance.R
#'
NULL


#' @importFrom shiny NS tagList
#' @rdname plot-variance
#' @export
mod_ds_variance_ui <- function(id) {
    ns <- NS(id)
    tagList(
        helpText("Display the condition-wise distributions of the log-intensity 
        CV (Coefficient of Variation) of the protein/peptides."),
        helpText("For better visualization, it is possible to zoom in by
            click-and-drag."),
        highcharter::highchartOutput(ns("viewDistCV"), width = 600, height = 600)
    )
}



#'
#' @importFrom shiny NS tagList
#' @rdname plot-variance
#' @export
mod_ds_variance_server <- function(id,
                                   vData,
                                   pal.name = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$viewDistCV <- renderHighchart({
          req(vData())
          withProgress(message = "Making plot", value = 100, {
                varDist <- CVDist(vData(), pal.name)
            })
        })
    })
}
