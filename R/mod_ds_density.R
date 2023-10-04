#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param DaparViz An instance of the class `DaparViz`.
#' @param pal.name It is a reactive value.
#'
#' @name density-plot
#' 
#' @return NA
#'
#' @example inst/extdata/examples/ex_mod_ds_density.R
#'
NULL

#' @export
#' @importFrom shiny NS tagList
#' @importFrom highcharter highchartOutput
#' @rdname density-plot
mod_ds_density_ui <- function(id) {
    ns <- NS(id)
    tagList(
      shinyjs::useShinyjs(),
      fluidPage(
        shinyjs::hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
        highcharter::highchartOutput(ns("plot_ui"))
    )
    )
}


#' @export
#' @rdname density-plot
#'
#' @importFrom highcharter renderHighchart
#'
mod_ds_density_server <- function(id,
                                  DaparViz = reactive({NULL}),
                                  pal.name = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(
          data = NULL
        )
        
        observe({
          if(inherits(DaparViz(), "DaparViz"))
            rv$data <- DaparViz()

          shinyjs::toggle('badFormatMsg', condition = !inherits(DaparViz(), "DaparViz"))
        }, priority = 1000)


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

