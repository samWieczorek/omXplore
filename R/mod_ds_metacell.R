#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name metacell-plots
#' 
#' @return NA
#'
#' @example examples/example_mod_ds_metacell.R
#'
NULL

#' @rdname metacell-plots
#' @export
#' 
mod_ds_metacell_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),

        p('Select one or several tag(s) to display statistics about'),
        uiOutput(ns('chooseTagUI')),
        fluidRow(
            column(width = 4,
                   highchartOutput(ns("qMetacell")), height = "600px"),
            column(width = 4,
                   highchartOutput(ns("qMetacell_per_lines"))),
            column(width = 4,
                   highchartOutput(ns("qmetacell_per_lines_per_conds")))
            )
        )
}


#' @rdname metacell-plots
#' @export
#' 
mod_ds_mv_server <- function(id, 
                             obj = reactive({NULL}), 
                             design = NULL,
                             pal = reactive({NULL}), 
                             pattern = reactive({NULL}),
                             showSelect = reactive({TRUE})) {
    moduleServer(id, function(input, output, session) {
            ns <- session$ns
            
            rv <- reactiveValues(
                chooseTag = pattern(),
                showSelect = if(is.null(pattern())) TRUE else showSelect()
            )
            
            tmp.tags <- mod_metacell_tree_server('tree', obj = reactive({obj()}))
            
            observeEvent(tmp.tags()$values, ignoreNULL = FALSE, ignoreInit = TRUE,{
                rv$chooseTag <- tmp.tags()$values
            })
            
            
            output$chooseTagUI <- renderUI({
                req(obj())
                mod_metacell_tree_ui(ns('tree'))
             })

            output$qMetacell <- renderHighchart({
               tmp <- NULL
               tmp <- metacellHisto_HC(object = obj(),
                                       design = design,
                                       pattern = rv$chooseTag,
                                       pal = pal()
                                        )
                tmp
            })



            output$qMetacell_per_lines <- renderHighchart({
               tmp <- NULL
                tmp <-
                  metacellPerLinesHisto_HC(object = obj(),
                                             design = design,
                                             pattern = rv$chooseTag,
                                             indLegend = c(2:nrow(design))
                                             )
                # future(createPNGFromWidget(tmp,pattern))
                # })
                tmp
            })



            output$qmetacell_per_lines_per_conds <- renderHighchart({
               tmp <- NULL
                # isolate({
                # pattern <- paste0(GetCurrentObjName(),".MVplot2")
                tmp <- metacellPerLinesHistoPerCondition_HC(object = obj(),
                                                            design = design,
                                                            pattern = rv$chooseTag,
                                                            pal = pal()
                                                            )
                # future(createPNGFromWidget(tmp,pattern))
                # })
                tmp
            })
        }
    )
}



