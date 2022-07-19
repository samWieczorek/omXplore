#' @title Bar plot of missing values per lines using highcharter.
#'
#' @description
#'
#' This method plots a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (ie proteins).
#'
#'
#' @details
#'
#' - distribution of the missing values per line,
#'
#' - a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (ie proteins) and per conditions,
#'
#' - Histogram of missing values.
#'
#'
#' - Variance : Builds a densityplot of the CV of entities in numeric matrix.
#' The CV is calculated for each condition present in the dataset
#' (see the slot \code{'Condition'} in the \code{colData()} DataFrame)
#'
#' - Heatmap:
#'
#'
#' The function [heatmapD()]
#'
#'
#' The function [] is inspired from the function 'heatmap.2'
#' that displays a numeric matrix. For more information, please refer to the help
#' of the heatmap.2 function.
#'
#'
#' @section Missing values:
#'
#' #' - distribution of the missing values per line,
#'
#' - a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (ie proteins) and per conditions,
#'
#' - Histogram of missing values.
#'
#' @name ds-plots
#'
#' @return A plot
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @examples
#' library(SummarizedExperiment)
#' data(ft, package='DaparViz')
#' data(ft_na, package='DaparViz')
#'
#' qData <- assay(ft, 1)
#' conds <- design(ft)$Condition
#' pal <- "Dark2"
#'
#' data(ft, package='DaparViz')
#' qData <- assay(ft, 1)
#' conds <- design(ft)$Condition
#' densityPlot(qData, conds)
#'
#' legend <- design(ft)$Sample.name
#' densityPlot(qData, conds, pal.name = "Dark2")
#'
#' CVDist(qData, conds)
#'
#' #----------------------------------------------
#' # Plots a heatmap for generic quantitative data
#' #----------------------------------------------
#'
#' heatmapD(assay(ft, 1), conds)
#'
#' #----------------------------------------------
#' # Plots a heatmap for missing values visualization
#' #----------------------------------------------
#'
#'
#' #' #----------------------------------------
#' # Plots a histogram of missing values
#' #----------------------------------------
#'
#' mvPerLinesHisto(qData)
#'
#' #----------------------------------------
#' # Plots a histogram of missing values
#' #----------------------------------------
#'
#' mvHisto(qData, conds, pal.name = pal)
#'
#' #----------------------------------------
#' # Plots a histogram of missing values
#' #----------------------------------------
#'
#' data(ft, package='DaparViz')
#' mv.heatmap(assay(ft, 1))
#'
#' #----------------------------------------
#' # Launch a the shiny module for all plots
#' # Here, an example with the density plot
#' #----------------------------------------
#'
#' if (interactive()) {
#'     data(ft_na, package='DaparViz')
#'     ui <- mod_all_ds_ui("plot")
#'
#'     server <- function(input, output, session) {
#'         mod_all_ds_server("plot", object = reactive({
#'             ft_na
#'         }))
#'     }
#'
#'     shinyApp(ui = ui, server = server)
#' }
NULL







#' @rdname ds-plots
#' @export
listPlotModules <- function() {
    ll <- ls("package:DaparViz")
    ll <- ll[grep("mod_ds_", ll)]
    ll <- gsub("_server", "", ll)
    ll <- gsub("_ui", "", ll)
    ll <- unique(ll)
    ll
}




#' @param id A `character(1)` for the 'id' of the shiny module. It must be
#' the same as for the server function.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs
#' @rdname ds-plots
#' @export
mod_all_ds_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        fluidPage(
            div(
                style = general_style,
                uiOutput(ns("chooseDataset_ui"))
            ),
            div(
                style = general_style,
                uiOutput(ns("ShowVignettes_ui"))
            ),
            br(), br(), br(),
            uiOutput(ns("ShowPlots_ui"))
        )
    )
}

#' @param id A `character(1)` for the 'id' of the shiny module. It must be
#' the same as for the '*_ui' function.
#'
#' @param object A instance of the class `QFeatures`.
#'
#' @importFrom shinyjs show hide hidden
#' @import shiny
#'
#' @rdname ds-plots
#' @export
#'
mod_all_ds_server <- function(id, object) {
    if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("Please install SummarizedExperiment: 
            BiocManager::install('SummarizedExperiment')")
    }
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        .width <- .height <- 40

        ll.mods <- listPlotModules()
        conds <- reactiveVal()
        current.se <- reactiveVal()
        btns.history <- reactiveVal(rep(0, length(ll.mods)))


        observe({
            req(object())
            stopifnot(inherits(object(), "QFeatures"))
            conds(SummarizedExperiment::colData(object())$Condition)
        })


        observeEvent(GetVignettesBtns(), {
            clicked <- which(btns.history() != GetVignettesBtns())
            shinyjs::show(paste0("div_", ll.mods[clicked], "_large"))
            lapply(ll.mods[-clicked], function(y) {
                shinyjs::hide(paste0("div_", y, "_large"))
            })
            btns.history(GetVignettesBtns())
        })

        GetVignettesBtns <- reactive({
            unlist(lapply(ll.mods, function(x) input[[x]]))
        })

        output$ShowPlots_ui <- renderUI({
            lapply(ll.mods, function(x) {
                shinyjs::hidden(
                    div(
                        id = ns(paste0("div_", x, "_large")),
                        do.call(
                            paste0(x, "_ui"),
                            list(ns(paste0(x, "_large")))
                        )
                    )
                )
            })
        })


        output$ShowVignettes_ui <- renderUI({
            lapply(ll.mods, function(x) {
                actionButton(ns(x),
                    label = tagList(
                        p(gsub("mod_ds_", "", x)),
                        img(
                            src = paste0("images/", 
                                gsub("mod_", "", x), ".png"),
                            height = "50px"
                        )
                    ),
                    style = "padding: 0px;
                      border: none;
                      background-size: cover;
                           background-position: center;"
                )
            })
        })

        observeEvent(input$chooseDataset, {
            current.se(object()[[input$chooseDataset]])
        })

        output$chooseDataset_ui <- renderUI({
            req(object())
            if (length(names(object())) == 0) {
                choices <- list(" " = character(0))
            } else {
                choices <- names(object())
            }

            selectInput(ns("chooseDataset"), "Dataset",
                choices = choices,
                selected = names(object())[length(object())],
                width = 200
            )
        })



        #
        # Calls to server modules
        #
        mod_ds_seExplorer_server("mod_ds_seExplorer_large",
            se = reactive({
                current.se()
            })
        )


        mod_ds_intensity_server("mod_ds_intensity_large",
            se = reactive({
                current.se()
            }),
            conds = conds()
        )


        mod_ds_pca_server("mod_ds_pca_large",
            data = reactive({
                SummarizedExperiment::assay(current.se())
            }),
            conds = conds()
        )


        mod_ds_variance_server("mod_ds_variance_large",
            data = reactive({
                SummarizedExperiment::assay(current.se())
            }),
            conds = conds()
        )

        mod_ds_corrmatrix_server(
            id = "mod_ds_corrmatrix_large",
            data = reactive({
                SummarizedExperiment::assay(current.se())
            })
        )



        mod_ds_heatmap_server("mod_ds_heatmap_large",
            data = reactive({
                SummarizedExperiment::assay(current.se())
            }),
            conds = conds()
        )


        mod_ds_mv_server("mod_ds_mv_large",
            data = reactive({
                SummarizedExperiment::assay(current.se())
            }),
            conds = conds()
        )

        mod_ds_density_server("mod_ds_density_large",
            data = reactive({
                SummarizedExperiment::assay(current.se())
            }),
            conds = conds()
        )
    })
}
