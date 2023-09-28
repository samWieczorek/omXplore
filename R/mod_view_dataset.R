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
#' @param id A `character(1)` for the 'id' of the shiny module. It must be
#' the same as for the '*_ui' function.
#' @param ll.vizData A instance of the class `VizList`.

#'
#' @return A plot
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @example examples/ex_mod_view_dataset.R
#' 
NULL







#' @rdname ds-plots
#' @export
listPlotModules <- function() {
  # Lists module in the package `DaparViz`
  ll.daparViz <- ls("package:DaparViz")
  ll.daparViz <- ll.daparViz[grep("mod_ds_", ll.daparViz)]
  ll.daparViz <- gsub("_server", "", ll.daparViz)
  ll.daparViz <- gsub("_ui", "", ll.daparViz)
  ll.daparViz <- unique(ll.daparViz)
  ll.daparViz
  
  # Lists module in the global environment
  ll.env <- lsf.str(envir = globalenv())
  ll.env <- ll.env[grep("mod_ds_", ll.env)]
  ll.env <- gsub("_server", "", ll.env)
  ll.env <- gsub("_ui", "", ll.env)
  ll.env <- unique(ll.env)
  
  c(ll.daparViz, ll.env)
}





#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @rdname ds-plots
#' @export
#' 
#' @example examples/ex_mod_view_dataset.R
#' 
mod_view_dataset_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        fluidPage(
            div(style = general_style,  uiOutput(ns("chooseDataset_ui"))),
            div(style = general_style, uiOutput(ns("ShowVignettes_ui"))),
            br(), br(), br(),
            uiOutput(ns("ShowPlots_ui"))
        )
    )
}

#'
#' @importFrom shinyjs show hide hidden
#' @import shiny
#'
#' @rdname ds-plots
#' @export
#'
mod_view_dataset_server <- function(id, 
                                    ll.vizData = reactive({NULL})
                                    ) {

    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        .width <- .height <- 40

        ll.mods <- listPlotModules()
        conds <- reactiveVal()
        current.se <- reactiveVal()
        btns.history <- reactiveVal(rep(0, length(ll.mods)))


        observe({
            req(length(ll.vizData()) > 0)
            stopifnot(inherits(ll.vizData(), "VizList"))
            #browser()
            conds(ll.vizData()@ll.vizData[[1]]@conds)
        })


        observeEvent(GetVignettesBtns(), ignoreInit = TRUE, {
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
                    div(id = ns(paste0("div_", x, "_large")),
                        do.call(
                            paste0(x, "_ui"), list(ns(paste0(x, "_large")))
                        )
                    )
                )
            })
        })


        output$ShowVignettes_ui <- renderUI({
          
          lapply(ll.mods, function(x) {
            
            # By default, search image from the images directory of the DaparViz
            # package. This works for built-in plot modules. For external modules,
            # then load customized resource path
            img_path <- system.file('images', paste0(gsub("mod_", "", x), ".png"), package='DaparViz')
            if (file.exists(img_path))
              img_src <- paste0("images/", gsub("mod_", "", x), ".png")
            else
              img_src <- paste0("img_", gsub("mod_", "", x), "/", gsub("mod_", "", x), ".png")
            
             actionButton(ns(x),
                    label = tagList(
                        p(gsub("mod_ds_", "", x)),
                        tags$img(src = img_src, height = "50px")
                        ),
                    style = "padding: 0px; border: none; background-size: cover; background-position: center;"
                )
            })
        })

        observeEvent(req(ll.vizData(), input$chooseDataset), ignoreNULL = TRUE,{
            current.se(ll.vizData()@ll.vizData[[input$chooseDataset]])
        })

        output$chooseDataset_ui <- renderUI({
            req(ll.vizData())

            if (length(ll.vizData()@ll.vizData) == 0) {
                choices <- list(" " = character(0))
            } else {
                choices <- names(ll.vizData()@ll.vizData)
            }

            selectInput(ns("chooseDataset"), "Dataset",
                choices = choices,
                selected = names(ll.vizData()@ll.vizData)[length(ll.vizData())],
                width = 200
            )
        })

  observe({
    req(current.se())
    for (mod in listPlotModules())
      do.call(paste0(mod, '_server'), 
              list(id = paste0(mod, '_large'),
                   vizData = reactive({current.se()})
                  )
              )
    })
  
})
}







