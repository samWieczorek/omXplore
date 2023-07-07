#' @title Color legend
#'
#' @description
#' xxxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj A `character()` xxx
#' @param hide.white  xxxxx
#' 
#' @name color-legend
#' 
#' @return NA
#'
#' @example examples/example_mod_colorLegend.R
#' 
NULL





#' @import shiny
#'
#' @rdname color-legend
#'
#' @export
#' 
mod_colorLegend_ui <- function(id) {
    ns <- NS(id)
    if (!requireNamespace("shinyBS", quietly = TRUE)) {
        stop("Please install shinyBS: BiocManager::install('shinyBS')")
    }

    shinyBS::bsCollapse(
      id = "collapseExample",
      open = "",
      shinyBS::bsCollapsePanel(
        title = "Legend of colors",
        uiOutput(ns("legend")),
        style = ""
      )
    )
}



#' @export
#' @import DaparToolshed
#' @rdname color-legend
mod_colorLegend_server <- function(id, obj, hide.white = TRUE) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      if (!requireNamespace("DaparToolshed", quietly = TRUE)) {
        stop("Please install DaparToolshed: BiocManager::install('DaparToolshed')")
      }
      
      output$legend <- renderUI({
        req(obj)
        stopifnot(inherits(obj, "SummarizedExperiment"))
        
        mc <- custom_metacell_colors()
        
        
        # Keep only tags that are in the dataset
        presentTags <- GetMetacellTags(obj,
                                level = typeDataset(obj),
                                onlyPresent = TRUE,
                                all = FALSE)

     
        
        tagList(
          lapply(presentTags, function(x) {
           # browser()
            if (mc[[x]] != "white" || (mc[[x]] == "white" && !isTRUE(hide.white))) {
              tagList(
                tags$div(
                  class = "color-box",
                  style = paste0("display:inline-block; vertical-align: middle;
                    width:20px; height:20px; border:1px solid #000; background-color: ", mc[[x]], ";"),
                ),
                tags$p(
                  style = paste0("display:inline-block;  vertical-align: middle;"),  x),
                br()
              )
            }
          })
        )
      })
    }
  )
}



