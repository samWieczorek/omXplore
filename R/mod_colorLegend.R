#' @title Color legend for DaparToolshed
#'
#' @description
#' Shows xxx based on the tags in the package 'DaparToolshed'
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param presentTags A vector of `character()` which correspond to the tags.
#' @param hide.white  A `boolean()` to indicate whether the white cells must be
#' hidden or not.
#' 
#' @name color-legend
#' 
#' @return NA
#'
#' @example examples/ex_mod_colorLegend.R
#' 
NULL


#' @import shiny
#' @import shinyBS
#' @rdname color-legend
#' @export
#' 
mod_colorLegend_ui <- function(id) {
    ns <- NS(id)
    if (!requireNamespace("shinyBS", quietly = TRUE)) {
        stop("Please install shinyBS: BiocManager::install('shinyBS')")
    }

    bsCollapse(id = "collapseExample",
               open = "",
               bsCollapsePanel(title = "Legend of colors",
                               uiOutput(ns("legend")),
                               style = "")
               )
}



#' @export
#' @import DaparToolshed
#' @rdname color-legend
mod_colorLegend_server <- function(id, 
                                   presentTags = NULL, 
                                   hide.white = TRUE) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      if (!requireNamespace("DaparToolshed", quietly = TRUE)) {
        stop("Please install DaparToolshed: BiocManager::install('DaparToolshed')")
      }
      
      output$legend <- renderUI({
         req(presentTags)
       
         mc <- custom_metacell_colors()
        
        tagList(
          lapply(presentTags, function(x) {
           if (mc[[x]] != "white" || (mc[[x]] == "white" && !isTRUE(hide.white))) {
              tagList(
                tags$div(
                  class = "color-box",
                  style = paste0("display:inline-block; vertical-align: middle;
                    width:20px; height:20px; border:1px solid #000; background-color: ", mc[[x]], ";"),
                ),
                tags$p(style = paste0("display:inline-block;  vertical-align: middle;"),  x),
                br()
              )
            }
          })
        )
      })
    }
  )
}



