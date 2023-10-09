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
#' @example inst/extdata/examples/ex_colorLegend.R
#' 
NULL


#' @export
#' @rdname color-legend
#' 
custom_metacell_colors <- function()
  list("Any" = "white",
       "Missing" = "#CF8205",
       "Missing POV" = "#E5A947",
       "Missing MEC" = "#F1CA8A",
       "Quantified" = "#0A31D0",
       "Quant. by recovery" = "#B9C4F2",
       "Quant. by direct id" = "#6178D9",
       "Combined tags" = "#1E8E05",
       "Imputed" = "#A40C0C",
       "Imputed POV" = "#E34343",
       "Imputed MEC" = "#F59898")

#' @import shiny
#' @import shinyBS
#' @rdname color-legend
#' @export
#' 
colorLegend_ui <- function(id) {
    ns <- NS(id)
    pkgs.require('shinyBS')
    shinyBS::bsCollapse(id = "collapseExample",
               open = "",
               shinyBS::bsCollapsePanel(title = "Legend of colors",
                               uiOutput(ns("legend")),
                               style = "")
               )
}



#' @export
#' @rdname color-legend
colorLegend_server <- function(id, 
                                   presentTags = reactive({NULL}), 
                                   hide.white = TRUE) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      

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



