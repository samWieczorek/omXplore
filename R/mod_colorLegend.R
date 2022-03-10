#' @title Color legend
#' 
#' @description 
#' xxxx
#' 
#' 
#' @name color-legend
#' 
#' @examples
#' if(interactive()){
#'  text <- LETTERS[1:5]
#'  colors <- RColorBrewer::brewer.pal(5, 'Dark2')
#'  ui <- mod_colorLegend_ui('plot')
#'  server <- function(input, output, session) {
#'    mod_colorLegend_server('plot', text, colors)
#'    }
#'  
#'  shinyApp(ui=ui, server=server)
#' }
NULL




#' @param id xxx
#'
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom shiny NS tagList
#' 
#' @rdname color-legend
#'
#' @export 
mod_colorLegend_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    bsCollapse(id = "collapseExample", 
               open = "",
               bsCollapsePanel(title = "Legend of colors",
                             uiOutput(ns('legend')),
                             style = "info")
             )
    )
  
}


#' @param id The 'id' of the shiny module.
#' @param text A `character()` xxx
#' @param colors A `HEX()` xxx
#' 
#' @export
#' 
#' @importFrom stats rnorm
#' 
#' @rdname color-legend
mod_colorLegend_server <- function(id,
                                   text,
                                   colors){
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        stopifnot(length(text) == length(colors))
      })
      
      output$legend <- renderUI({
        lapply(seq_len(length(text)), function(x){
            tagList(
                tags$div(class="color-box",
                         style = paste0("display: inline-block; 
                                      vertical-align: middle;
                                      width: 20px; 
                                      height: 20px;
                                      border: 1px solid #000; 
                                      background-color: ", 
                                        colors[x] , ";"),
                ),
                tags$p(style = paste0("display: inline-block; 
                                       vertical-align: middle;"),
                       text[x]),
                br()
              )
          })
      })
    })
  
}
