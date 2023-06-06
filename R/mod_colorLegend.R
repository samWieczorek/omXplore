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
#' @examples
#' if (interactive()) {
#'     ui <- tagList(
#'mod_colorLegend_ui("plot1"),
#'mod_colorLegend_ui("plot2"),
#'mod_colorLegend_ui("plot3")
#')
#'server <- function(input, output, session) {
    
#'    # Use the default color palette
#'    mod_colorLegend_server("plot1", paste0('Default_',1:5))
#'    
#'    # Use of a user-defined color palette
#'    mod_colorLegend_server("plot2", 
#'        paste0('Pastel2_',1:5), 
#'        colors = RColorBrewer::brewer.pal(5, "Pastel2"))
#'    
#'    # Use of a  palette
#'    mod_colorLegend_server("plot3", 
#'        paste0('Accent_',1:5), 
#'        pal.name = "Accent")
#'}
#'
#'shinyApp(ui, server)
#' }
NULL





#' @importFrom shiny NS tagList
#'
#' @rdname color-legend
#'
#' @export
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
#'
#' @rdname color-legend
mod_colorLegend_server <- function(id, obj, hide.white = TRUE) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      
      output$legend <- renderUI({
        mc <- metacell.def(GetTypeofData(obj()))
        
        tagList(
          lapply(1:nrow(mc), function(x) {
            if (mc[x, "color"] != "white" ||
                (mc[x, "color"] == "white" &&
                 !isTRUE(hide.white))) {
              tagList(
                tags$div(
                  class = "color-box",
                  style = paste0(
                    "display:inline-block; vertical-align: middle;
                    width:20px; height:20px; border:1px solid #000;
                    background-color: ", mc[x, "color"], ";"
                  ),
                ),
                tags$p(
                  style = paste0("display:inline-block;
                                       vertical-align: middle;"),
                  mc[x, "node"]
                ),
                br()
              )
            }
          })
        )
      })
    }
  )
}




ui <- tagList(
  mod_colorLegend_ui("plot1"),
  mod_colorLegend_ui("plot2"),
  mod_colorLegend_ui("plot3")
  )

server <- function(input, output, session) {
  
      data(Exp1_R25_prot, package='DAPARdata')
      # Use the default color palette
      mod_colorLegend_server("plot1", reactive({Exp1_R25_prot}))
      
      # Use of a user-defined color palette
      mod_colorLegend_server("plot2", reactive({Exp1_R25_prot}))
      
      # Use of a  palette
      mod_colorLegend_server("plot3", reactive({Exp1_R25_prot}))
  }
  
shinyApp(ui, server)