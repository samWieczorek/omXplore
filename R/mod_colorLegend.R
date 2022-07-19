#' @title Color legend
#'
#' @description
#' xxxx
#'
#'
#' @name color-legend
#' 
#' @return NA
#'
#' @examples
#' if (interactive()) {
#'     text <- LETTERS[1:5]
#'     colors <- RColorBrewer::brewer.pal(5, "Dark2")
#'     ui <- mod_colorLegend_ui("plot")
#'     server <- function(input, output, session) {
#'
#'         # Use the default color palette
#'         mod_colorLegend_server("plot", text)
#'
#'         # Use of a user-defined color palette
#'         mod_colorLegend_server("plot", text, colors = colors)
#'
#'         # Use of a  palette
#'         mod_colorLegend_server("plot", text, pal.name = "Dark3")
#'     }
#'
#'     shinyApp(ui = ui, server = server)
#' }
NULL




#' @param id A `character(1)` which is the id of the shiny module.
#'
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

    fluidPage(
        shinyBS::bsCollapse(
            id = "collapseExample",
            open = "",
            shinyBS::bsCollapsePanel(
                title = "Legend of colors",
                uiOutput(ns("legend")),
                style = "info"
            )
        )
    )
}


#' @param id A `character(1)` which is the id of the shiny module.
#' @param text A `character()` xxx
#' @param pal.name A `character()` xxxxx
#' @param colors A `HEX()` containing the color codes to apply to the plot.
#' The number of colors must be equal to the length of parameter 'text'.
#'
#' @export
#'
#' @rdname color-legend
mod_colorLegend_server <- function(id,
                                   text,
                                   pal.name = NULL,
                                   colors = NULL) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns


            Get_colors <- reactive({
                .colors <- NULL

                cond1 <- is.null(pal.name) && is.null(colors)
                cond2 <- !is.null(pal.name) && is.null(colors)
                cond3 <- is.null(pal.name) && !is.null(colors)

                stopifnot(cond1 || cond2 || cond3)

                if (isTRUE(cond1)) {
                    .colors <- ExtendPalette(length(text), "Dark2")
                } else if (isTRUE(cond2)) {
                    .colors <- ExtendPalette(length(text), pal.name)
                } else if (isTRUE(cond3)) {
                    stopifnot(length(text) == length(colors))
                    .colors <- colors
                }

                .colors
            })

            output$legend <- renderUI({
                Get_colors()
                lapply(seq_len(length(text)), function(x) {
                    tagList(
                        tags$div(
                            class = "color-box",
                            style = paste0(
                                "display: inline-block;
                                      vertical-align: middle;
                                      width: 20px;
                                      height: 20px;
                                      border: 1px solid #000;
                                      background-color: ",
                                Get_colors()[x], ";"
                            ),
                        ),
                        tags$p(
                            style = paste0("display: inline-block;
                                       vertical-align: middle;"),
                            text[x]
                        ),
                        br()
                    )
                })
            })
        }
    )
}
