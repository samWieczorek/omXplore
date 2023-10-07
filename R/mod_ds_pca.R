#' @title Bar plot of missing values per lines using highcharter.
#'
#' @description
#'
#' This method plots a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (ie proteins).
#' @name ds-pca
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `DaparViz`
#'
#' @return A plot
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @examples
#' data(vData_ft)
#' ds_pca(vData_ft[[1]])
#'
NULL


#' @rdname ds-pca
#' @importFrom shiny NS tagList uiOutput
mod_ds_pca_ui <- function(id) {
    ns <- NS(id)
    tagList(
      shinyjs::useShinyjs(),
      shinyjs::hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
      uiOutput(ns("WarningNA_PCA")),
      uiOutput(ns("pcaOptions")),
      uiOutput(ns("pcaPlots"))
    )
}

#'
#' @rdname ds-pca
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom highcharter renderHighchart
#'
#' @export
mod_ds_pca_server <- function(id,
                              obj) {
    pkgs.require('factoextra')
  
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        rv.pca <- reactiveValues(
          data = NULL,
          PCA_axes = NULL,
          res.pca = NULL,
          PCA_varScale = NULL
        )
        
        
        observe({
          
          if(inherits(obj(), "DaparViz"))
            rv.pca$data <- as.matrix(obj()@qdata)
          
          shinyjs::toggle('badFormatMsg', condition = is.null(rv.pca$data))
        }, priority = 1000)
        
       output$WarningNA_PCA <- renderUI({
            req(rv.pca$data)
            req(length(which(is.na(rv.pca$data))) > 0)

            tagList(
                tags$p(style = "color:red;font-size: 20px",
                "Warning: As your dataset contains missing values, 
                the PCA cannot be computed. Please impute them first.")
            )
        })



        output$pcaOptions <- renderUI({
            req(rv.pca$data)
            req(rv.pca$res.pca)
            req(length(which(is.na(rv.pca$data))) == 0)
            tagList(
                tags$div(
                    tags$div(style = "display:inline-block; vertical-align: middle; padding-right: 20px;",
                        numericInput(ns("pca.axe1"), "Dimension 1",
                                     min = 1, 
                                     max = Compute_PCA_dim(),
                                     value = 1,
                                     width = "100px")),
                    tags$div(
                        style = "display:inline-block; vertical-align: middle;",
                        numericInput(ns("pca.axe2"), "Dimension 2",
                                     min = 1,
                                     max = Compute_PCA_dim(),
                                     value = 2,
                                     width = "100px")
                    ),
                    tags$div(style = "display:inline-block; vertical-align: middle; padding-right: 20px;",
                        checkboxInput(ns("varScale_PCA"), "Variance scaling",
                                      value = rv.pca$PCA_varScale)
                    )
                )
            )
        })


        observeEvent(c(input$pca.axe1, input$pca.axe2), {
            rv.pca$PCA_axes <- c(input$pca.axe1, input$pca.axe2)
        })


        observeEvent(input$varScale_PCA, {
            rv.pca$PCA_varScale <- input$varScale_PCA
            rv.pca$res.pca <- wrapper_pca(DaparViz = DaparViz(),
                                          var.scaling = rv.pca$PCA_varScale,
                                          ncp = Compute_PCA_dim()
                                          )
        })

        observeEvent(rv.pca$data, {
            if (length(which(is.na(rv.pca$data))) == 0) {
                rv.pca$res.pca <- wrapper_pca(DaparViz = DaparViz(),
                                              var.scaling = rv.pca$PCA_varScale,
                                              ncp = Compute_PCA_dim()
                                              )
            }
        })


        output$pcaPlots <- renderUI({
            req(rv.pca$data)
            req(length(which(is.na(rv.pca$data))) == 0)
            req(rv.pca$res.pca$var$coord)

            tagList(
                plotOutput(ns("pcaPlotVar")),
                plotOutput(ns("pcaPlotInd")),
                mod_format_DT_ui(ns("PCAvarCoord")),
                highcharter::highchartOutput(ns("pcaPlotEigen"))
            )
        })
        
        tryCatch({
        mod_format_DT_server("PCAvarCoord",
            data = reactive({as.data.frame(rv.pca$res.pca$var$coord)}),
            showRownames = TRUE,
            full_style = reactive({
                list(
                    cols = colnames(rv.pca$res.pca$var$coord),
                    vals = colnames(rv.pca$res.pca$var$coord),
                    unique = unique(DaparViz@conds),
                    pal = RColorBrewer::brewer.pal(3, "Dark2")[seq_len(2)]
                )
            })
        )},
        warning = function(w) {
          shinyjs::info(conditionMessage(w))
          return(NULL)
        },
        error = function(e) {
          shinyjs::info(conditionMessage(e))
          return(NULL)
        },
        finally = {
          # cleanup-code
        }
        )

        output$pcaPlotVar <- renderPlot({
            req(c(rv.pca$PCA_axes, rv.pca$res.pca))
            withProgress(message = "Making plot", value = 100, {
                factoextra::fviz_pca_var(rv.pca$res.pca,
                    axes = rv.pca$PCA_axes,
                    col.var = "cos2",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE
                )
            })
        })

        output$pcaPlotInd <- renderPlot({
            req(c(rv.pca$PCA_axes, rv.pca$res.pca))
            withProgress(message = "Making plot", value = 100, {
                factoextra::fviz_pca_ind(rv.pca$res.pca,
                                         axes = rv.pca$PCA_axes,
                                         geom = "point"
                                         )
            })
        })


        output$pcaPlotEigen <- renderHighchart({
            req(rv.pca$res.pca)

            withProgress(message = "Making plot", value = 100, {
                plotPCA_Eigen(rv.pca$res.pca)
            })
        })



        Compute_PCA_dim <- reactive({
            nmax <- 12 # ncp should not be greater than...
            # for info, ncp = number of components or dimensions in PCA results

            y <- rv.pca$data
            nprot <- dim(y)[1]
            n <- dim(y)[2] # If too big, take the number of conditions.

            if (n > nmax)
                n <- length(unique(DaparViz@conds))

            ncp <- min(n, nmax)
            ncp
        })
    })
}


#' @import shiny
#' @export
#' @rdname ds-pca
ds_pca <- function(obj){
  
  ui <- mod_ds_pca_ui("plot")

server <- function(input, output, session)
  mod_ds_pca_server("plot", obj = reactive({obj}))
shinyApp(ui = ui, server = server)
}