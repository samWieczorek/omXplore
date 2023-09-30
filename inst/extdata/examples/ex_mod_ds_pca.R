library(highcharter)
library(DaparViz)
library(shiny)

data(ft)
vList <- convert2viz(ft)
vData <- vList@ll.vizData[[1]]


res.pca <- wrapper_pca(vData, ncp = 6)
plotPCA_Var(res.pca)
plotPCA_Ind(res.pca)
plotPCA_Eigen_hc(res.pca)




ui <- mod_ds_pca_ui("plot")

server <- function(input, output, session) {
  data(ft)
  vList <- convert2viz(ft)
  vData <- convert2viz(ft)@ll.vizData[[1]]
  data(ft)
  vData <- ft
  
  mod_ds_pca_server("plot", vizData = reactive({convert2viz(ft)@ll.vizData[[1]]}))
  }

if (interactive())
  shinyApp(ui = ui, server = server)
