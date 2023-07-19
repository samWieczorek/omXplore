library(DaparViz)
library(highcharter)
library(MSnbase)
library(SummarizedExperiment)

options(shiny.fullstacktrace = TRUE)



ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)



server <- function(input, output, session) {
  # data(ft, package='DaparToolshed')
  # data(ft_na, package='DaparToolshed')
  # vList <- convert2viz(ft)
  # vList_na <- convert2viz(ft_na)
  
  # Example with a QFeatures dataset
  data(Exp1_R25_prot, package='DaparToolshedData')
  vList <- convert2viz(Exp1_R25_prot)
  
  # Example with a series of MSnSet datasets
  # data(Exp1_R25_prot, package='DAPARdata')
  # data(Exp1_R25_pept, package='DAPARdata')
  # data(Exp1_R2_pept, package='DAPARdata')
  # ll.tmp <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
  #                    nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))
  # 
  # vList <- convert2viz(ll.tmp)
  
  
  
  mod_view_dataset_server("dataset", 
                          ll.vizData = reactive({vList}))
}

shinyApp(ui, server)