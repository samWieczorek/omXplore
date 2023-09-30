library(DaparViz)
library(highcharter)
library(MSnbase)
library(shiny)

options(shiny.fullstacktrace = TRUE)



ui <- fluidPage(
  
  mod_plots_tracking_ui("tracker")
)



server <- function(input, output, session) {
  # data(ft, package='DaparToolshed')
  # data(ft_na, package='DaparToolshed')
  # vList <- convert2viz(ft)
  # vList_na <- convert2viz(ft_na)
  
  #
  # Example with a QFeatures dataset
  #
  data(Exp1_R25_pept, package='DaparToolshedData')
  vList <- convert2viz(Exp1_R25_pept)
  vData <- vList@ll.vizData[[1]]
  
  #
  # Example with a series of MSnSet datasets
  #
  # data(Exp1_R25_prot, package='DAPARdata')
  # data(Exp1_R25_pept, package='DAPARdata')
  # data(Exp1_R2_pept, package='DAPARdata')
  # ll.tmp <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
  #                    nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))
  # 
  # vList <- convert2viz(ll.tmp)
  
  data(Exp1_R25_pept, package='DaparToolshedData')
  #vData <- Exp1_R25_pept
  
  
  indices <- mod_plots_tracking_server("tracker", 
                                       vizData = reactive({vData}))
  
  observe({
    print(indices())
    
  })
}

if (interactive())
  shinyApp(ui, server)