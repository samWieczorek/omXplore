library(DaparViz)
library(highcharter)
library(MSnbase)
library(shiny)


options(shiny.fullstacktrace = TRUE)



ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)



server <- function(input, output, session) {
  # data(Exp1_R25_prot, package='DaparToolshedData')
  # obj <- convert2viz(ft)
  # obj <- convert2viz(ft_na)
  
  #
  # Example with a QFeatures dataset
  #
  data(Exp1_R25_pept, package='DaparToolshedData')
  obj <- convert2viz(Exp1_R25_pept)
  
  #
  # Example with a series of MSnSet datasets
  #
  # data(Exp1_R25_prot, package='DAPARdata')
  # data(Exp1_R25_pept, package='DAPARdata')
  # data(Exp1_R2_pept, package='DAPARdata')
  # ll.tmp <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
  #                    nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))
  # 
  # obj <- convert2viz(ll.tmp)
  
  
  
  mod_view_dataset_server("dataset", object = reactive({obj}))
}

if (interactive())
  shinyApp(ui, server)