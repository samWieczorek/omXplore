library(DaparViz)
library(highcharter)
library(MSnbase)
library(shiny)


options(shiny.fullstacktrace = TRUE)



ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)



server <- function(input, output, session) {
   
  #
  # Example with a QFeatures dataset
  #
  data(ft)
  obj <- ft
  
  #
  # Example with a series of MSnSet datasets
  #
  # data(Exp1_R25_prot, package='DAPARdata')
  # data(Exp1_R25_pept, package='DAPARdata')
  # data(Exp1_R2_pept, package='DAPARdata')
  # obj <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
  #                    nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))
  # 
  # 
  
  mod_view_dataset_server("dataset", 
                          ll.vizData = reactive({convert2viz(obj)}))
}

if (interactive())
  shinyApp(ui, server)