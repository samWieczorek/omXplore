data(ft, package='DaparToolshed')
data(ft_na, package='DaparToolshed')

ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)



server <- function(input, output, session) {
  ll.vizData <- convert2viz(ft)
  
  mod_view_dataset_server("dataset", 
                          ll.vizData = reactive({ll.vizData}))
}

shinyApp(ui, server)