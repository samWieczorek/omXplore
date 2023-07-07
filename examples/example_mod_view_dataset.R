data(ft, package='DaparToolshed')
data(ft_na, package='DaparToolshed')

ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)

server <- function(input, output, session) {
  mod_view_dataset_server("dataset", object = reactive({ft_na}))
}

shinyApp(ui, server)