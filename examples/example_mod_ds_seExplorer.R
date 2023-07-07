data(ft, package='DaparToolshed')
data(ft_na, package='DaparToolshed')

ui <- mod_ds_seExplorer_ui("plot")

server <- function(input, output, session) {
        mod_ds_seExplorer_server("plot", reactive({ft_na[[1]]})
        )
    }

    shinyApp(ui = ui, server = server)