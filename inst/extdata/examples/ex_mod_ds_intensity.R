library(DaparViz)

ui <- fluidPage(
  tagList(
    mod_plots_tracking_ui('tracker'),
    mod_ds_intensity_ui("iplot")
  )
)

server <- function(input, output, session) {
  data(vData_ft)
  obj <- vData_ft[1]
  indices <- mod_plots_tracking_server("tracker", 
                                       vizData = reactive({obj}))
  
  mod_ds_intensity_server("iplot",
                          vizData = reactive({obj}),
                          track.indices = reactive({indices()})
                          )
    }

if (interactive())
  shinyApp(ui = ui, server = server)