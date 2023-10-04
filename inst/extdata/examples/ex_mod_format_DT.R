library(DaparViz)

ui <- mod_format_DT_ui("dt")

server <- function(input, output, session) {
  
  rv <- reactiveValues(selected = NULL)

  data(vData_ft)
  obj <- vData_ft[[1]]
  xls_style <- NULL
  hcStyle = list(
    cols = colnames(obj@qdata)[1:6],
    vals = colnames(obj@qdata)[4],
    unique = unique(obj@conds),
    pal = RColorBrewer::brewer.pal(3, "Dark2")[seq_len(2)]
  )
  
  rv$selected <- mod_format_DT_server("dt", 
                                      data = reactive({obj@qdata}),
                                      dataForStyling = reactive({NULL}),
                                      withDLBtns = FALSE,
                                      showRownames = FALSE,
                                      dom = 'Bt',
                                      hc_style = reactive({hcStyle}),
                                      xls_style = reactive({xls_style}),
                                      filename = "export",
                                      hideCols = reactive({NULL}),
                                      selection = 'single'
                                      )
  
   observeEvent(rv$selected(), {
     print(paste0('Selected line(s):', rv$selected()))
   })
}

if (interactive())
  shinyApp(ui = ui, server = server)
