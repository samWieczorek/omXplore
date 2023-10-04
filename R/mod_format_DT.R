#' @title   mod_format_DT_ui and mod_format_DT_server
#'
#' @description
#'
#' A shiny Module.
#' 
#' See `DT` package homepage for more details about styling tables.
#' If no style is precised, this module show the raw data.
#' If any style is given, then the dataset must be well configured (I.e. it 
#' must contain the correct columns )
#' 
#' 
#' @param id shiny id
#' @param data A `data.frame`
#' @param dataForStyling xxx
#' @param withDLBtns A boolean to indicate whether to display download buttons or not.
#' @param showRownames A boolean to indicate whether to show rownames.
#' @param dom xxx
#' @param hc_style xxx
#' @param xls_style A list of four items:
#' * cols: a vector of colnames of columns to show,
#' * vals: a vector of colnames of columns that contain values,
#' * unique: unique(conds),
#' * pal: RColorBrewer::brewer.pal(3, "Dark2")[seq_len(2)]
#' @param filename A `character(1)` which is the default filename for download.
#' @param hideCols xxx
#' @param selection xxx
#'
#' @name mod_format_DT
#' 
#' @return NA
#' 
NULL



#' @importFrom shiny NS tagList
#' @import DT
#'
#' @export
#' @rdname mod_format_DT
#'
mod_format_DT_ui <- function(id) {
    pkgs.require(c('shinyjs', 'DT'))
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("dl_div"), dl_ui(ns("DL_btns")))),
    fluidRow(
      column(width = 12,
             DT::dataTableOutput(ns("StaticDataTable"))
      )
    )
  )
}

#'
#' @export
#'
#' @importFrom htmlwidgets JS
#' @import DT
#' @rdname mod_format_DT
mod_format_DT_server <- function(id,
                                 data = reactive({NULL}),
                                 dataForStyling = reactive({NULL}),
                                 withDLBtns = FALSE,
                                 showRownames = FALSE,
                                 dom = 'Bt',
                                 hc_style = reactive({NULL}),
                                 xls_style = reactive({NULL}),
                                 filename = "Prostar_export",
                                 hideCols = reactive({NULL}),
                                 selection = 'single'
                                 ){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    pkgs.require('DT')
    proxy = DT::dataTableProxy(session$ns('StaticDataTable'), session)
    
    rv <- reactiveValues(
      data = NULL,
      dataOUt = NULL
    )
    
    
    checkValidity <- reactive({
      passed <- TRUE
      
      passed <- passed && !is.null(dataForStyling())
      passed <- passed && inherits(data(), 'data.frame')
      passed <- passed && inherits(dataForStyling(), 'data.frame')
      passed <- passed && nrow(data()) == nrow(dataForStyling())
      
      passed
    })
    
    
    observe({
      req(data())
      rv$data <- data()
      
      if(!is.null(dataForStyling()) && checkValidity()){
        # Check validity of dataForStyling
        rv$data <- cbind(rv$data, dataForStyling())
      }
      
      DT::replaceData(proxy, rv$data, resetPaging = FALSE)
    })
    
    # observe({
    #   shinyjs::toggle("dl_div", condition = isTRUE(withDLBtns))
    # })
    
    # dl_server(id = "DL_btns",
    #           dataIn = reactive({rv$data}),
    #           name = reactive({filename}),
    #           excel.style = reactive({xls_style()})
    #           )
    
    
    observeEvent(input$StaticDataTable_rows_selected, {
      rv$dataOut <- input$StaticDataTable_rows_selected
    })
    
    
    output$StaticDataTable <- DT::renderDataTable(server=TRUE,{
      
      req(length(rv$data) > 0)
      .jscode <- DT::JS("$.fn.dataTable.render.ellipsis( 30 )")

      dt <- DT::datatable(
        rv$data, 
        escape = FALSE,
        selection = selection,
        rownames= showRownames,
        plugins = "ellipsis",
        options = list(
          initComplete = initComplete(),
          dom = dom,
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = '_all', className = "dt-center", render = .jscode),
            list(targets = hideCols()-1, visible = FALSE)
          )
          #ordering = FALSE
        )
      )
      
      if (!is.null(hc_style())){
        dt <- dt %>%
          DT::formatStyle(
            columns = hc_style()$cols,
            valueColumns = hc_style()$vals,
            backgroundColor = DT::styleEqual(hc_style()$unique, hc_style()$pal)
          )
      }

      dt
    })
    
    initComplete <- function(){
      return (htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
        "}"))
    }
    
    
   return(reactive({rv$dataOut}))
  })
  
}



