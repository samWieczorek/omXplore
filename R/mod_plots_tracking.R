#' @title mod_plots_tracking_ui and mod_plots_tracking_server
#'
#' @description This shiny module provides a tool to select
#'
#' @param id shiny id
#' @param vizData internal
#' @param params A `list` of three items to give instructions to the module
#' in this it is run in slave mode:
#' * type: xxxxx
#' * names: xxxxx
#' * indices: xxxxx
#' @param resetBtn A `boolean(1)` which indicates whether to show the 'Reset' button 
#' or not.
#' 
#' @return A `list` (same structure as the parameter 'params")
#'
#' @example inst/extdata/examples/ex_mod_plots_tracking.R
#' 
#' @name mod_plots_tracking
#' 
NULL



#' @rdname mod_plots_tracking
#' @export
#' @importFrom shiny NS tagList
#'
mod_plots_tracking_ui <- function(id) {
  pkgs.require('shinyjs')
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    hidden(div(id = ns('badFormatMsg'), h3(bad_format_txt))),
    hidden(actionButton(ns('reset'), "Reset")),
    hidden(selectInput(ns("typeSelect"), "Type of selection",
                choices = character(0),
                selected = character(0),
                width = "130px")),
    
    hidden(
      selectizeInput(inputId = ns("listSelect"),
                   label = "Select protein",
                   choices = character(0),
                   width = "400px",
                   multiple = TRUE,
                   options = list(maxOptions = 10000))
    ),
    
    shinyjs::hidden(
      textInput(ns("randSelect"), "Random", width = "120px")
      ),
    
    shinyjs::hidden(
      selectInput(ns("colSelect"), "Column", choices = character(0)))
  )
}


#' @rdname mod_plots_tracking
#'
#' @export
#' @keywords internal
#' @import shinyjs
#'
mod_plots_tracking_server <- function(id, 
                                      vizData = reactive({NULL}),
                                      resetBtn = reactive({FALSE})){
   
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv.track <- reactiveValues(
      type = character(0),
      data = NULL)
    
    
    dataOut <- reactiveValues(
      indices = NULL
      )
  
  
  observe({
    if(inherits(vizData(), "VizData"))
      rv.track$data <- vizData()

    shinyjs::toggle('badFormatMsg', condition = is.null(rv.track$data))
    shinyjs::toggle('typeSelect', condition = !is.null(rv.track$data))
  }, priority = 1000)
  
  
  
  # observe({
  #   req(rv.track$data)
  #   shinyjs::toggle('reset', condition = isTRUE(resetBtn()))
  #   
  #   
  # })
  
  
  observe({
    req(rv.track$data)
    
    shinyjs::toggle('reset', condition = isTRUE(resetBtn()))
    
    
    if (length(Get_LogicalCols_in_Dataset()) > 0)
      updateSelectInput(session, "typeSelect", 
                        choices = c("None" = "None", "Protein list" = "List",
                                    "Random" = "Random", "Column" = "Column"))
    else
      updateSelectInput(session, "typeSelect", 
                        choices = c("None" = "None", 
                                    "Protein list" = "List",
                                    "Random" = "Random"))
    
    if(!is.null(rv.track$data@colID) && 
       rv.track$data@colID != "" && 
       length(rv.track$data@colID) > 0)
    updateSelectizeInput(session, "listSelect",
                         choices = (rv.track$data@metadata)[, rv.track$data@colID], 
                         selected = character(0),
                         server=TRUE)
    
    updateSelectInput(session, "randSelect", 
                      selected = character(0))
    
    
    updateSelectInput(session, "colSelect",
                      choices = Get_LogicalCols_in_Dataset(),
                      selected = character(0))
    })
  
  
  
  Get_LogicalCols_in_Dataset <- reactive({
    logical.cols <- lapply(colnames(rv.track$data@metadata),
                           function(x)
                             is.logical((rv.track$data@metadata)[, x])
                           )
    logical.cols <- which(unlist(logical.cols))
    logical.cols
  })
  
  
  observeEvent(input$typeSelect, {
    rv.track$type <- input$typeSelect
    shinyjs::toggle("listSelect", condition = input$typeSelect == "List")
    shinyjs::toggle("randSelect", condition = input$typeSelect == "Random")
    shinyjs::toggle("colSelect", condition = input$typeSelect == "Column" && length(Get_LogicalCols_in_Dataset()) > 0)
    
    updateSelectInput(session, "listSelect", selected = "")
    updateSelectInput(session, "randSelect", selected = "")
    updateSelectInput(session, "colSelect", selected = NULL)
    
   if (input$typeSelect == 'None')
    RunReset()
  })
  
  
  RunReset <- function(){
    updateSelectInput(session, "typeSelect", selected = "None")
    updateSelectInput(session, "listSelect", NULL)
    updateSelectInput(session, "randSelect", selected = "")
    updateSelectInput(session, "colSelect", selected = NULL)
    
    rv.track$type <- character(0)
    dataOut$indices <- NULL
  }
  
  observeEvent(input$reset, ignoreInit = TRUE, ignoreNULL = TRUE, {
    RunReset()
  })
  
  
  observeEvent(req(length(input$listSelect) > 0), ignoreNULL = FALSE, {
    
    dataOut$indices <- match(input$listSelect, 
                             (rv.track$data@metadata)[, rv.track$data@colID])
    })

  
  observeEvent(req(!is.null(input$randSelect) && input$randSelect != ""), ignoreNULL = FALSE, {
    
    dataOut$indices <- sample(1:nrow(rv.track$data@metadata), 
                              as.numeric(input$randSelect), 
                              replace = FALSE)
    })
  
  
  

  observeEvent(req(input$colSelect), {
    dataOut$indices <- which((rv.track$data@metadata)[, input$colSelect] == TRUE)
    })
  
  
  return(reactive({dataOut$indices}))
})
}
