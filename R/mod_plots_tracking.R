#' @title   mod_plots_boxplots_ui and mod_plots_boxplots_server
#'
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param vizData internal
#' @param params A `list` of several items to give instructions to the module
#' in this it is run in slave mode:
#' * xxxx: xxxxx
#' * xxxx: xxxxx
#' * xxxx: xxxxx
#' * xxxx: xxxxx
#' * xxxx: xxxxx
#' 
#' @param reset internal
#' @param slave A `boolean(1)` which indicates whether the module is run in a
#' slave mode or in an autonomous mode (default)
#'
#' @example examples/ex_mod_plots_tracking.R
#' 
NULL



#' @rdname mod_plots_tracking
#'
#' @keywords internal
#'
#' @export
#'
#' @importFrom shiny NS tagList
#'
mod_plots_tracking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    selectInput(ns("typeSelect"), "Type of selection",
                choices = character(0),
                selected = character(0),
                width = "130px"),
    
    shinyjs::hidden(
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
                                      params = reactive({NULL}),
                                      reset = FALSE){
  default.dataOut <- list(
    type = character(0),
    names = NULL, 
    indices = NULL)
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    rv.track <- reactiveValues(
      dataOut = default.dataOut
      )
  
  
  observe({
    stopifnot(inherits(vizData(), "VizData"))
    
    })
  
  
  
  Get_LogicalCols_in_Dataset <- reactive({
    logical.cols <- lapply(colnames(vizData()@metadata),
                           function(x)
                             is.logical((vizData()@metadata)[, x])
                           )
    logical.cols <- which(unlist(logical.cols))
    logical.cols
  })
  
  
  observeEvent(input$typeSelect, {
    rv.track$dataOut$type <- input$typeSelect
    shinyjs::toggle("listSelect", condition = input$typeSelect == "List")
    shinyjs::toggle("randSelect", condition = input$typeSelect == "Random")
    shinyjs::toggle("colSelect", condition = input$typeSelect == "Column" && length(Get_LogicalCols_in_Dataset()) > 0)
  })
  
  
  
  # observeEvent(req(reset()), ignoreInit = TRUE, {
  # 
  #     updateSelectInput(session, "typeSelect", selected = "None")
  #     updateSelectInput(session, "listSelect", NULL)
  #     updateSelectInput(session, "randSelect", selected = "")
  #     updateSelectInput(session, "colSelect", selected = NULL)
  #     
  #     rv.track$dataOut <- default.dataOut
  # })
  
  
  
  observeEvent(params(), ignoreInit = FALSE, ignoreNULL = FALSE, {
    #if (is.null(params())){
      
    
    
    if (length(Get_LogicalCols_in_Dataset()) > 0)
      updateSelectInput(session, "typeSelect", 
                      choices = c("None" = "None", "Protein list" = "List",
                                  "Random" = "Random", "Column" = "Column"))
    else
      updateSelectInput(session, "typeSelect", 
                        choices = c("None" = "None", "Protein list" = "List",
                                    "Random" = "Random"))
      
      updateSelectizeInput(session, "listSelect",
                           choices = (vizData()@metadata)[, vizData()@colID], 
                           selected = character(0),
                           server=TRUE)
    
      updateSelectInput(session, "randSelect", 
                        selected = character(0))
      
      
      updateSelectInput(session, "colSelect",
                        choices = Get_LogicalCols_in_Dataset(),
                        selected = character(0))
    
    #} else 
    if (!is.null(params())){
      updateSelectInput(session, "typeSelect", selected = params()$type)
      switch(params()$type,
             list = updateSelectizeInput(session, "listSelect", selected = params()$names),
             random = updateSelectInput(session, "randSelect", selected = params()$names),
             column = updateSelectInput(session, "colSelect", selected = params()$names)
             )

    }
  })
  

  
  
  
  
  
  
  observeEvent(req(length(input$listSelect) > 0), ignoreNULL = FALSE, {
    req(is.null(params()))
    
    rv.track$dataOut$names <- input$listSelect
    rv.track$dataOut$indices <- match(input$listSelect, (vizData()@metadata)[, vizData()@colID])

    updateSelectInput(session, "randSelect", selected = "")
    updateSelectInput(session, "colSelect", selected = NULL)
    })
  
  
  
  
  
  observeEvent(req(!is.null(input$randSelect) && input$randSelect != ""), ignoreNULL = FALSE, {
    
    rv.track$dataOut$indices <- sample(1:nrow(vizData()@metadata), as.numeric(input$randSelect), replace = FALSE)
    rv.track$dataOut$names <- (vizData()@metadata)[rv.track$dataOut$indices, vizData()@colID]

    updateSelectInput(session, "listSelect", selected = "")
    updateSelectInput(session, "colSelect", selected = NULL)
    })
  
  
  

  observeEvent(req(input$colSelect), {
    rv.track$dataOut$indices <- sample(1:nrow(vizData()@metadata), as.numeric(input$randSelect), replace = FALSE)
    rv.track$dataOut$names <- (vizData()@metadata)[rv.track$dataOut$indices, vizData()@colID]
    
    updateSelectInput(session, "listSelect", selected = "")
    updateSelectInput(session, "randSelect", selected = NULL)
    })
  
  
  return(reactive({rv.track$dataOut}))
})
}
