#' @title mod_plots_cc_ui and mod_plots_cc_server
#'
#' @description  A shiny Module.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param cc xxx
#'
#' @keywords internal
#'
#' @return NA
#'
#' @name connected-components
#'
#' @examples
#'
#' xxx
#'
NULL



#' @importFrom shiny NS tagList
#' @importFrom shinyjs toggle hidden
#' @import shinyBS
#'
#' @export
#' @rdname connected_components
mod_ds_cc_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Peptide-Protein Graph",
           value = "graphTab",
           tabsetPanel(id = "graphsPanel",
             tabPanel("One-One Connected Components",
               tagList(
                 fluidRow(
                   column(width = 4, tagList(
                     mod_download_btns_ui(ns("OneOneDT_DL_btns")),
                     DT::dataTableOutput(ns("OneOneDT"))
                   )),
                   column(width = 8, tagList(
                     # uiOutput(ns('OneOneDTDetailed_ui')),
                     DT::dataTableOutput(ns("OneOneDTDetailed"))
                   ))
                 )
               )
             ),
             tabPanel("One-Multi Connected Components",
               tagList(
                 fluidRow(
                   column(width = 4,
                     tagList(
                       mod_download_btns_ui(ns("OneMultiDT_DL_btns")),
                       DT::dataTableOutput(ns("OneMultiDT"))
                     )
                   ),
                   column(width = 8,
                     tagList(
                       DT::dataTableOutput(ns("OneMultiDTDetailed"))
                     )
                   )
                 )
               )
             ),
             tabPanel("Multi-Multi Connected Components",
               tagList(
                 uiOutput(ns("pepInfo_ui")),
                 selectInput(ns("searchCC"), "Search for CC",
                             choices = c(
                               "Tabular view" = "tabular",
                               "Graphical view" = "graphical"
                             ),
                             width = "150px"
                 ),
                 fluidRow(
                   column(width = 6, tagList(
                     highchartOutput(ns("jiji")),
                     uiOutput(ns("CCMultiMulti_DL_btns_ui")),
                     shinyjs::hidden(
                       dataTableOutput(ns("CCMultiMulti"))
                     )
                   )),
                   column(width = 6, tagList(
                     visNetworkOutput(ns("visNet_CC"), height = "600px")
                   ))
                 ),
                 uiOutput(ns("CCDetailed"))
               )
             )
           )
  )
}



#' @importFrom tibble tibble
#' @importFrom shinyjs toggle hidden
#' @export
#' @rdname connected_components
mod_ds_cc_server <- function(id, vizData) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      rvCC <- reactiveValues(
        ## selected CC in global CC list (tab or plot)
        selectedCC = NULL,
        selectedNode = NULL,
        selectedNeighbors = NULL,
        selectedCCgraph = NULL,
        
        # when the user selects a node in the graph
        detailedselectedNode = list(
          sharedPepLabels = NULL,
          specPepLabels = NULL,
          protLabels = NULL
        )
      )
      
      observeEvent(req(input$searchCC), {
        shinyjs::toggle("jiji", condition = input$searchCC == "graphical")
        shinyjs::toggle("CCMultiMulti", condition = input$searchCC == "tabular")
      })
      
      
      output$pepInfo_ui <- renderUI({
        selectInput(ns("pepInfo"), "Peptide Info",
                    choices = colnames(vizData()@metadata),
                    multiple = TRUE)
      })
      
      
      # select a point in the grpah
      observeEvent(input$click, { rvCC$selectedNode <- input$click})
      
      
      # Get the id of selected neighbors in the graph
      observeEvent(input$visNet_CC_highlight_color_id, {
        rvCC$selectedNeighbors <- input$visNet_CC_highlight_color_id
      })
      
      
      # select a CC in the summary table
      observeEvent(input$CCMultiMulti_rows_selected, {
        rvCC$selectedCC <- input$CCMultiMulti_rows_selected
      })
      
      # select a CC in the jitter plot
      observeEvent(req(input$eventPointClicked), {
        .str <- strsplit(input$eventPointClicked, "_")
        this.index <- as.integer(.str[[1]][1])
        this.index + 1
        rvCC$selectedCC <- this.index + 1
      })
      
      
      output$visNet_CC <- renderVisNetwork({
        req(rvCC$selectedCC)
        .select <- rvCC$selectedCC
        local <- (vizData()@cc)[Get_CC_Multi2Any()]
        Xshared <- DAPAR::GetMatAdj(obj())$matWithSharedPeptides
        rvCC$selectedCCgraph <- buildGraph(local[[.select]], Xshared)
        
        display.CC.visNet(rvCC$selectedCCgraph) %>%
          visEvents(click = paste0(
            "function(nodes){
                Shiny.onInputChange('",
            ns("click"), "', nodes.nodes[0]);
                Shiny.onInputChange('",
            ns("node_selected"), "', nodes.nodes.length);
                ;}"
          )) %>%
          visOptions(highlightNearest = TRUE)
      })
      
      
      output$jiji <- renderHighchart({
        tooltip <- NULL
        
        isolate({
          local <- (vizData()@cc)[Get_CC_Multi2Any()]
          n.prot <- unlist(lapply(local, function(x) {length(x$proteins)}))
          n.pept <- unlist(lapply(local, function(x) {length(x$peptides)}))
          df <- tibble::tibble(
            x = jitter(n.pept),
            y = jitter(n.prot),
            index = 1:length(local)
          )
          
          if (!is.null(tooltip)) {
            df <- cbind(df, vizData@metadata[tooltip])
          }
          
          colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
          if (ncol(df) > 3) {
            colnames(df)[4:ncol(df)] <-
              paste("tooltip_", colnames(df)[4:ncol(df)], sep = "")
          }
          
          clickFun <-
            JS(paste0(
              "function(event) {Shiny.onInputChange('",
              ns("eventPointClicked"),
              "', [this.index]+'_'+ [this.series.name]);}"
            ))
          
          rv$tempplot$plotCC <- plotJitter_rCharts(df,
                                                   clickFunction = clickFun
          )
        })
        rv$tempplot$plotCC
      })
      
      
      GetDataFor_CCMultiMulti <- reactive({
        Get_CC_Multi2Any()
        
        df <- cbind(
          id = 1:length(Get_CC_Multi2Any()),
          nProt = cbind(lapply(
            (vizData()@cc)[Get_CC_Multi2Any()],
            function(x) {
              length(x$proteins)
            }
          )),
          nPep = cbind(lapply(
            (vizData()@cc)[Get_CC_Multi2Any()],
            function(x) {
              length(x$peptides)
            }
          )),
          proteins = cbind(lapply(
            (vizData()@cc)[Get_CC_Multi2Any()],
            function(x) {
              paste(x$proteins, collapse = ",")
            }
          )),
          peptides = cbind(lapply(
            (vizData()@cc)[Get_CC_Multi2Any()],
            function(x) {
              paste(x$proteins, collapse = ",")
            }
          ))
        )
        
        colnames(df) <- c(
          "id",
          "nProt",
          "nPep",
          "Proteins Ids",
          "Peptides Ids"
        )
        
        df
      })
      
      
      
      output$CCMultiMulti_DL_btns_ui <- renderUI({
        req(input$searchCC == "tabular")
        mod_download_btns_ui(ns("CCMultiMulti_DL_btns"))
      })
      
      mod_download_btns_server("CCMultiMulti_DL_btns",
                               df.data = reactive({
                                 GetDataFor_CCMultiMulti()
                               }),
                               name = reactive({
                                 "CC_MultiMulti"
                               }),
                               colors = reactive({
                                 NULL
                               }),
                               df.tags = reactive({
                                 NULL
                               })
      )
      
      
      
      
      output$CCMultiMulti <- DT::renderDataTable(server = TRUE, {
        dat <- DT::datatable(GetDataFor_CCMultiMulti(),
                             selection = "single",
                             rownames = FALSE,
                             extensions = c("Scroller"),
                             options = list(
                               initComplete = initComplete(),
                               dom = "frt",
                               # deferRender = TRUE,
                               # bLengthChange = FALSE,
                               scrollX = 400,
                               scrollY = 400,
                               displayLength = 10,
                               scroller = TRUE
                               # orderClasses = TRUE,
                               # autoWidth=TRUE
                             )
        )
        
        return(dat)
      })
      
      
      
      observeEvent(c(
        rvCC$selectedNeighbors,
        input$node_selected,
        rvCC$selectedCCgraph
      ), {
        local <- (vizData()@cc)[Get_CC_Multi2Any()]
        rvCC$selectedNeighbors
        
        nodes <- rvCC$selectedCCgraph$nodes
        
        if (!is.null(input$node_selected) && input$node_selected == 1) {
          sharedPepIndices <- intersect(
            rvCC$selectedNeighbors,
            which(nodes[, "group"] == "shared.peptide")
          )
          specPepIndices <- intersect(
            rvCC$selectedNeighbors,
            which(nodes[, "group"] == "spec.peptide")
          )
          protIndices <- intersect(
            rvCC$selectedNeighbors,
            which(nodes[, "group"] == "protein")
          )
        } else {
          .shared <- "shared.peptide"
          .spec <- "spec.peptide"
          sharedPepIndices <- which(nodes[, "group"] == .shared)
          specPepIndices <- which(nodes[, "group"] == .spec)
          protIndices <- which(nodes[, "group"] == "protein")
        }
        
        .shared <- nodes[sharedPepIndices, "label"]
        .specs <- nodes[specPepIndices, "label"]
        .ind <- nodes[protIndices, "label"]
        rvCC$detailedselectedNode$sharedPepLabels <- .shared
        rvCC$detailedselectedNode$specPepLabels <- .specs
        rvCC$detailedselectedNode$protLabels <- .ind
      })
      
      
      output$CCDetailed <- renderUI({
        req(rvCC$detailedselectedNode)
        req(rvCC$selectedCC)
        # browser()
        tagList(
          h4("Proteins"),
          dataTableOutput(ns("CCDetailedProt")),
          h4("Specific peptides"),
          dataTableOutput(ns("CCDetailedSpecPep")),
          h4("Shared peptides"),
          dataTableOutput(ns("CCDetailedSharedPep"))
        )
      })
      
      output$CCDetailedProt <- DT::renderDataTable(server = TRUE, {
        req(rvCC$selectedCC)
        rvCC$detailedselectedNode
        if (is.null(rvCC$detailedselectedNode$protLabels)) {
          return(NULL)
        }
        .protLabels <- rvCC$detailedselectedNode$protLabels
        
        df <- data.frame(proteinId = unlist(.protLabels))
        colnames(df) <- c("Proteins Ids")
        dt <- DT::datatable(df,
                            extensions = c("Scroller"),
                            options = list(
                              initComplete = initComplete(),
                              dom = "rt",
                              blengthChange = FALSE,
                              ordering = FALSE,
                              scrollX = 400,
                              scrollY = 100,
                              displayLength = 10,
                              scroller = TRUE,
                              header = FALSE,
                              server = FALSE
                            )
        )
        dt
      })
      
      
      
      #######
      
      output$CCDetailedSharedPep <- DT::renderDataTable(server = TRUE, {
        rvCC$detailedselectedNode
        input$pepInfo
        
        req(rvCC$detailedselectedNode$sharedPepLabels)
        
        
        ind <- 1:ncol(vizData()@qdata)
        data <- getDataForExprs(vizData(), rv$settings_nDigits)
        .n <- ncol(data)
        pepLine <- rvCC$detailedselectedNode$sharedPepLabels
        indices <- unlist(lapply(pepLine, function(x) {
          which(rownames(data) == x)
        }))
        data <- data[indices, c(ind, (ind + .n / 2))]
        
        if (!is.null(input$pepInfo)) {
          data <- cbind(data, (vizData()@metadata)[pepLine, input$pepInfo])
          colnames(data)[(1 + .n - length(input$pepInfo)):.n] <- input$pepInfo
        }
        
        offset <- length(input$pepInfo)
        c.tags <- BuildColorStyles(vizData())$tags
        c.colors <- BuildColorStyles(vizData())$colors
        
        dt <- DT::datatable(data,
                            extensions = c("Scroller"),
                            options = list(
                              initComplete = initComplete(),
                              dom = "rt",
                              blengthChange = FALSE,
                              ordering = FALSE,
                              scrollX = 400,
                              scrollY = 150,
                              displayLength = 10,
                              scroller = TRUE,
                              header = FALSE,
                              server = FALSE,
                              columnDefs = list(
                                list(
                                  targets = c(
                                    (((.n - offset) / 2) + 1):(.n - offset)
                                  ),
                                  visible = FALSE
                                )
                              )
                            )
        ) %>%
          DT::formatStyle(
            colnames(data)[1:((.n - offset) / 2)],
            colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
            backgroundColor = DT::styleEqual(c.tags, c.colors)
          )
        
        dt
      })
      
      
      
      
      
      ##### -----------
      output$CCDetailedSpecPep <- DT::renderDataTable(server = TRUE, {
        rvCC$detailedselectedNode
        input$pepInfo
        req(rvCC$detailedselectedNode$specPepLabels)
        
        ind <- 1:ncol(vizData()@qdata)
        data <- getDataForExprs(vizData(), rv$settings_nDigits)
        .n <- ncol(data)
        pepLine <- rvCC$detailedselectedNode$specPepLabels
        indices <- unlist(lapply(
          pepLine,
          function(x) {which(rownames(data) == x)}
        ))
        data <- data[indices, c(ind, (ind + .n / 2))]
        
        if (!is.null(input$pepInfo)) {
          data <- cbind(data, (vizData()@metadata)[pepLine, input$pepInfo])
          colnames(data)[(1 + .n - length(input$pepInfo)):.n] <- input$pepInfo
        }
        
        offset <- length(input$pepInfo)
        
        c.tags <- BuildColorStyles(vizData())$tags
        c.colors <- BuildColorStyles(vizData())$colors
        
        dt <- DT::datatable(data,
                            extensions = c("Scroller"),
                            options = list(
                              initComplete = initComplete(),
                              dom = "rt",
                              blengthChange = FALSE,
                              ordering = FALSE,
                              scrollX = 400,
                              scrollY = 100,
                              displayLength = 10,
                              scroller = TRUE,
                              header = FALSE,
                              server = FALSE,
                              columnDefs = list(
                                list(
                                  targets = c(
                                    (((.n - offset) / 2) + 1):(.n - offset)
                                  ),
                                  visible = FALSE
                                )
                              )
                            )
        ) %>%
          DT::formatStyle(
            colnames(data)[1:((.n - offset) / 2)],
            colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
            backgroundColor = DT::styleEqual(c.tags, c.colors)
          )
        
        dt
      })
      
      
      
      
      
      
      Get_CC_One2One <- reactive({
        vizData()@cc
        ll.prot <- lapply(vizData()@cc, function(x) {ncol(x)})
        ll.pept <- lapply(vizData()@cc, function(x) {nrow(x)})
        ll.prot.one2one <- intersect(which(ll.prot == 1), which(ll.pept == 1))
        ll.prot.one2one
      })
      
      
      
      Get_CC_One2multi <- reactive({
        vizData()@cc
        ll.prot <- lapply(vizData()@cc, function(x) {ncol(x)})
        ll.pept <- lapply(vizData()@cc, function(x) {nrow(x)})
        ll.prot.one2multi <- intersect(which(ll.prot == 1),  which(ll.pept > 1))
        ll.prot.one2multi
      })
      
      
      Get_CC_Multi2Any <- reactive({
        vizData()@cc
        ll.prot <- lapply(vizData()@cc, function(x) {ncol(x)})
        ll.pept <- lapply(vizData()@cc, function(x) {nrow(x)})
        ll.prot.multi2any <- which(ll.prot > 1)
        ll.prot.multi2any
      })
      
      
      
      BuildOne2OneTab <- reactive({
        vizData()@cc
        df <- cbind(
          cbind(
            lapply((vizData()@cc)[Get_CC_One2One()], function(x) {colnames(x)})),
          cbind(
            lapply((vizData()@cc)[Get_CC_One2One()], function(x) {rownames(x)}))
        )
        
        colnames(df) <- c("proteins", "peptides")
        
        df
      })
      
      BuildOne2MultiTab <- reactive({
        vizData()@cc
        
        df <- cbind(
          proteins = cbind(lapply((vizData()@cc)[Get_CC_One2multi()], function(x) {colnames(x)})),
          nPep = cbind(lapply((vizData()@cc)[Get_CC_One2multi()], function(x) {nrow(x)})),
          peptides = cbind(lapply((vizData()@cc)[Get_CC_One2multi()],function(x) {paste(rownames(x), collapse = ",")}))
        )
        colnames(df) <- c("proteins", "nPep", "peptides")
        
        df
      })
      
      
      BuildMulti2AnyTab <- reactive({
        vizData()@cc
        df <- cbind(id = 1:length(Get_CC_Multi2Any()),
          proteins = cbind(lapply((vizData()@cc)[Get_CC_Multi2Any()], function(x) {colnames(x)})),
          nProt = cbind(lapply((vizData()@cc)[Get_CC_Multi2Any()], function(x) {ncol(x)})),
          nPep = cbind(lapply((vizData()@cc)[Get_CC_Multi2Any()], function(x) {nrow(x)})),
          peptides = cbind(lapply((vizData()@cc)[Get_CC_Multi2Any()], function(x) {paste(rownames(x), collapse = ",")}))
        )
        colnames(df) <- c("proteins", "nPep", "peptides")
        
        df
      })
      
      
      
      mod_download_btns_server("OneMultiDT_DL_btns",
                               df.data = reactive({
                                 df <- BuildOne2MultiTab()
                                 colnames(df) <- c("Proteins Ids", "nPep", "Peptides Ids")
                                 df
                               }),
                               name = reactive({"CC_OneMulti"}),
                               colors = reactive({NULL}),
                               df.tags = reactive({NULL})
                               )
      
      
      output$OneMultiDT <- DT::renderDataTable(server = TRUE, {
        req(vizData()@cc)
        df <- BuildOne2MultiTab()
        colnames(df) <- c("Proteins Ids", "nPep", "Peptides Ids")
        
        dat <- DT::datatable(df,
                             selection = "single",
                             rownames = FALSE,
                             extensions = c("Scroller"),
                             options = list(
                               initComplete = initComplete(),
                               dom = "rt",
                               deferRender = TRUE,
                               bLengthChange = TRUE,
                               displayLength = 10,
                               scrollX = 400,
                               scrollY = 400,
                               scroller = TRUE,
                               autoWidth = FALSE,
                               columns.searchable = FALSE,
                               columnDefs = list(list(
                                 width = c("60px"),
                                 targets = c(list(0), list(1))
                               ))
                             )
        )
        
        return(dat)
      })
      
      
      
      GetDataFor_OneMultiDTDetailed <- reactive({
        input$pepInfo
        req(input$OneMultiDT_rows_selected)
        
        line <- input$OneMultiDT_rows_selected
        ind <- 1:ncol(vizData()@qdata)
        data <- getDataForExprs(vizData(), rv$settings_nDigits)
        .n <- ncol(data)
        .pep <- input$pepInfo
        pepLine <- unlist(strsplit(unlist(BuildOne2MultiTab()[line, "peptides"]), split = ","))
        
        indices <- unlist(lapply(pepLine, function(x) {which(rownames(data) == x)}))
        
        data <- data[indices, c(ind, (ind + .n / 2))]
        
        if (!is.null(.pep)) {
          data <- cbind(data, (vizData()@metadata)[pepLine, .pep])
          colnames(data)[(1 + .n - length(.pep)):.n] <- .pep
        }
        
        data
      })
      
      
      output$OneMultiDTDetailed <- DT::renderDataTable(server = TRUE, {
        input$pepInfo
        req(input$OneMultiDT_rows_selected)
        
        data <- GetDataFor_OneMultiDTDetailed()
        .n <- ncol(data)
        offset <- length(input$pepInfo)
        
        c.tags <- BuildColorStyles(vizData())$tags
        c.colors <- BuildColorStyles(vizData())$colors
        
        dt <- DT::datatable(data,
                            extensions = c("Scroller"),
                            options = list(
                              initComplete = initComplete(),
                              dom = "frtip",
                              pageLength = 10,
                              blengthChange = FALSE,
                              displayLength = 10,
                              ordering = FALSE,
                              header = FALSE,
                              server = FALSE,
                              columnDefs = list(
                                list(
                                  targets = c(
                                    (((.n - offset) / 2) + 1):(.n - offset)
                                  ),
                                  visible = FALSE
                                )
                              )
                            )
        ) %>%
          DT::formatStyle(
            colnames(data)[1:((.n - offset) / 2)],
            colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
            backgroundColor = DT::styleEqual(c.tags, c.colors)
          )
        
        dt
      })
      
      
      
      
      mod_download_btns_server("OneOneDT_DL_btns",
                               df.data = reactive({
                                 df <- BuildOne2OneTab()
                                 colnames(df) <- c("Proteins Ids", "Peptides Ids")
                                 df
                               }),
                               name = reactive({"CC_OneOne"}),
                               colors = reactive({NULL}),
                               df.tags = reactive({NULL})
                               )
      
      
      output$OneOneDT <- DT::renderDataTable(server = TRUE, {
        req(vizData()@cc)
        df <- BuildOne2OneTab()
        colnames(df) <- c("Proteins Ids", "Peptides Ids")
        dat <- DT::datatable(df,
                             selection = "single",
                             rownames = FALSE,
                             extensions = c("Scroller"),
                             options = list(
                               initComplete = initComplete(),
                               dom = "frtip",
                               deferRender = TRUE,
                               bLengthChange = FALSE,
                               scrollX = 400,
                               scrollY = 200,
                               scroller = TRUE,
                               orderClasses = TRUE,
                               autoWidth = FALSE,
                               columns.searchable = F,
                               columnDefs = list(list(
                                 width = c("60px"),
                                 targets = c(list(0), list(1), list(2))
                               ))
                             )
        )
        
        return(dat)
      })
      
      
      
      GetDataFor_OneOneDTDetailed <- reactive({
        req(vizData()@cc)
        req(input$OneOneDT_rows_selected)
        input$pepInfo
        .pep <- input$pepInfo
        line <- input$OneOneDT_rows_selected
        ind <- 1:ncol(vizData()@qdata)
        data <- getDataForExprs(vizData(), rv$settings_nDigits)
        .n <- ncol(data)
        
        pepLine <- BuildOne2OneTab()[line, 2]
        
        indices <- unlist(lapply(pepLine, function(x) {
          which(rownames(data) == x)
        }))
        data <- data[indices, c(ind, (ind + .n / 2))]
        if (!is.null(.pep)) {
          data <- cbind(data, (vizData()@metadata)[pepLine, .pep])
          colnames(data)[(1 + .n - length(.pep)):.n] <- .pep
        }
        
        data
      })
      
      
      output$OneOneDTDetailed <- DT::renderDataTable(server = TRUE, {
        req(vizData()@cc)
        req(input$OneOneDT_rows_selected)
        data <- GetDataFor_OneOneDTDetailed()
        .n <- ncol(data)
        offset <- length(input$pepInfo)
        
        c.tags <- BuildColorStyles(vizData())$tags
        c.colors <- BuildColorStyles(vizData())$colors
        
        dt <- DT::datatable(data,
                            extensions = c("Scroller"),
                            options = list(
                              initComplete = initComplete(),
                              dom = "frtip",
                              blengthChange = FALSE,
                              pageLength = 10,
                              displayLength = 10,
                              ordering = FALSE,
                              header = FALSE,
                              server = FALSE,
                              columnDefs = list(
                                list(
                                  targets = c(
                                    (((.n - offset) / 2) + 1):(.n - offset)
                                  ),
                                  visible = FALSE
                                )
                              )
                            )
        ) %>%
          DT::formatStyle(
            colnames(data)[1:((.n - offset) / 2)],
            colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
            backgroundColor = DT::styleEqual(c.tags, c.colors)
          )
        
        dt
      })
    }
  )
}
