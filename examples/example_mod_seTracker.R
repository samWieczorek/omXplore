

    ui <- tagList(
        mod_seTracker_ui("track"),
        uiOutput("show")
    )

    server <- function(input, output, session) {
        tmp <- reactiveVal()
        data(ft, package='DaparViz')
        obj <- ft[[1]]
        tmp <- mod_seTracker_server(id = "track", 
                                    mdata = reactive({rowData(obj)}),
                                    colID = idcol(obj))

        output$show <- renderUI({
            p(paste0(tmp(), collapse = " "))
        })
    }
    shinyApp(ui = ui, server = server)
