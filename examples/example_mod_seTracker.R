

    ui <- tagList(
        mod_seTracker_ui("track"),
        uiOutput("show")
    )

    server <- function(input, output, session) {
        tmp <- reactiveVal()
        data(ft, package='DaparViz')
        vizData <- Coerce2VizData(qf, 1)
        tmp <- mod_seTracker_server(id = "track", 
                                    vizData = reactive({vizData}))

        output$show <- renderUI({
            p(paste0(tmp(), collapse = " "))
        })
    }
    shinyApp(ui = ui, server = server)
