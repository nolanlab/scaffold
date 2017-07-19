render_editing_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(6,
                   selectInput("editingui_scaffold_file", "Choose a Scaffold file:", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$")), width = "100%"),
                   selectInput("editingui_active_graph", "Active graph:", choices = c("All"), width = "100%"),
                   #selectInput("editingui_columns_to_remove", "Columns to remove:", choices = c(""), multiple = T, width = "100%"),
                   selectInput("editingui_columns_to_add", "Columns to add:", choices = c("", list.files(path = working.directory, pattern = "*.txt$")), width = "100%"),
                   verbatimTextOutput("editingui_status"),
                   actionButton("editingui_process", "Process")
            )
        )
    )
    
    
})}

editingui_scaffold_data <- reactive({
    file_name <- input$editingui_scaffold_file
    if(!is.null(file_name) && file_name != "")
    {
        file_name <- file.path(working.directory, file_name)
        data <- scaffold:::my_load(file_name)
        updateSelectInput(session, "editingui_active_graph", choices = c("All", names(data$graphs)))
        return(data)
    }
    else
        return(NULL)
})

#observe({
#    data <- editingui_scaffold_data()
#    if(!is.null(data))
#    {
#        G <- NULL
#        if(input$editingui_active_graph == "All")
#            G <- data$graphs[[1]]
#        else
#            G <- data$graphs[[input$editingui_active_graph]]
#        updateSelectInput(session, "editingui_columns_to_remove", choices = igraph::list.vertex.attributes(G))
#    }
#})


output$editingui_status <- renderText({
    if(!is.null(input$editingui_process) && input$editingui_process != 0)
    {
        out_name <- scaffold:::process_scaffold_edits(working.directory, input$editingui_scaffold_file, editingui_scaffold_data(),
            input$editingui_active_graph, input$editingui_columns_to_add, c())#input$editingui_columns_to_remove)
        updateSelectInput(session, "graphui_dataset", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$")))
        return(sprintf("Created output file %s", out_name))
    }
})