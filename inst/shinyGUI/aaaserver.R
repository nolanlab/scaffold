reactiveNetwork <- function (outputId)
{
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

row <- function(...)
{
    tags$div(class="row", ...)
}

col <- function(width, ...)
{
    tags$div(class=paste0("span", width), ...)
}


busy_dialog <- function(start.string, end.string)
{
    conditionalPanel(
        condition <- "$('html').hasClass('shiny-busy')",
        br(),
        p(strong("Processing data...please wait."))
    )
}


render_unsupervised_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(6,
                selectInput("unsupervisedui_file_for_markers", "Load marker names from file", choices = c("", list.files(path = working.directory, pattern = "*.clustered.txt$")), width = "100%"),
                selectInput("unsupervisedui_markers", "Choose the markers to use for the anlaysis", choices = c(""), multiple = T, width = "100%"),
                selectInput("unsupervisedui_files_list", label = "Files to include", choices = c(list.files(path = working.directory, pattern = "*.clustered.txt$")),
                               multiple = T, width = "100%"),
                numericInput("unsupervisedui_filtering_threshold", "Edge filtering threshold", 10, min = 1),
                actionButton("unsupervisedui_start_analysis", "Start analysis"), br(), br(), br(), br(), br(), br()
            ),
            column(12,
                verbatimTextOutput("unsupervisedui_dialog")
            )
        )
    )
})}





shinyServer(function(input, output, session)
{
    options(shiny.error=traceback) 
    if(exists(".ScaffoldWorkingDir"))
        working.directory <- .ScaffoldWorkingDir
    else
        working.directory <- dirname(file.choose())
    output$graphUI <- render_graph_ui(working.directory, input, output, session)
    output$analysisUI <- render_analysis_ui(working.directory, input, output, session)
    output$clusteringUI <- render_clustering_ui(working.directory, input, output, session)
    output$mappingUI <- render_mapping_ui(working.directory, input, output, session)
    output$editingUI <- render_editing_ui(working.directory, input, output, session)
    output$unsupervisedUI <- render_unsupervised_ui(working.directory, input, output, session)
    
    
    
    #UnsupervisedUI functions
    
    observe({
        if(!is.null(input$unsupervisedui_file_for_markers) && input$unsupervisedui_file_for_markers != "")
        {
            tab <- read.table(file.path(working.directory, input$unsupervisedui_file_for_markers), header = T, sep = "\t", check.names = F, quote = "")
            updateSelectInput(session, "unsupervisedui_markers", choice = names(tab))
        }
    })
    
    output$unsupervisedui_dialog <- renderText({
        s <- NULL
        if(!is.null(input$unsupervisedui_start_analysis) && input$unsupervisedui_start_analysis != 0)
        {
            out.name <- scaffold:::force_directed_unsupervised(working.directory, 
                input$unsupervisedui_files_list, input$unsupervisedui_markers, input$unsupervisedui_filtering_threshold)
            s <- sprintf("Files analyzed:\n%s", paste(input$unsupervisedui_files_list, collapse = "\n"))
            s <- paste(s, sprintf("Output file: %s", out.name), sep = "\n")
        }
        return(s)
    })
    
    #EditingUI functions
    
 
    
    #MappingUI functions
    
    
    
    #ClusteringUI functions



    #AnalysisUI functions
    
    #GraphUI functions


})



