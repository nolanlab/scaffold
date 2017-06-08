

render_analysis_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            column(6,
                selectInput("analysisui_reference", "Choose a reference dataset:", choices = c("", list.files(path = working.directory, pattern = "*.clustered.txt$")), width = "100%"),
                selectInput("analysisui_markers", "Choose the markers for SCAFFoLD", choices = c(""), multiple = T, width = "100%"),
                selectInput("analysisui_mode", "Running mode", choices = c("Gated", "Existing", "Unsupervised", "Combined"), width = "100%"),
                selectInput("analysisui_ew_influence_type", "Edge weight influence", choices = c("Proportional", "Fixed"), width = "100%"),
                conditionalPanel(
                    condition = "input.analysisui_ew_influence_type == 'Fixed'",
                    numericInput("analysisui_ew_influence", "Specifiy Edge weight value", 12), br()
                ),
                checkboxInput("analysisui_inter_cluster_connections", "Add inter-cluster connections", value = FALSE),
                conditionalPanel(
                    condition = "input.analysisui_inter_cluster_connections == true",
                    selectInput("analysisui_markers_inter_cluster", "Markers for inter-cluster connections (if different)", choices = c(""), multiple = T, width = "100%"), 
                    numericInput("analysisui_inter_cluster_weight", "Weight factor for inter-cluster connections", 0.7, min = 0, max = 10, step = 0.1), br()
                ),
                numericInput("analysisui_asinh_cofactor", "asinh cofactor", 5),
                actionButton("analysisui_start", "Start analysis"), br(), br(),
                conditionalPanel(
                    condition <- "$('html').hasClass('shiny-busy')",
                    br(),
                    p(strong("Processing data...please wait."))
                ),
                conditionalPanel(
                    condition <- "!$('html').hasClass('shiny-busy') && input.analysisui_start > 0",
                    br(),
                    p(strong("Data processing is complete!"))
                )
                
            )
        ),
        fluidRow(
            column(12,
                verbatimTextOutput("analysisui_empty"), br(), br(), br(), br(), br(), br()
            )
        )
    )
})}



get_analysisui_mode <- reactive({
    if(!is.null(input$analysisui_mode))
    {
        if(input$analysisui_mode == "Existing")
            updateSelectInput(session, "analysisui_reference", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$")))
        else if(input$analysisui_mode == "Gated" || input$analysisui_mode == "Unsupervised")
            updateSelectInput(session, "analysisui_reference", choices = c("", list.files(path = working.directory, pattern = "*.clustered.txt$")))
        return(input$analysisui_mode)
    }
    return("")
})


observe({
    if(get_analysisui_mode() == "Gated" || get_analysisui_mode() == "Unsupervised")
    {
        #This test is a horrible workaround because the input values are not invalidated after the call to updateSelectInput
        if(!is.null(input$analysisui_reference) && grepl("*.clustered.txt$", input$analysisui_reference))
        {
            tab <- read.table(paste(working.directory, input$analysisui_reference, sep = "/"), header = T, sep = "\t", check.names = F)
            updateSelectInput(session, "analysisui_markers", choices = names(tab))
            updateSelectInput(session, "analysisui_markers_inter_cluster", choices = names(tab))
        }
    }
    else if(get_analysisui_mode() == "Existing" && grepl("*.scaffold$", input$analysisui_reference))
    {
        if(!is.null(input$analysisui_reference) && input$analysisui_reference != "")
        {
            #For the time being load the marker values from the first clustered.txt file
            f <- list.files(path = working.directory, pattern = "*.clustered.txt$", full.names = T)[1]
            tab <- read.table(f, header = T, sep = "\t", check.names = F)
            updateSelectInput(session, "analysisui_markers", choices = names(tab))
            updateSelectInput(session, "analysisui_markers_inter_cluster", choices = names(tab))
        }
    }
})

output$analysisui_empty <- renderText({
    if(!is.null(input$analysisui_start) && input$analysisui_start != 0)
        isolate({
                if(!is.null(input$analysisui_reference) && input$analysisui_reference != "" &&
                    !is.null(input$analysisui_markers) && length(input$analysisui_markers) > 0)
                {
                    files.analyzed <- NULL
                    ew_influence <- NULL
                    if(!is.null(input$analysisui_ew_influence_type)
                            && input$analysisui_ew_influence_type == 'Fixed')
                    {
                        if(!is.null(input$analysisui_ew_influence))
                            ew_influence <- input$analysisui_ew_influence
                    }
                    
                    
                    if(input$analysisui_mode == "Gated")
                    {
                        files.analyzed <- scaffold:::run_analysis_gated(working.directory, input$analysisui_reference,
                            input$analysisui_markers, inter.cluster.connections = input$analysisui_inter_cluster_connections, col.names.inter_cluster = input$analysisui_markers_inter_cluster,
                            asinh.cofactor = input$analysisui_asinh_cofactor, ew_influence = ew_influence, inter_cluster.weight_factor = input$analysisui_inter_cluster_weight, overlap_method = "repel")
                    }
                    else if(input$analysisui_mode == "Existing")
                    {
                        files.analyzed <- scaffold:::run_analysis_existing(working.directory, input$analysisui_reference,
                            input$analysisui_markers, inter.cluster.connections = input$analysisui_inter_cluster_connections, ew_influence = ew_influence)
                    }
                    if(input$analysisui_mode == "Unsupervised")
                    {
                        files.analyzed <- scaffold:::run_analysis_unsupervised(working.directory, input$analysisui_reference,
                            input$analysisui_markers, inter.cluster.connections = input$analysisui_inter_cluster_connections, ew_influence = ew_influence)
                    }
                }
                updateSelectInput(session, "graphui_dataset", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$")))
                ret <- sprintf("Analysis completed with markers %s\n", paste(input$analysisui_markers, collapse = " "))
                ret <- paste(ret, sprintf("Files analyzed:\n%s", paste(files.analyzed, collapse = "\n")), sep = "")
                return(ret)
        })
        
        
})
