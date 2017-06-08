

get_cluster_groups_table <- function(v, key) {
    tags$table(class = "table table-hover table-striped",
        tags$tr(tags$td(
            v[1],
            tags$button(class = "btn btn-xs btn-warning pull-right", onClick = sprintf("Shiny.onInputChange('clusteringui_remove_clustering_group', {'key':'%s', 'x':Math.random()})", key),
                tags$span(class = "glyphicon glyphicon-trash")
            )
        )),
        ifelse(length(v > 1), 
            tagList(lapply(tail(v, n = -1), function(x) {tags$tr(tags$td(x))})),
            tagList()
        )
    )
}

render_clustering_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            column(6,
                selectInput("clusteringui_file_for_markers", "Load marker names from file", choices = c("", list.files(path = working.directory, pattern = "*.fcs$")), width = "100%"),
                selectInput("clusteringui_markers", "Choose the markers for clustering", choices = c(""), multiple = T, width = "100%")
            )
        ),
        fluidRow(
            column(6,
                   selectInput("clusteringui_files_list", label = "File list", choices = c(list.files(path = working.directory, pattern = "*.fcs$")),
                               selectize = F, multiple = T, width = "100%"),
                   actionButton("clusteringui_add_clustering_group", "Add clustering group")
            ),
            column(6,
                   uiOutput("clusteringui_clustering_groups_table")
            )
        ),
        fluidRow(
            column(6,    
                numericInput("clusteringui_downsample_to", "Pre-downsample data to (0 means no pre-downsampling, only valid for multiple files clustering)", value = 0),
                numericInput("clusteringui_num_clusters", "Number of clusters", value = 200, min = 1, max = 2000),
                numericInput("clusteringui_num_samples", "Number of samples", value = 50, min = 1), 
                numericInput("clusteringui_asinh_cofactor", "asinh cofactor", value = 5), 
                numericInput("clusteringui_num_cores", "Number of CPU cores to use", value = 1), 
                br(), br(), br(), br(), br(), br(),
                actionButton("clusteringui_start", "Start clustering"), br(), br(),
                conditionalPanel(
                    condition <- "$('html').hasClass('shiny-busy')", br(),
                    p(strong("Processing data...please wait."))
                ),
                conditionalPanel(
                    condition <- "!$('html').hasClass('shiny-busy') && input.clusteringui_start > 0", br(),
                    p(strong("Data processing is complete!"))
                )
            )
        ),
        fluidRow(
            column(12,
                verbatimTextOutput("clusteringui_dialog"), br(), br(), br(), br(), br(), br()
            )
        )

    )
})}



observe({
    if(!is.null(input$clusteringui_file_for_markers) && grepl("*.fcs$", input$clusteringui_file_for_markers))
    {
        v <- scaffold:::get_fcs_col_names(file.path(working.directory, input$clusteringui_file_for_markers))
        updateSelectInput(session, "clusteringui_markers", choices = v)
    }
})

clusteringui_reactive_values <- reactiveValues(clustering_groups = NULL)

output$clusteringui_clustering_groups_table = renderUI({
    dd <- clusteringui_reactive_values$clustering_groups
    return(tagList(mapply(get_cluster_groups_table, dd, names(dd), SIMPLIFY = F)))
})

observe({
    key <- input$clusteringui_remove_clustering_group$key
    if(!is.null(key) && key != "") 
        isolate({
            clusteringui_reactive_values$clustering_groups[key] <- NULL
        })
})

observeEvent(input$clusteringui_add_clustering_group, {
    files_list <- isolate({input$clusteringui_files_list})
    clusteringui_reactive_values$clustering_groups <- c(clusteringui_reactive_values$clustering_groups,
                                                        setNames(list(files_list), files_list[1])
    )
    print(clusteringui_reactive_values$clustering_groups)  
})


output$clusteringui_dialog <- renderText({
    if(!is.null(input$clusteringui_start) && input$clusteringui_start != 0)
    isolate({
        col.names <- input$clusteringui_markers
        files.analyzed <- scaffold:::cluster_fcs_files_groups(working.directory, clusteringui_reactive_values$clustering_groups, input$clusteringui_num_cores, col.names, 
                                input$clusteringui_num_clusters, input$clusteringui_num_samples, input$clusteringui_asinh_cofactor, input$clusteringui_downsample_to)
        #files.analyzed <- scaffold:::cluster_fcs_files_in_dir(working.directory, input$clusteringui_num_cores, col.names, 
        #                        input$clusteringui_num_clusters, input$clusteringui_num_samples, input$clusteringui_asinh_cofactor)
        ret <- sprintf("Clustering completed with markers %s\n", paste(input$clusteringui_markers, collapse = " "))
        ret <- paste(ret, sprintf("Files analyzed:\n%s", paste(files.analyzed, collapse = "\n")), sep = "")
        updateSelectInput(session, "analysisui_reference", choices = c("", list.files(path = working.directory, pattern = "*.clustered.txt$")))
        return(ret)
    })
})
