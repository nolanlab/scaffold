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

render_graph_ui <- function(working.directory, ...){renderUI({
fluidPage(
    fluidRow(
        column(6,
            tags$head(tags$script(src = "d3.min.js")),
            tags$head(tags$script(src = "graph.js")),
            tags$head(tags$script(src = "rect_select.js")),
            singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'rect_select.css'))),
            singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'graph.css'))),
            reactiveNetwork(outputId = "graphui_mainnet")
        ),
        column(3,
               dataTableOutput("graphui_table")
        ),
        column(3,
    
            selectInput("graphui_dataset", "Choose a dataset:", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$")), width = "100%"),
            selectizeInput("graphui_selected_graph", "Choose a graph:", choices = c(""), width = "100%"),
            selectizeInput("graphui_active_sample", "Active sample", choices = c("All"), width = "100%"),
            selectInput("graphui_marker", "Nodes color:", choices = c("Default"), width = "100%"),
            fluidRow(
                column(6,
                    selectInput("graphui_stats_type", "Stats type", choices = c("Ratio", "Difference"))
                ),
                column(6,
                    selectInput("graphui_stats_relative_to", "Stats relative to:", choices = c("Absolute"), width = "100%")
                )
            ),
            selectInput("graphui_color_scaling", "Color scaling:", choices = c("global", "local"), width = "100%"),
            h4("Colors for scale"),
            selectInput("graphui_color_number", "Number of colors", choices = c(2,3)),
            fluidRow(
                column(6,
                    shinyjs::colourInput("graphui_color_under", "Under:", value = "#FFFF00")
                ),
                column(6,
                    shinyjs::colourInput("graphui_color_over", "Over:", value = "#0000FF")
                )
            ),
            fluidRow(
                column(4,
                    shinyjs::colourInput("graphui_color_min", "Min:", value = "#E7E7E7")
                ),
                column(4,
                    conditionalPanel(
                        condition = "input.graphui_color_number == 3",
                        shinyjs::colourInput("graphui_color_mid", "Mid:", value = "#E7E7E7")
                    )
                ),
                column(4,
                    shinyjs::colourInput("graphui_color_max", "Max:", value = "#E71601")
                )
            ),
            conditionalPanel(
                condition = "input.graphui_color_number == 3",
                sliderInput("graphui_color_scale_mid", "Color scale midpoint", min = 0.0, max = 5.0, value = 2.5, round = -2, step = 0.1, sep = "")
            ),
            sliderInput("graphui_color_scale_lim", "Color scale limits", min = 0.0, max = 5.0, value = c(0.0, 5.0), round = -2, step = 0.1, sep = ""),
            fluidRow(
                column(6,
                    numericInput("graphui_color_scale_min", "Color scale min:", 0)
                ),
                column(6,
                    numericInput("graphui_color_scale_max", "Color scale max:", 5)
                )
            ),
            fluidRow(
                column(6,
                    selectInput("graphui_node_size", "Nodes size:", choices = c("Proportional", "Default"), width = "100%")
                ),
                column(6,
                    numericInput("graphui_min_node_size", "Minimum node size", 2, min = 0, max = 1000)
                )
            ),
            fluidRow(
                column(6,
                    numericInput("graphui_max_node_size", "Maximum node size", 60, min = 0, max = 1000)
                ),
                column(6,
                    numericInput("graphui_landmark_node_size", "Landmark node size", 8, min = 0, max = 1000)
                )
            ),
            selectInput("graphui_display_edges", "Display edges:", choices = c("All", "Highest scoring", "Inter cluster", "To landmark"), width = "100%"), br(),
            actionButton("graphui_reset_graph_position", "Reset graph position"), br(),
            actionButton("graphui_toggle_landmark_labels", "Toggle landmark labels"), br(),
            actionButton("graphui_toggle_cluster_labels", "Toggle cluster labels"), br(),
            actionButton("graphui_export_selected_clusters", "Export selected clusters"), br(),
            p("For the export to work, the original RData files corresponding to the clustered files in use must be located in the working directory"),
            actionButton("graphui_plot_clusters", "Plot selected clusters"), checkboxInput("graphui_pool_cluster_data", "Pool cluster data", value = FALSE), br(),
            selectInput("graphui_plot_type", "Plot type:", choices = c("Density", "Boxplot", "Scatterplot"), width = "100%"),
            selectInput("graphui_markers_to_plot", "Markers to plot in cluster view:", choices = c(""), multiple = T, width = "100%"),
            verbatimTextOutput("graphui_dialog1")
        )
    ),
    fluidRow(
        column(12,
            plotOutput("graphui_plot")
        )
    )
)
})}

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


html_list <- function(vars, id) {
  hl <- paste0("<ul id=\'",id,"\' class='stab'>")
  for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
  paste0(hl, "</ul>")
}

returnOrder <- function(inputId, vars) {
  tagList(
    singleton(tags$head(tags$script(src = 'sort.js'))),
    singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'sort.css'))),
    HTML(html_list(vars, inputId)),
    tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
  )
}


updateReturnOrder <- function(session, inputId, vars)
{
  session$sendInputMessage(inputId, list(value = vars))
}

render_mapping_ui <- function(working.directory, ...){renderUI({
  fluidPage(
    tags$head(tags$script(src = "jquery-ui.min.js")),
    fluidRow(
      column(6,
             selectInput("mappingui_ref_scaffold_file", "Select reference SCAFFoLD file", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$")), width = "100%"),
             selectInput("mappingui_ref_scaffold_file_markers", "Select the markers to include in the mapping", choices = c(""), multiple = T, width = "100%"),
             br(), br(),
             wellPanel(returnOrder("mappingui_ref_markers_list", c(""))),
             br(), br(), br()
      ),
      column(6,
             selectInput("mappingui_sample_clustered_file", "Select a sample clustered file", choices = c("", list.files(path = working.directory, pattern = "*.clustered.txt$")), width = "100%"),
             selectInput("mappingui_sample_clustered_file_markers", "Select the markers to include in the mapping", choices = c(""), multiple = T, width = "100%"),
             br(), br(),
             wellPanel(returnOrder("mappingui_clustered_markers_list", c(""))),
             br(), br(), br()
      )
    ),
    fluidRow(
        column(12,
               selectInput("mappingui_ew_influence_type", "Edge weight influence", choices = c("Proportional", "Fixed")),
               conditionalPanel(
                   condition = "input.mappingui_ew_influence_type == 'Fixed'",
                   numericInput("mappingui_ew_influence", "Specifiy Edge weight value", 12), br()
               ),
               selectInput("mappingui_overlap_method", "Overlap resolution method", choices = c("repel", "expand")),
               checkboxInput("mappingui_inter_cluster_connections", "Add inter-cluster connections", value = FALSE),
               conditionalPanel(
                   condition = "input.mappingui_inter_cluster_connections == true",
                   selectInput("mappingui_markers_inter_cluster", "Markers for inter-cluster connections (if different)", choices = c(""), multiple = T, width = "100%"), 
                   numericInput("mappingui_inter_cluster_weight", "Weight factor for inter-cluster connections", 0.7, min = 0, max = 10, step = 0.1), br()
               )
        )
    ),
    fluidRow(
      column(12,
             actionButton("mappingui_start", "Start analysis"), br(), br(),
             conditionalPanel(
               condition <- "$('html').hasClass('shiny-busy')",
               br(),
               p(strong("Processing data...please wait."))
             ),
             conditionalPanel(
               condition <- "!$('html').hasClass('shiny-busy') && input.mappingui_start > 0",
               br(),
               p(strong("Data processing is complete!"))
             ),
             verbatimTextOutput("mappingui_dialog"), br(), br(), br(), br(), br(), br()
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
    
    #MappingUI functions
    
    
    observe({
        if(!is.null(input$graphui_color_scaling) && input$graphui_color_scaling == "global")
        {
            updateSliderInput(session, "graphui_color_scale_lim", min = input$graphui_color_scale_min,
                              max = input$graphui_color_scale_max)
        }
    })
    
    output$mappingui_dialog <- renderText({
        if(!is.null(input$mappingui_start) && input$mappingui_start != 0)
            isolate({
                col.names <- input$mappingui_clustered_markers_list
                ref.col.names <- input$mappingui_ref_markers_list
                names.map <- ref.col.names
                #Missing values (i.e. non-mapped markers) are filled with NA
                names(names.map) <- col.names 
                ew_influence <- NULL
                if(!is.null(input$mappingui_ew_influence_type) && input$mappingui_ew_influence_type == 'Fixed')
                {
                    if(!is.null(input$mappingui_ew_influence))
                        ew_influence <- input$mappingui_ew_influence
                }
                
                
                scaffold:::run_analysis_existing(working.directory, input$mappingui_ref_scaffold_file,
                                                 input$mappingui_ref_markers_list, inter.cluster.connections = input$mappingui_inter_cluster_connections, 
                                                 names.map = names.map, col.names.inter_cluster = input$mappingui_markers_inter_cluster,
                                                 inter_cluster.weight_factor = input$mappingui_inter_cluster_weight,
                                                 overlap_method = input$mappingui_overlap_method, ew_influence = ew_influence)
                                                 
                
                updateSelectInput(session, "graphui_dataset", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$")))
                ret <- sprintf("Analysis completed with markers %s\n", paste(input$mappingui_ref_scaffold_fil, collapse = " "))
                if(!is.null(names.map))
                    ret <- paste(ret, sprintf("Mapping: %s -> %s\n", paste(names.map, collapse = " "), paste(names(names.map), collapse = " ")), sep = "")
                return(ret)
            })
    })
    
    observe({
      if(!is.null(input$mappingui_sample_clustered_file) && input$mappingui_sample_clustered_file != "")
      {
        tab <- read.table(paste(working.directory, input$mappingui_sample_clustered_file, sep = "/"), header = T, sep = "\t", quote = "", check.names = F)
        updateSelectInput(session, "mappingui_sample_clustered_file_markers", choices = names(tab))
        updateSelectInput(session, "mappingui_markers_inter_cluster", choices = names(tab))
      }
    })
    
    observe({
      if(!is.null(input$mappingui_sample_clustered_file_markers) && length(input$mappingui_sample_clustered_file_markers > 0))
      {
        updateReturnOrder(session, "mappingui_clustered_markers_list", input$mappingui_sample_clustered_file_markers)
      }
    })
    
    observe({
      if(!is.null(input$mappingui_ref_scaffold_file) && input$mappingui_ref_scaffold_file != "")
      {
        file_name <- paste(working.directory, input$mappingui_ref_scaffold_file, sep = "/")
        sc.data <- scaffold:::my_load(file_name)
        
        updateSelectInput(session, "mappingui_ref_scaffold_file_markers", choices = sc.data$scaffold.col.names)
      }
    })
    
    observe({
      if(!is.null(input$mappingui_ref_scaffold_file_markers) && length(input$mappingui_ref_scaffold_file_markers > 0))
      {
        updateReturnOrder(session, "mappingui_ref_markers_list", input$mappingui_ref_scaffold_file_markers)
      }
    })
    
    
    #ClusteringUI functions

    observe({
        if(!is.null(input$clusteringui_file_for_markers) && grepl("*.fcs$", input$clusteringui_file_for_markers))
        {
            v <- scaffold:::get_fcs_col_names(working.directory, input$clusteringui_file_for_markers)
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



    #AnalysisUI functions
    
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

    #GraphUI functions

    scaffold_data <- reactive({
        file_name <- input$graphui_dataset
        if(!is.null(file_name) && file_name != "")
        {
            file_name <- paste(working.directory, file_name, sep = "/")
            print("Loading data...")
            data <- scaffold:::my_load(file_name)
            updateSelectInput(session, "graphui_selected_graph", choices = c("", names(data$graphs)))
            return(data)
        }
        else
            return(NULL)
    })
    
    
    get_main_graph <- reactive({
        sc.data <- scaffold_data()
        if(!is.null(sc.data) && !is.null(input$graphui_selected_graph) && input$graphui_selected_graph != "")
        {
          attrs <- scaffold:::get_numeric_vertex_attributes(sc.data, input$graphui_selected_graph)
          node.size.attr <- scaffold:::combine_marker_sample_name("popsize", input$graphui_active_sample)
          
          isolate({
            sel.marker <- NULL
            if(input$graphui_marker %in% attrs)
              sel.marker <- input$graphui_marker
            else
              sel.marker <- "Default"
            updateSelectInput(session, "graphui_marker", choices = c("Default", attrs), selected = sel.marker)
            updateSelectInput(session, "graphui_markers_to_plot", choices = attrs, selected = attrs)
            sample.names <- scaffold:::get_sample_names(sc.data, input$graphui_selected_graph)
            updateSelectInput(session, "graphui_active_sample", choices = c("All", sample.names),
                              selected = input$graphui_active_sample)
            updateSelectInput(session, "graphui_stats_relative_to", choices = c("Absolute", sample.names),
                              selected = input$graphui_stats_relative_to)
          })
          return(scaffold:::get_graph(sc.data, input$graphui_selected_graph, node.size.attr, input$graphui_min_node_size,
                                      input$graphui_max_node_size, input$graphui_landmark_node_size))
        }
        else
            return(NULL)
    })
    
    read_color_scale_info <- reactive({
        return(list(sel.marker = input$graphui_marker, color.scale.lim = input$graphui_color_scale_lim,
                    color.scale.mid = input$graphui_color_scale_mid))
    })
    
    get_color_scale <- reactive({
        #This code only updates the color scales
        sc.data <- scaffold_data()
        if(is.null(sc.data) || is.null(get_main_graph())) return(NULL)
        sel.marker <- input$graphui_marker
        rel.to <- input$graphui_stats_relative_to
        color.scaling <- input$graphui_color_scaling
        stats.type <- input$graphui_stats_type
        isolate({
            color <- NULL
            if(sel.marker != "")
            {
                #Colors are not really important here, only included because they need to be passed to the function
                min.color <- input$graphui_color_min
                mid.color <- input$graphui_color_mid
                max.color <- input$graphui_color_max
                under.color <- input$graphui_color_under
                over.color <- input$graphui_color_over
                color <- scaffold:::get_color_for_marker(sc.data, sel.marker, rel.to, input$graphui_selected_graph, 
                                                         input$graphui_active_sample, color.scaling, stats.type, colors.to.interpolate = c(min.color, mid.color, max.color),
                                                         under.color, over.color)
                if(!is.null(color$color.scale.lim) 
                    && !(is.null(color.scaling)) && color.scaling == "local")
                {
                    updateSliderInput(session, "graphui_color_scale_lim", min = color$color.scale.lim$min,
                                      max = color$color.scale.lim$max, step = 0.1, value = c(color$color.scale.lim$min, color$color.scale.lim$max))
                    updateSliderInput(session, "graphui_color_scale_mid", min = color$color.scale.lim$min,
                                      max = color$color.scale.lim$max, step = 0.1, value = mean(c(color$color.scale.lim$min, color$color.scale.lim$max)))
                }
            }
        })
    })
    
    get_color <- reactive({
        #This code does the actual coloring
        get_color_scale()
        color.scale.info <- read_color_scale_info()
        min.color <- input$graphui_color_min
        mid.color <- input$graphui_color_mid
        max.color <- input$graphui_color_max
        under.color <- input$graphui_color_under
        over.color <- input$graphui_color_over
        color.scale.lim <- color.scale.info$color.scale.lim
        colors.to.interpolate <- NULL
        color.scale.mid <- NULL
        if(input$graphui_color_number == 3)
        {
            colors.to.interpolate <- c(min.color, mid.color, max.color)
            color.scale.mid <- color.scale.info$color.scale.mid
        }
        else
            colors.to.interpolate <- c(min.color, max.color)
        return(
            isolate({
                sel.marker <- color.scale.info$sel.marker
                
                color.vector <- NULL
                active.sample <- input$graphui_active_sample
                rel.to <- input$graphui_stats_relative_to
                color.scaling <- input$graphui_color_scaling
                stats.type <- input$graphui_stats_type
                
                if(sel.marker != "")
                {
                    sc.data <- scaffold_data()
                    if(!is.null(sc.data))
                    {
                        color <- scaffold:::get_color_for_marker(sc.data, sel.marker, rel.to, input$graphui_selected_graph, 
                                    active.sample, color.scaling, stats.type, colors.to.interpolate = colors.to.interpolate, under.color, over.color,
                                    color.scale.limits = color.scale.lim, color.scale.mid = color.scale.mid)
                        color.vector <- color$color.vector
                    }
                }
                return(color.vector)
            })
        )
    })
    
    
    
    output$graphui_mainnet <- reactive({
        ret <- get_main_graph()
        if(!is.null(ret))
        {
            ret$color <- get_color()
            ret$trans_to_apply <- isolate({input$graphui_cur_transform})
        }
        return(ret)
    })
    
    output$graphui_table <- renderDataTable({
        sc.data <- scaffold_data()
        if(!is.null(sc.data) && !is.null(input$graphui_selected_graph) && input$graphui_selected_graph != "")
        {
            if(is.null(input$graphui_selected_nodes) || length(input$graphui_selected_nodes) == 0)
            {
                scaffold:::get_number_of_cells_per_landmark(scaffold_data(), input$graphui_selected_graph)     
            }
            else
            {
                scaffold:::get_summary_table(scaffold_data(), input$graphui_selected_graph, input$graphui_selected_nodes)
            }
        }
    }, options = list(scrollX = TRUE, searching = FALSE, scrollY = "800px", paging = FALSE, info = FALSE, processing = FALSE))
    
    output$graphui_dialog1 <- reactive({
        sc.data <- scaffold_data()
        ret <- ""
        if(!is.null(sc.data))
            ret <- sprintf("Markers used for SCAFFoLD: %s", paste(sc.data$scaffold.col.names, collapse = ", "))
        return(ret)
    })
    

    output$graphui_plot = renderPlot({
        p <- NULL
        #session$sendCustomMessage(type = "get_selected_nodes", list())
        if(!is.null(input$graphui_plot_clusters) && input$graphui_plot_clusters != 0)
        {
            isolate({
                col.names <- input$graphui_markers_to_plot
                if((length(col.names) >= 1) && (length(input$graphui_selected_nodes) >= 1))
                    p <- scaffold:::plot_cluster(scaffold_data(), input$graphui_selected_nodes, input$graphui_selected_graph, 
                                             input$graphui_markers_to_plot, input$graphui_pool_cluster_data, input$graphui_plot_type)
            })
        }
        print(p)
    })


    #output$graphui_plot_title = renderPrint({
    #    if(!is.null(input$graphui_selected_cluster) && input$graphui_selected_cluster != "")
    #        sprintf("Plotting cluster %s", input$graphui_selected_cluster)
    #})
    
  
    
    
    observe({
        if(!is.null(input$graphui_reset_colors) && input$graphui_reset_colors != 0)
        {
            session$sendCustomMessage(type = "reset_colors", "none")
        }
    })
    
    observe({
        if(!is.null(input$graphui_export_selected_clusters) && input$graphui_export_selected_clusters > 0)
        {
            isolate({
                if(!is.null(input$graphui_selected_nodes) && length(input$graphui_selected_nodes) >= 1)
                    scaffold:::export_clusters(working.directory, input$graphui_selected_graph, input$graphui_selected_nodes)
            })
        }
    })
    
    observe({
        if(!is.null(input$graphui_reset_graph_position) && input$graphui_reset_graph_position != 0)
        {
            session$sendCustomMessage(type = "reset_graph_position", "none")
        }
    })
    
    observe({
        if(!is.null(input$graphui_toggle_landmark_labels) && input$graphui_toggle_landmark_labels != 0)
        {
            display <- ifelse(input$graphui_toggle_landmark_labels %% 2 == 0, "", "none")
            session$sendCustomMessage(type = "toggle_label", list(target = "landmark", display = display))
        }
    })
    
    observe({
            display_edges <- input$graphui_display_edges
            session$sendCustomMessage(type = "toggle_display_edges", display_edges)
    })
    
    observe({
        if(!is.null(input$graphui_toggle_cluster_labels) && input$graphui_toggle_cluster_labels != 0)
        {
            display <- ifelse(input$graphui_toggle_cluster_labels %% 2 == 0, "none", "")
            session$sendCustomMessage(type = "toggle_label", list(target = "cluster", display = display))
        }
    })
    
    observe({
        display <- tolower(input$graphui_node_size)
        session$sendCustomMessage(type = "toggle_node_size", list(display = display))
    })
    
    
    observe({
        if(!is.null(input$graphui_toggle_node_size) && input$graphui_toggle_node_size != 0)
        {
            display <- ifelse(input$graphui_toggle_node_size %% 2 == 0, "proportional", "default")
            session$sendCustomMessage(type = "toggle_node_size", list(display = display))
        }
    })
})



