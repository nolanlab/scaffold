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
            selectizeInput("graphui_selected_graph", "Choose a graph:", choices = c(""), width = "100%",
                           options = list(onChange = I("function() {var sel = d3.select('g'); if(!sel.empty()) Shiny.onInputChange('graphui_cur_transform', sel.attr('transform'))}"))),
            selectInput("graphui_marker", "Nodes color:", choices = c(""), width = "100%"),
            selectInput("graphui_color_scaling", "Color scaling:", choices = c("global", "local"), width = "100%"),
            selectInput("graphui_node_size", "Nodes size:", choices = c("Proportional", "Default"), width = "100%"),
            numericInput("graphui_min_node_size", "Minimum node size", 2, min = 0, max = 1000),
            numericInput("graphui_max_node_size", "Maximum node size", 60, min = 0, max = 1000),
            numericInput("graphui_landmark_node_size", "Landmark node size", 8, min = 0, max = 1000),
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

render_clustering_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            column(6,
                selectInput("clusteringui_file_for_markers", "Load marker names from file", choices = c("", list.files(path = working.directory, pattern = "*.fcs$")), width = "100%"),
                selectInput("clusteringui_markers", "Choose the markers for clustering", choices = c(""), multiple = T, width = "100%"),
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

    working.directory <- dirname(file.choose())
    output$graphUI <- render_graph_ui(working.directory, input, output, session)
    output$analysisUI <- render_analysis_ui(working.directory, input, output, session)
    output$clusteringUI <- render_clustering_ui(working.directory, input, output, session)
    output$mappingUI <- render_mapping_ui(working.directory, input, output, session)
    
    #MappingUI functions
    
    output$mappingui_dialog <- renderText({
        if(!is.null(input$mappingui_start) && input$mappingui_start != 0)
            isolate({
                col.names <- input$mappingui_clustered_markers_list
                ref.col.names <- input$mappingui_ref_markers_list
                names.map <- ref.col.names
                #Missing values (i.e. non-mapped markers) are filled with NA
                names(names.map) <- col.names 
                scaffold:::run_analysis_existing(working.directory, input$mappingui_ref_scaffold_file,
                                                 input$mappingui_ref_markers_list, inter.cluster.connections = input$mappingui_inter_cluster_connections, 
                                                 names.map = names.map, col.names.inter_cluster = input$mappingui_markers_inter_cluster,
                                                 inter_cluster.weight_factor = input$mappingui_inter_cluster_weight,
                                                 overlap_method = input$mappingui_overlap_method)
                                                 
                
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
    
    
    output$clusteringui_dialog <- renderText({
        if(!is.null(input$clusteringui_start) && input$clusteringui_start != 0)
        isolate({
            col.names <- input$clusteringui_markers
            files.analyzed <- scaffold:::cluster_fcs_files_in_dir(working.directory, input$clusteringui_num_cores, col.names, 
                                    input$clusteringui_num_clusters, input$clusteringui_num_samples, input$clusteringui_asinh_cofactor)
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
          isolate({
            sel.marker <- NULL
            if(input$graphui_marker %in% attrs)
              sel.marker <- input$graphui_marker
            else
              sel.marker <- "Default"
            updateSelectInput(session, "graphui_marker", choices = c("Default", attrs), selected = sel.marker)
            updateSelectInput(session, "graphui_markers_to_plot", choices = attrs, selected = attrs)
          })
          return(scaffold:::get_graph(sc.data, input$graphui_selected_graph, input$graphui_cur_transform, input$graphui_min_node_size,
                                      input$graphui_max_node_size, input$graphui_landmark_node_size))
        }
        else
            return(NULL)
    })
    
    output$graphui_mainnet <- reactive({
        ret <- get_main_graph()
        if(!is.null(ret))
        {
            isolate({
                if(!is.null(input$graphui_marker))
                    ret$color <- scaffold:::get_color_for_marker(scaffold_data(), input$graphui_marker, input$graphui_selected_graph, input$graphui_color_scaling)
            })
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
    
    #output$graphui_dialog2 <- renderUI({
    #    if(!is.null(input$graphui_selected_landmark) && input$graphui_selected_landmark != "")
    #    {
    #        sc.data <- scaffold_data()
    #        if(!is.null(sc.data))
    #            return(get_pubmed_references(sc.data, input$graphui_selected_graph, input$graphui_selected_landmark))
    #    }
    #})
    
    #output$graphui_dialog3 <- renderUI({
    #    if(!is.null(input$graphui_selected_graph) && input$graphui_selected_graph == "BM-C57BL_6_Fluorescence.clustered.txt")
    #    {
    #        return(HTML("8-color fluorescence experiment from Bone Marrow of C57BL/6 mice<br>Data from Qiu et al., Nat Biotechnol (2011) 'Extracting a cellular hierarchy from high-dimensional cytometry data with SPADE', PMID: <a href='http://www.ncbi.nlm.nih.gov/pubmed/21964415' target='_blank'>21964415</a>"))
    #    }
    #})



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
        if(is.null(input$graphui_marker)) return(NULL)
        sel.marker <- input$graphui_marker
        color.scaling <- input$graphui_color_scaling
        isolate({
            if(sel.marker != "")
            {
                sc.data <- scaffold_data()
                if(!is.null(sc.data))
                {
                    v <- scaffold:::get_color_for_marker(sc.data, sel.marker, input$graphui_selected_graph, color.scaling)
                    session$sendCustomMessage(type = "color_nodes", v)
                }
            }
        })
    })
    
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



