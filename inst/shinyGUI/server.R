


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
        column(9,
            includeHTML("graph.js"),
            reactiveNetwork(outputId = "graphui_mainnet")
        ),
        column(3,
    
            selectInput("graphui_dataset", "Choose a dataset:", choices = c("", list.files(path = working.directory, pattern = "*.scaffold$"))),
            selectizeInput("graphui_selected_graph", "Choose a graph:", choices = c(""), options = list(onChange = I("function() {var sel = d3.select('g'); if(!sel.empty()) Shiny.onInputChange('graphui_cur_transform', sel.attr('transform'))}"))),
            selectInput("graphui_marker", "Nodes color:", choices = c("")),
            selectInput("graphui_color_scaling", "Color scaling:", choices = c("global", "local")),
            selectInput("graphui_node_size", "Nodes size:", choices = c("Proportional", "Default")),
            selectInput("graphui_display_edges", "Display edges:", choices = c("All", "Highest scoring only")), br(),
            actionButton("graphui_reset_graph_position", "Reset graph position"), br(),
            actionButton("graphui_toggle_landmark_labels", "Toggle landmark labels"), br(),
            actionButton("graphui_toggle_cluster_labels", "Toggle cluster labels"), br(),
            br(),
            selectInput("graphui_markers_to_plot", "Markers to plot in cluster view:", choices = c(""), multiple = T),
            br(), br(),
            verbatimTextOutput("graphui_dialog1"),
            htmlOutput("graphui_dialog2"),br(),
            htmlOutput("graphui_dialog3"),
            br(),br(),
            p("Technical Note: As previously described elsewhere, we have observed higher background staining on Neutrophils, especially for antibodies used at higher concentrations (see Table S1). Therefore, protein expression on this population should be interpreted with caution")
        )
    ),
    
    

    fluidRow(
        column(12,
            verbatimTextOutput("graphui_plot_title")
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
                selectInput("clusteringui_file_for_markers", "Load marker names from file", choices = c("", list.files(path = working.directory, pattern = "*.fcs$"))),
                selectInput("clusteringui_markers", "Choose the markers for clustering", choices = c(""), multiple = T),
                numericInput("clusteringui_num_clusters", "Number of clusters", value = 200, min = 1, max = 2000),
                numericInput("clusteringui_num_samples", "Number of samples", value = 50, min = 1), br(), br(), br(), br(), br(), br(),
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
                selectInput("analysisui_reference", "Choose a reference dataset:", choices = c("", list.files(path = working.directory, pattern = "*.clustered.txt$"))),
                selectInput("analysisui_markers", "Choose the markers for SCAFFoLD", choices = c(""), multiple = T),
                selectInput("analysisui_mode", "Running mode", choices = c("Gated", "Existing", "Unsupervised", "Combined")),
                checkboxInput("analysisui_inter_cluster_connections", "Add inter-cluster connections", value = FALSE),
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




shinyServer(function(input, output, session)
{

    working.directory <- dirname(file.choose())
    #working.directory <- paste(getwd(), "data/", sep = "/")
    output$graphUI <- render_graph_ui(working.directory, input, output, session)
    output$analysisUI <- render_analysis_ui(working.directory, input, output, session)
    output$clusteringUI <- render_clustering_ui(working.directory, input, output, session)


    #ClusteringUI markers

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
            files.analyzed <- scaffold:::cluster_fcs_files_in_dir(working.directory, col.names, input$clusteringui_num_clusters, input$clusteringui_num_samples)
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
            }
        }
        else if(get_analysisui_mode() == "Existing" && grepl("*.scaffold$", input$analysisui_reference))
        {
            if(!is.null(input$analysisui_reference) && input$analysisui_reference != "")
            {
                d <- scaffold:::my_load(paste(working.directory, input$analysisui_reference, sep = "/"))
                markers <- scaffold:::get_numeric_vertex_attributes(d$graphs[[1]])
                updateSelectInput(session, "analysisui_markers", choices = markers)
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
                        if(input$analysisui_mode == "Gated")
                        {
                            files.analyzed <- scaffold:::run_analysis_gated(working.directory, input$analysisui_reference,
                                input$analysisui_markers, inter.cluster.connections = input$analysisui_inter_cluster_connections)
                        }
                        else if(input$analysisui_mode == "Existing")
                        {
                            files.analyzed <- scaffold:::run_analysis_existing(working.directory, input$analysisui_reference,
                                input$analysisui_markers, inter.cluster.connections = input$analysisui_inter_cluster_connections)
                        }
                        if(input$analysisui_mode == "Unsupervised")
                        {
                            files.analyzed <- scaffold:::run_analysis_unsupervised(working.directory, input$analysisui_reference,
                                input$analysisui_markers, inter.cluster.connections = input$analysisui_inter_cluster_connections)
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
        #file_name <- "mouse_immune_reference_1000.scaffold"
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
        if(!is.null(sc.data))
        {
            if(!is.null(input$graphui_selected_graph) && input$graphui_selected_graph != "")
            {
                attrs <- scaffold:::get_numeric_vertex_attributes(sc.data, input$graphui_selected_graph)
                isolate({
                    sel.marker <- NULL
                    if(input$graphui_marker %in% attrs)
                        sel.marker <- input$graphui_marker
                    else
                        sel.marker <- "Default"
                    updateSelectInput(session, "graphui_marker", choices = c("Default", attrs), selected = sel.marker)
                    updateSelectInput(session, "graphui_markers_to_plot", choices = attrs)
                })
                scaffold:::get_graph(sc.data, input$graphui_selected_graph, input$graphui_cur_transform)
            }
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
    
    output$graphui_dialog3 <- renderUI({
        if(!is.null(input$graphui_selected_graph) && input$graphui_selected_graph == "BM-C57BL_6_Fluorescence.clustered.txt")
        {
            return(HTML("8-color fluorescence experiment from Bone Marrow of C57BL/6 mice<br>Data from Qiu et al., Nat Biotechnol (2011) 'Extracting a cellular hierarchy from high-dimensional cytometry data with SPADE', PMID: <a href='http://www.ncbi.nlm.nih.gov/pubmed/21964415' target='_blank'>21964415</a>"))
        }
    })



    output$graphui_plot = renderPlot({
        p <- NULL
        if(!is.null(input$graphui_selected_cluster) && input$graphui_selected_cluster != "")
        {
            col.names <- input$graphui_markers_to_plot
            print(input$graphui_selected_cluster)
            if(length(col.names >= 1))
            p <- scaffold:::plot_cluster(scaffold_data(), input$graphui_selected_cluster, input$graphui_selected_graph, input$graphui_markers_to_plot)
        }
        print(p)
    })


    output$graphui_plot_title = renderPrint({
        if(!is.null(input$graphui_selected_cluster) && input$graphui_selected_cluster != "")
            sprintf("Plotting cluster %s", input$graphui_selected_cluster)
    })
    
  
    
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



