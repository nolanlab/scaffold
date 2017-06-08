

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


updateReturnOrder <- function(session, inputId, vars) {
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
    