
render_citrus_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(6,
                fluidRow(
                    column(8,
                        p("Input file")
                    ),
                    column(4,
                        actionButton("citrusui_select_input_file", "Select input file")
                    )
                ),
                verbatimTextOutput("citrusui_selected_input_file"),
                fluidRow(
                    column(8,
                        p("Metadata file")
                    ),
                    column(4,
                        actionButton("citrusui_select_metadata_file", "Select metadata file")
                    )
                ),
                verbatimTextOutput("citrusui_selected_metadata_file"),
                actionButton("citrusui_run_analysis", "Run analysis")
            ),
            column(6,
                inputPanel(
                    h2("Model specification"),
                    selectInput("citrusui_selected_endpoint", "Predict", choices = c(""), multiple = F, width = "100%"),
                    selectInput("citrusui_endpoint_grouping", "For each", choices = c(""), multiple = T, width = "100%"),
                    selectInput("citrusui_predictors", "Using", choices = c(""), multiple = T, width = "100%")
                ),
                selectInput("citrusui_selected_model_type", "Select the type of model", choices = c("pamr", "SAM"), width = "100%"),
                selectInput("citrusui_selected_features", "Select features to use", choices = c(""), multiple = T, width = "100%"),
                fluidRow(
                    column(6,
                        selectInput("citrusui_normalization_baseline", "Baseline for normalization", choices = c("No normalization"), width = "100%")
                    ),
                    column(6,
                        conditionalPanel(
                            condition = "input.citrusui_normalization_baseline != 'No normalization'",
                            selectInput("citrusui_normalization_formula", "Normalization formula", choices = c("Difference", "Ratio"), width = "100%")
                        )
                    )
                )
                
                
            )
        ),
        fluidRow(
            column(12,
                DT::dataTableOutput("citrusui_metadata_table")
            )
        )
    )
})}


citrusui.reactive.values <- reactiveValues(
    input.file.name = " ",
    metadata.file.name = " ",
    input.tab = NULL,
    metadata.tab = NULL
)

output$citrusui_selected_input_file <- renderText({
    citrusui.reactive.values$input.file.name
})

output$citrusui_selected_metadata_file <- renderText({
    citrusui.reactive.values$metadata.file.name
})


output$citrusui_metadata_table <- DT::renderDataTable({
    citrusui.reactive.values$metadata.tab
}, options = list(pageLength = 50))


observeEvent(input$citrusui_select_input_file, {
    isolate({
        citrusui.reactive.values$input.file.name <- file.choose()
        citrusui.reactive.values$input.tab <- read.table(citrusui.reactive.values$input.file.name, 
                                                         header = T, sep = "\t", check.names = F, stringsAsFactors = F, quote = "")
    })
})

observeEvent(input$citrusui_select_metadata_file, {
    isolate({
        citrusui.reactive.values$metadata.file.name <- file.choose()
        citrusui.reactive.values$metadata.tab <- read.table(citrusui.reactive.values$metadata.file.name,
                                                            header = T, sep = "\t", check.names = F, stringsAsFactors = F, quote = "")
    })
})



observeEvent(input$citrusui_run_analysis, {
    isolate({
        print("FIXME!!")
    
        endpoint <- citrusui.reactive.values$metadata.tab[, input$citrusui_selected_endpoint]
        showModal(modalDialog(
            title = "Citrus report",
            "Citrus analysis started, please wait..."
        ))
        
        #scaffold:::run_citrus(working.directory, citrusui.reactive.values$input.tab, 
        #        input$citrusui_selected_features, endpoint, input$citrusui_selected_model_type, baseline = NULL)
        
        scaffold:::calculate_cluster_features(citrusui.reactive.values$input.tab, citrusui.reactive.values$metadata.tab,
                    input$citrusui_selected_features, input$citrusui_predictors, input$citrusui_endpoint_grouping)
        
        showModal(modalDialog(
            title = "Citrus report",
            "Citrus analysis completed"
        ))
    })
})


observe({
    if(!is.null(citrusui.reactive.values$metadata.tab)) {
        isolate({
            col.names <- names(citrusui.reactive.values$metadata.tab)
            col.names <- grep("file", col.names, invert = T, value = T)
            updateSelectInput(session, "citrusui_selected_endpoint", choices = col.names)
            updateSelectInput(session, "citrusui_endpoint_grouping", choices = col.names)
            updateSelectInput(session, "citrusui_predictors", choices = col.names)
        })
    }
})

observe({
    if(!is.null(citrusui.reactive.values$input.tab)) {
        isolate({
            col.names <- names(citrusui.reactive.values$input.tab)
            samples <- strsplit(grep("@", col.names, value = T), "@")
            samples <- sapply(samples, "[", 2)
            col.names <- grep("cellType|@", col.names, invert = T, value = T)
            
            updateSelectInput(session, "citrusui_selected_features", choices = col.names)
            updateSelectInput(session, "citrusui_normalization_baseline", choices = c("No normalization", samples))
        })
    }
})