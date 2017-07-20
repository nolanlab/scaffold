
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
                verbatimTextOutput("citrusui_selected_metadata_file")
            ),
            column(6,
                selectInput("citrusui_selected_model_type", "Select the type of model", choices = c("pamr", "SAM"), width = "100%"),
                p("Select which features to use"),
                checkboxInput("citrusui_abundance_features", "Abundance features"),
                checkboxInput("citrusui_expression_features", "Marker expression"),
                conditionalPanel(
                    condition = "input.citrusui_expression_features == true",
                    selectInput("citrusui_selected_feature_markers", "Select markers", choices = c(""), multiple = T, width = "100%")
                ),
                conditionalPanel(
                    condition = "output.citrusui_selected_metadata_file != ' '",
                    selectInput("citrusui_selected_endpoint", "Select endpoint", choices = c(""), multiple = F, width = "100%")
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



observe({
    if(!is.null(citrusui.reactive.values$metadata.tab)) {
        isolate({
            col.names <- names(citrusui.reactive.values$metadata.tab)
            col.names <- grep("file|condition", col.names, invert = T, value = T)
            updateSelectInput(session, "citrusui_selected_endpoint", choices = col.names)
        })
    }
})

observe({
    if(!is.null(citrusui.reactive.values$input.tab)) {
        isolate({
            col.names <- names(citrusui.reactive.values$input.tab)
            col.names <- grep("cellType|popsize|@", col.names, invert = T, value = T)
            updateSelectInput(session, "citrusui_selected_feature_markers", choices = col.names)
        })
    }
})