
render_citrus_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(6,
                actionButton("citrusui_select_input_file", "Select input file"),
                actionButton("citrusui_select_metadata_file", "Select metadata file")
            )
        )
    )
})}


citrusui.reactive.values <- reactiveValues(
    input.file = NULL,
    metadata.file = NULL
)


observeEvent(input$citrusui_select_input_file, {
    isolate({
        citrusui.reactive.values$input.file <- file.choose()
    })
})

observeEvent(input$citrusui_select_metadata_file, {
    isolate({
        citrusui.reactive.values$metadata.file <- file.choose()
    })
})