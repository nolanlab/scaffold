
ui <- navbarPage("SCAFFoLD",
    tabPanel("Map exploration", uiOutput("graphUI")),
    tabPanel("Run SCAFFoLD Analysis", uiOutput("analysisUI")),
    tabPanel("Run clustering", uiOutput("clusteringUI")),
    tabPanel("Map dataset", uiOutput("mappingUI")),
    tabPanel("Unsupervised map", uiOutput("unsupervisedUI")),
    tabPanel("Edit SCAFFoLD file", uiOutput("editingUI"))
)



server <- function(input, output, session) {
    options(shiny.error=traceback) 
    if(exists(".ScaffoldWorkingDir"))
        working.directory <- .ScaffoldWorkingDir
    else
        working.directory <- dirname(file.choose())

    app.dir <- file.path(system.file(package = "scaffold"), "shinyGUI")

    source(file.path(app.dir, "server", "tab_editing.R"), local = T)$value
    source(file.path(app.dir, "server", "tab_mapping.R"), local = T)$value
    source(file.path(app.dir, "server", "tab_clustering.R"), local = T)$value
    source(file.path(app.dir, "server", "tab_analysis.R"), local = T)$value
    source(file.path(app.dir, "server", "tab_graph.R"), local = T)$value

    output$graphUI <- render_graph_ui(working.directory, input, output, session)
    output$analysisUI <- render_analysis_ui(working.directory, input, output, session)
    output$clusteringUI <- render_clustering_ui(working.directory, input, output, session)
    output$mappingUI <- render_mapping_ui(working.directory, input, output, session)
    output$editingUI <- render_editing_ui(working.directory, input, output, session)
}

shinyApp(ui = ui, server = server)