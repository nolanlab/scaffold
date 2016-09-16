
shinyUI(
        navbarPage("SCAFFoLD",
            tabPanel("Map exploration", uiOutput("graphUI")),
            tabPanel("Run SCAFFoLD Analysis", uiOutput("analysisUI")),
            tabPanel("Run clustering", uiOutput("clusteringUI")),
            tabPanel("Map dataset", uiOutput("mappingUI")),
            tabPanel("Unsupervised map", uiOutput("unsupervisedUI")),
            tabPanel("Edit SCAFFoLD file", uiOutput("editingUI"))
    )
)





