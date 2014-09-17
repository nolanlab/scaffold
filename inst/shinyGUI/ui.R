
shinyUI(
        navbarPage("SCAFFoLD",
            tabPanel("Map exploration", uiOutput("graphUI")),
            tabPanel("Run SCAFFoLD Analysis", uiOutput("analysisUI")),
            tabPanel("Run clustering", uiOutput("clusteringUI")),
            tabPanel("Map dataset", uiOutput("mappingUI"))
    )
)





