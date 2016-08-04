process_scaffold_edits <- function(working.directory, sc.file.name, sc.data, active.graph, cols.to.add, cols.to.remove)
{
    to.process <- NULL
    if(cols.to.add != "")
        cols.to.add <- read.table(file.path(working.directory, cols.to.add), header = T, sep = "\t")
    if(active.graph == "All")
        to.process <- names(sc.data$graphs)
    else
        to.process <- c(active.graph)
    
    
    for(graph.name in to.process)
    {
        G <- sc.data$graphs[[graph.name]]
        num.landmarks <- sum(V(G)$type == 1)
        
        for(x in cols.to.remove)
            G <- remove.vertex.attribute(G, x)
        
        for(x in names(cols.to.add))
            G <- set.vertex.attribute(G, x, value = c(rep(NA, num.landmarks), cols.to.add[,x]))
        sc.data$graphs[[graph.name]] <- G
    }
    
    out.name <- tempfile(pattern = sc.file.name, tmpdir = working.directory, fileext = ".scaffold")
    sc.data$dataset.statistics <- scaffold:::get_dataset_statistics(sc.data)
    scaffold:::my_save(sc.data, out.name)
    return(out.name)
}