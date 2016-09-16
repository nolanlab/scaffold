force_directed_unsupervised <- function(working.dir, input.files, col.names, filtering.threshold)
{
    tab <- NULL
    common.cols <- get_common_columns(working.dir, input.files)
    
    
    for(f in input.files)
    {
        temp <- read.table(file.path(working.dir, f), header = T, sep = "\t", check.names = F, quote = "")
        temp <- temp[, common.cols]
        temp$sample <- f
        tab <- rbind(tab, temp)
    }
    
    G <- scaffold:::build_graph(tab, col.names, filtering_T = filtering.threshold)
    cc <- igraph::multilevel.community(G)
    V(G)$Modularity.Class <- as.character(cc$membership)
    
    out.name <- file.path(working.dir, sprintf("%s.unsupervised.graphml", input.files[[1]]))
    write.graph(G, out.name, format = "graphml")
    return(out.name)
}