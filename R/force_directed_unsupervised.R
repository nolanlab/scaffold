
force_directed_unsupervised <- function(working.dir, input.files, col.names, filtering.threshold, output.name, output.type = "legacy")
{
    tab <- NULL
    ret <- list(graphs = list())
    common.cols <- get_common_columns(working.dir, input.files)
    
    
    for(f in input.files)
    {
        temp <- read.table(file.path(working.dir, f), 
            header = T, sep = "\t", check.names = F, quote = "", stringsAsFactors = F)
        temp <- temp[, common.cols]
        temp$sample <- f
        tab <- rbind(tab, temp)
    }

    G <- scaffold:::build_graph(tab, col.names, filtering_T = filtering.threshold)
    
    for(i in names(tab))
        G <- set.vertex.attribute(G, name = i, value = tab[, i])
    
    cc <- igraph::multilevel.community(G)
    V(G)$Modularity.Class <- as.character(cc$membership)
    V(G)$name <- seq_along(V(G)$name)
    
    print("Running ForceAtlas2...")
    G <- complete.forceatlas2(G, first.iter = 50000, overlap.iter = 1, overlap_method = NULL, ew_influence = 5)
    print("ForceAtlas2 done")
    
    if(output.type == "legacy")
    {
        out.name <- file.path(working.dir, sprintf("%s.unsupervised.graphml", output.name))
        write.graph(G, out.name, format = "graphml")
        ret$graphs <- c(ret$graphs, setNames(list(G), input.files[[1]]))
        my_save(ret,  file.path(working.dir, sprintf("%s.unsupervised.scaffold", output.name)))
    }
    else if(output.type == "directory")
    {
        full.path <- file.path(working.dir, "unsupervised_graphs")
        
        dir.create(full.path, recursive = T)
        out.name <- file.path(full.path, sprintf("%s.unsupervised.graphml", output.name))
        write.graph(G, out.name, format = "graphml" )
        G.json <- scaffold:::graph_to_json(G)
        cat(G.json, file = gsub("graphml", "json", out.name))
    }
    
    return(out.name)
}