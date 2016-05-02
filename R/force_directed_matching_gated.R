options(stringsAsFactors = F)
options(warn = 1)


my_save <- function(obj, f_name)
{
    con <- file(f_name, "wb")
    serialize(obj, con, ascii = F)
    close(con)
}

my_load <- function(f_name)
{
	con <- file(f_name, "rb")
	retval <- unserialize(con)
	close(con)
	return(retval)
}


convert_fcs <- function(f, asinh.cofactor)
{
    comp <- grep("SPILL", names(description(f)), value = T)
    if(length(comp) > 0)
    {
        print("Found compensation matrix, applying...")
        comp <- description(f)[comp][[1]]
        if(is.character(comp))
        {
            comp <- strsplit(comp, ",")[[1]]
            num.channels <- as.numeric(comp[1])
            m <- matrix(nrow = num.channels, byrow = T, data = as.numeric(comp[(num.channels + 2):length(comp)]))
            colnames(m) <- comp[2:(1 + num.channels)]
            comp <- m
        }
        f <- compensate(f, spillover = comp)
    }
	tab <- exprs(f)
    m <- as.matrix(tab)
	m <- asinh(m / asinh.cofactor)
	col.names <- colnames(m)
	tab <- data.frame(m)
	names(tab) <- col.names
	return(tab)
}


filter_small_populations <- function(tab, T = 0.0001)
{
    freq <- tab$popsize / sum(tab$popsize)
    return(tab[freq >= T,])
}

downsample_by <- function(tab, col.name, size)
{
    print(sprintf("Downsampling to %d events", size))
    return(ddply(tab, col.name, function(x, size)
    {
        #s <- min(c(size, nrow(x)))
        if(nrow(x) <= size)
            return(x)
        else
            return(x[sample(1:nrow(x), size),])
    }, size = size))
}


load_attractors_from_gated_data <- function(dir, asinh.cofactor)
{
  files <- list.files(dir, ".fcs")
	res <- NULL
	for(f in files)
	{
        population <- tail(strsplit(f, "_")[[1]], n = 1)
		fcs <- read.FCS(paste(dir, f, sep = "/"))
		tab <- convert_fcs(fcs, asinh.cofactor)
        
        if(!all(pData(parameters(fcs))$desc == " "))
            colnames(tab) <- pData(parameters(fcs))$desc
        else
            colnames(tab) <- pData(parameters(fcs))$name
            
       	if(any(is.na(colnames(tab))))
	{
		w <- is.na(colnames(tab))
		colnames(tab)[w] <- pData(parameters(fcs))$name[w]
	}

        tab <- as.matrix(tab)
        tab[tab < 0] <- 0
        tab <- as.data.frame(tab)
        
		tab <- cbind(tab, population, stringsAsFactors = F)
		res <- rbind(res, tab)
	}
    
  downsampled.data <- downsample_by(res, "population", 1000)
  names(downsampled.data) <- gsub("population", "cellType", names(downsampled.data))
    
    #Change cellType to be numbers
	k <- unique(res$population)
	k <- data.frame(population = k, cellType = seq_along(k), stringsAsFactors = F)
	res <- merge(res, k)
	res <- res[, grep("population", names(res), invert = T)]
  res <- ddply(res, ~cellType, colwise(median))
	return(list(downsampled.data = downsampled.data, tab.attractors = res, cellType_key = k))
}


get_highest_scoring_edges <- function(G)
{
    #Remove inter-cluster edges for this calculation
    e <- get.edges(G, E(G))
    E(G)$edge_type <- "cluster_to_landmark"
    e <- cbind(V(G)$type[e[,1]], V(G)$type[e[,2]])
    to.remove <- (e[,1] == 2) & (e[,2] == 2)     
    E(G)$edge_type[(e[,1] == 2) & (e[,2] == 2)] <- "inter_cluster"
    g.temp <- delete.edges(G, E(G)[to.remove])
    
    V(g.temp)$highest_scoring_edge <- 0
    for(i in 1:vcount(g.temp))
    {
        if(V(g.temp)$type[i] == 2)
        {
            sel.edges <- incident(g.temp, i)
            max.edge <- sel.edges[which.max(E(G)[sel.edges]$weight)]
            V(g.temp)$highest_scoring_edge[i] <- max.edge
            E(G)$edge_type[max.edge] <- "highest_scoring"
        }
    }
    V(G)$highest_scoring_edge <- V(g.temp)$highest_scoring_edge
    return(G)
}




get_attractors_from_graph_clustering <- function(f_name, col.names)
{
    tab <- read.table(f_name, header = T, sep = "\t", check.names = F, stringsAsFactors = F)
    print("Running graph based clustering")
    tab <- tab[, grep("cellType|popsize|sample", colnames(tab), invert = T)]
    tab <- tab[!(apply(tab[, col.names], 1, function(x) {all(x == 0)} )),]
    G <- build_graph(tab, col.names, filtering_T = 10)
    temp.G <- G
    E(temp.G)$weight <- E(temp.G)$weight * 100
    cc <- multilevel.community(temp.G)
    V(G)$Modularity.Class <- cc$membership


    ret <- get_vertex_table(G)
    ret$x <- ret$y <- NULL
    ret <- ret[, c("Modularity.Class", colnames(tab))]
    colnames(ret) <- gsub("Modularity.Class", "cellType", colnames(ret))
    
    ret <- ddply(ret, ~cellType, colwise(median))
    ret <- ret[order(ret[, "cellType"]),]

    return(list(tab.attractors = ret, att.labels = c(paste("community", as.character(1:nrow(ret)), sep = "_"))))
    
}


get_dataset_statistics <- function(dataset)
{
    graphs <- dataset$graphs
    ret <- NULL
    for(G in graphs)
    {
        V(G)$popsize.relative <- V(G)$popsize / sum(V(G)$popsize, na.rm = T)
        tab <- get_vertex_table(G)
        max.vals <- sapply(tab, function(x) {if(is.numeric(x)) return(max(x, na.rm  =T))})
        max.vals <- max.vals[!sapply(max.vals, is.null)]
        for(i in 1:length(max.vals))
        {
            var.name <- names(max.vals)[i]
            if(!(var.name %in% names(ret)) || max.vals[[i]] > ret[[var.name]])
            ret[var.name] <- max.vals[i]
        }
    }
    return(list(max.marker.vals = ret))
    
}



add_missing_columns <- function(m, col.names, fill.data)
{
    v <- col.names[!(col.names %in% colnames(m))]
    print(sprintf("Adding missing columns: %s", paste(v, collapse = ", ")))
    ret <- matrix(nrow = nrow(m), ncol = length(v), data = fill.data)
    colnames(ret) <- v
    ret <- data.frame(m, ret, check.names = F)
    return(ret)
}

names_map_factory <- function(names.map)
{
    function(v)
    {
        sel <- v %in% names(names.map)
        if(any(sel))
            v[sel] <- names.map[v[sel]]
        return(v)
        
    }
}

#ref.scaffold.markers are the scaffold markers of the reference file when we are in "Existing" mode
process_files <- function(files.list, G.attractors, tab.attractors, att.labels, col.names, scaffold.mode, 
                          ref.scaffold.markers = NULL, names.mapping = NULL, ew_influence = NULL, 
                          col.names.inter_cluster = NULL, ...)
{
    ret <- list(graphs = list(), clustered.data = list())
    map_names <- names_map_factory(names.mapping)
    for(f in files.list)
    {
        print(paste("Processing", f, sep = " "))
        tab <- read.table(f, header = T, sep = "\t", quote = "", check.names = F, comment.char = "", stringsAsFactors = F)
        names(tab) <- map_names(names(tab))
        col.names.inter_cluster <- map_names(col.names.inter_cluster)
        if(scaffold.mode == "existing")
        {
            #Some markers in the reference scaffold file have been designated
            #for mapping, but they are missing from the sample files
            if(any(is.na(names(names.mapping))))
                tab <- add_missing_columns(tab, col.names, fill.data = 0)
            if(is.null(ew_influence))
                ew_influence <- ceiling(sum(!is.na(names(names.mapping))) / 3)
            print(tab[1:10,])
        }
        else
        {
            if(is.null(ew_influence))
                ew_influence <- ceiling(length(col.names) / 3)
        }
        tab <- tab[!apply(tab[, col.names], 1, function(x) {all(x == 0)}),]
        #tab <- filter_small_populations(tab)
    
       
        
        names(tab) <- gsub("cellType", "groups", names(tab))
        names(tab) <- gsub("^X", "", names(tab))
        print(sprintf("Running with Edge weight: %f", ew_influence))
        res <- process_data(tab, G.attractors, tab.attractors,
            col.names = col.names, att.labels = att.labels, already.clustered = T, ew_influence = ew_influence, 
            col.names.inter_cluster = col.names.inter_cluster, ...)
        G.complete <- get_highest_scoring_edges(res$G.complete)
        clustered.data <- my_load(gsub("txt$", "all_events.RData", f))
        names(clustered.data) <- map_names(names(clustered.data))
        clustered.data <- downsample_by(clustered.data, "cellType", 1000)
        
        ret$graphs[basename(f)] <- list(G.complete)
        ret$clustered.data[basename(f)] <- list(clustered.data)
        
        G.attractors <- res$G.attractors
    }
    ret <- c(ret, list(dataset.statistics = get_dataset_statistics(ret)))
    return(ret)
}

run_analysis_gated <- function(working.dir, ref.file, col.names, asinh.cofactor, ...)
{
    files.list <- list.files(path = working.dir, pattern = "*.clustered.txt$")
    files.list <- files.list[files.list != ref.file]
    print(sprintf("Markers used for SCAFFoLD: %s", paste(col.names, collapse = ", ")))
    files.list <- c(ref.file, files.list)
    print(paste("Using as reference", files.list[1], sep = " "))
    files.list <- paste(working.dir, files.list, sep = "/")
    ref.dir <- paste(working.dir, "gated/", sep = "/")
    gated_data <- load_attractors_from_gated_data(ref.dir, asinh.cofactor)
    tab.attractors <- gated_data$tab.attractors
    att.labels <- gated_data$cellType_key$population
    G.attractors <- NULL
    ret <- process_files(files.list, G.attractors, tab.attractors, att.labels, col.names, scaffold.mode = "gated", ...)
    ret <- c(list(scaffold.col.names = col.names, landmarks.data = gated_data$downsampled.data), ret)
    my_save(ret, paste(working.dir, sprintf("%s.scaffold", ref.file), sep = "/"))
    return(files.list)
}


run_analysis_unsupervised <- function(working.dir, ref.file, col.names, ...)
{
    files.list <- list.files(path = working.dir, pattern = "*.clustered.txt$")
    files.list <- files.list[files.list != ref.file]
    print(sprintf("Markers used for SCAFFoLD: %s", paste(col.names, collapse = ", ")))
    files.list <- c(ref.file, files.list)
    print(paste("Using as reference", files.list[1], sep = " "))
    files.list <- paste(working.dir, files.list, sep = "/")
  
    temp <- get_attractors_from_graph_clustering(files.list[[1]], col.names)
    tab.attractors <- temp$tab.attractors
    att.labels <- temp$att.labels
    G.attractors <- NULL
    ret <- process_files(files.list, G.attractors, tab.attractors, att.labels, col.names, scaffold.mode = "unsuperivsed", ...)
    ret <- c(list(scaffold.col.names = col.names), ret)
    my_save(ret, paste(working.dir, sprintf("%s.scaffold", ref.file), sep = "/"))
    return(files.list)
}





load_existing_layout <- function(scaffold.data)
{
    G <- scaffold.data$graphs[[1]]
    G <- induced.subgraph(G, V(G)$type == 1, impl = "copy_and_delete")
    tab <- get_vertex_table(G)
    V(G)$name <- 1:vcount(G)
    return(list(G.attractors = G, tab.attractors = tab))
}



run_analysis_existing <- function(working.dir, ref.scaffold.file, col.names, names.mapping = NULL, ...)
{
    files.list <- list.files(path = working.dir, pattern = "*.clustered.txt$")
    print(sprintf("Markers used for SCAFFoLD: %s", paste(col.names, collapse = ", ")))
    print(paste("Using as reference", ref.scaffold.file, sep = " "))
    files.list <- paste(working.dir, files.list, sep = "/")
    ref.scaffold.file <- paste(working.dir, ref.scaffold.file, sep = "/")
    ref.scaffold.data <- my_load(ref.scaffold.file)
    ref.scaffold.markers <- ref.scaffold.data$scaffold.col.names
    
    
    l <- load_existing_layout(ref.scaffold.data)
    tab.attractors <- l$tab.attractors
    G.attractors <- l$G.attractors
    att.labels <- V(G.attractors)$Label
    
    ret <- process_files(files.list, G.attractors, tab.attractors, att.labels, col.names, 
                         scaffold.mode = "existing", ref.scaffold.markers = ref.scaffold.markers, names.mapping = names.mapping, ...)
    ret <- c(list(scaffold.col.names = col.names, landmarks.data = ref.scaffold.data$landmarks.data), ret)
    my_save(ret, paste(working.dir, sprintf("%s.scaffold", basename(files.list[[1]])), sep = "/"))
    return(files.list)
}

    

