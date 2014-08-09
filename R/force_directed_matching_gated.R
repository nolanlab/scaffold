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


convert_fcs <- function(f)
{
	tab <- exprs(f)
    m <- as.matrix(tab)
	m <- asinh(m / 5)
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
    return(ddply(tab, col.name, function(x, size)
    {
        #s <- min(c(size, nrow(x)))
        if(nrow(x) <= size)
            return(x)
        else
            return(x[sample(1:nrow(x), size),])
    }, size = size))
}

load_attractors_from_gated_data <- function(dir)
{
    files <- list.files(dir, ".fcs")
	res <- NULL
	for(f in files)
	{
        population <- tail(strsplit(f, "_")[[1]], n = 1)
		fcs <- read.FCS(paste(dir, f, sep = "/"))
		tab <- convert_fcs(fcs)
        
        if(!all(pData(parameters(fcs))$desc == " "))
            colnames(tab) <- pData(parameters(fcs))$desc
        else
            colnames(tab) <- pData(parameters(fcs))$name
        #colnames(tab) <- gsub("-", ".", colnames(tab))
        tab <- as.matrix(tab)
        tab[tab < 0] <- 0
        tab <- as.data.frame(tab)
        
		tab <- cbind(tab, population)
		res <- rbind(res, tab)
	}
    
    downsampled.data <- downsample_by(res, "population", 1000)
    names(downsampled.data) <- gsub("population", "cellType", names(downsampled.data))
    
    #Change cellType to be numbers
	k <- unique(res$population)
	k <- data.frame(population = k, cellType = seq_along(k))
	res <- merge(res, k)
	res <- res[, grep("population", names(res), invert = T)]
    res <- ddply(res, ~cellType, colwise(median))
    
	return(list(downsampled.data = downsampled.data, tab.attractors = res, cellType_key = k))
}


get_highest_scoring_edges <- function(G)
{
    V(G)$highest_scoring_edge <- 0
    for(i in 1:vcount(G))
    {
        if(V(G)$type[i] == 2)
        {
            sel.edges <- incident(G, i)
            max.edge <- sel.edges[which.max(E(G)[sel.edges]$weight)]
            V(G)$highest_scoring_edge[i] <- max.edge
        }
    }
    return(G)
}




get_attractors_from_graph_clustering <- function(f_name, col.names)
{
    tab <- read.table(f_name, header = T, sep = "\t", check.names = F)
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

#This won't work if the different files have different markers
get_dataset_statistics_old <- function(dataset)
{
    graphs <- dataset$graphs
    temp <- NULL
    for(G in graphs)
    {
        V(G)$popsize.relative <- V(G)$popsize / sum(V(G)$popsize, na.rm = T)
        tab <- get_vertex_table(G)
        temp <- rbind(temp, tab)
    }
    max.vals <-  sapply(temp, function(x) {if(is.numeric(x)) return(max(x, na.rm  =T))})
    return(list(max.marker.vals = max.vals))
    
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




process_files <- function(files.list, G.attractors, tab.attractors, att.labels, col.names, ...)
{
    ret <- list(graphs = list(), clustered.data = list())
    
    for(f in files.list)
    {
        print(tab.attractors[1:5,])
        print(paste("Processing", f, sep = " "))
        tab <- read.table(f, header = T, sep = "\t", quote = "", check.names = F)
        tab <- tab[!apply(tab[, col.names], 1, function(x) {all(x == 0)}),]
        #tab <- filter_small_populations(tab)
        
        names(tab) <- gsub("cellType", "groups", names(tab))
        names(tab) <- gsub("^X", "", names(tab))
        ew_influence <- ceiling(length(col.names) / 3)
        res <- process_data(tab, G.attractors, tab.attractors,
            col.names = col.names, att.labels = att.labels, already.clustered = T, ew_influence = ew_influence, ...)
        G.complete <- get_highest_scoring_edges(res$G.complete)
        clustered.data <- my_load(gsub("txt$", "all_events.RData", f))
        clustered.data <- downsample_by(clustered.data, "cellType", 1000)
        
        ret$graphs[basename(f)] <- list(G.complete)
        ret$clustered.data[basename(f)] <- list(clustered.data)
        
        G.attractors <- res$G.attractors
    }
    ret <- c(ret, list(dataset.statistics = get_dataset_statistics(ret)))
    return(ret)
}

run_analysis_gated <- function(working.dir, ref.file, col.names, ...)
{
    files.list <- list.files(path = working.dir, pattern = "*.clustered.txt$")
    files.list <- files.list[files.list != ref.file]
    print(sprintf("Markers used for SCAFFoLD: %s", paste(col.names, collapse = ", ")))
    files.list <- c(ref.file, files.list)
    print(paste("Using as reference", files.list[1], sep = " "))
    files.list <- paste(working.dir, files.list, sep = "/")
    ref.dir <- paste(working.dir, "gated/", sep = "/")
    gated_data <- load_attractors_from_gated_data(ref.dir)
    
    tab.attractors <- gated_data$tab.attractors
    att.labels <- gated_data$cellType_key$population
    G.attractors <- NULL
    ret <- process_files(files.list, G.attractors, tab.attractors, att.labels, col.names, ...)
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
    ret <- process_files(files.list, G.attractors, tab.attractors, att.labels, col.names, ...)
    ret <- c(list(scaffold.col.names = col.names), ret)
    my_save(ret, paste(working.dir, sprintf("%s.scaffold", ref.file), sep = "/"))
    return(files.list)
}





load_existing_layout <- function(f_name)
{
	ref.data <- my_load(f_name)
    G <- ref.data$graphs[[1]]
	G <- induced.subgraph(G, V(G)$type == 1, impl = "copy_and_delete")
	tab <- get_vertex_table(G)
	V(G)$name <- 1:vcount(G)
	return(list(G.attractors = G, tab.attractors = tab))
}



run_analysis_existing <- function(working.dir, ref.file, col.names, ...)
{
    files.list <- list.files(path = working.dir, pattern = "*.clustered.txt$")
    print(sprintf("Markers used for SCAFFoLD: %s", paste(col.names, collapse = ", ")))
    print(paste("Using as reference", ref.file, sep = " "))
    files.list <- paste(working.dir, files.list, sep = "/")
    ref.file <- paste(working.dir, ref.file, sep = "/")
    
    l <- load_existing_layout(ref.file)
    tab.attractors <- l$tab.attractors
    G.attractors <- l$G.attractors
    att.labels <- V(G.attractors)$Label
    
    ret <- process_files(files.list, G.attractors, tab.attractors, att.labels, col.names, ...)
    ret <- c(list(scaffold.col.names = col.names), ret)
    my_save(ret, paste(working.dir, sprintf("%s.scaffold", basename(files.list[[1]])), sep = "/"))
    return(files.list)
}

    



######################################################################################################################################


TRASH <- function()
{
    
    load_attractors <- function(dir)
    {
        f <- paste(dir, "celltype_medians.txt", sep = "/")
        #if(file.exists(f))
        #{
        #    tab <- read.table(f, header = T, sep = "\t", check.names = F)
        #    names(tab) <- gsub("^X", "", names(tab))
        #    return(tab)
        #}
        #else
        {
            tab <- load_fcs_files(ref.dir)
            tab.attractors <- ddply(tab, ~cellType, colwise(median))
            write.table(tab.attractors, f, row.names = F, sep = "\t", quote = F)
            return(tab.attractors)
        }
    }
    
    
    load_fcs_files <- function(dir)
    {
        files <- list.files(dir, ".fcs")
        files <- grep("cluster.fcs|downsample.fcs|ungated.fcs", files, invert = T, value = T, ignore.case = T)
        res <- NULL
        for(f in files)
        {
            population <- tail(strsplit(f, "_")[[1]], n = 1)
            fcs <- read.FCS(paste(dir, f, sep = "/"))
            tab <- convert_fcs(fcs)
            
            if(!all(pData(parameters(fcs))$desc == " "))
            colnames(tab) <- pData(parameters(fcs))$desc
            else
            colnames(tab) <- pData(parameters(fcs))$name
            colnames(tab) <- gsub("-", ".", colnames(tab))
            tab <- as.matrix(tab)
            tab[tab < 0] <- 0
            tab <- as.data.frame(tab)
            
            tab <- cbind(tab, population)
            res <- rbind(res, tab)
        }
        k <- unique(res$population)
        k <- data.frame(population = k, cellType = seq_along(k))
        res <- merge(res, k)
        cat(k$population, file = paste(dir, "key.txt", sep = "/"), sep = "\n")
        res <- res[, grep("population", names(res), invert = T)]
        return(res)
        
    }
    
    


    
    
    MODE <- "combined_ungated"

    files.list <- list.files(".", "clustered.txt$")

    if(MODE == "gated") {
        print(paste("Using as reference", files.list[1], sep = " "))
        ref.dir <- "gated/"
        tab.attractors <- load_attractors(ref.dir)
        att.labels <- read.table(paste(ref.dir, "key.txt", sep = "/"), header = F, sep = "\t")[,1]
        G.attractors <- NULL
    } else if(MODE == "existing") {
        print(paste("Using as reference", list.files(".", "graphml")[[1]], sep = " "))
        l <- load_existing_layout(list.files(".", "graphml")[[1]])
        tab.attractors <- l$tab.attractors
        G.attractors <- l$G.attractors
        att.labels <- V(G.attractors)$name
    } else if(MODE == "unsupervised") {
        print(paste("Using as reference", files.list[1], sep = " "))
        tab <- read.table(files.list[1], header = T, sep = "\t", quote = "", check.names = F)
        temp <- get_attractors_from_graph_clustering(tab, col.names)
        tab.attractors <- temp$tab.attractors
        att.labels <- temp$att.labels
        G.attractors <- NULL
    } else if(MODE == "combined") {
        ref.dir <- "gated/"
        print(sprintf("Using as reference %s and %s", files.list[1], ref.dir))
        tab <- read.table(files.list[1], header = T, sep = "\t", quote = "", check.names = F)
        temp <- get_attractors_from_graph_clustering(tab, col.names)
        tab.attractors <- temp$tab.attractors
        att.labels <- temp$att.labels
        temp.attractors <- load_attractors(ref.dir)
        common.cols <- intersect(colnames(tab.attractors), colnames(temp.attractors))
        
        tab.attractors <- tab.attractors[, common.cols]
        temp.attractors <- temp.attractors[, common.cols]
        temp.att.labels <- read.table(paste(ref.dir, "key.txt", sep = "/"), header = F, sep = "\t")[,1]
        temp.attractors$cellType <- seq(from = max(as.numeric(tab.attractors$cellType)) + 1, length = nrow(temp.attractors))
        tab.attractors <- rbind(tab.attractors, temp.attractors)
        att.labels <- c(att.labels, temp.att.labels)
        G.attractors <- NULL
    } else if(MODE == "combined_ungated") {
        ref.dir <- "gated/"
        ungated <- "ungated/Healthies_not_in_gated.fcs.clustered.txt"
        print(sprintf("Using as reference %s and %s", ungated, ref.dir))
        tab <- read.table(ungated, header = T, sep = "\t", quote = "", check.names = F)
        temp <- get_attractors_from_graph_clustering(tab, col.names)
        tab.attractors <- temp$tab.attractors
        att.labels <- temp$att.labels
        temp.attractors <- load_attractors(ref.dir)
        common.cols <- intersect(colnames(tab.attractors), colnames(temp.attractors))
        
        tab.attractors <- tab.attractors[, common.cols]
        temp.attractors <- temp.attractors[, common.cols]
        temp.att.labels <- read.table(paste(ref.dir, "key.txt", sep = "/"), header = F, sep = "\t")[,1]
        temp.attractors$cellType <- seq(from = max(as.numeric(tab.attractors$cellType)) + 1, length = nrow(temp.attractors))
        tab.attractors <- rbind(tab.attractors, temp.attractors)
        att.labels <- c(att.labels, temp.att.labels)
        G.attractors <- NULL
    }




    for(f in files.list)
    {
        print(paste("Processing", f, sep = " "))
        outname <- paste(strsplit(f, "\\.")[[1]][1], "graphml", sep = ".")
        tab <- read.table(f, header = T, sep = "\t", quote = "", check.names = F)
        #Filter out cluster that have all 0's '
        this.col.names <- col.names[col.names %in% names(tab)]
        tab <- tab[!apply(tab[, this.col.names], 1, function(x) {all(x == 0)}),]
        tab <- filter_small_populations(tab)
        
        names(tab) <- gsub("cellType", "groups", names(tab))
        names(tab) <- gsub("^X", "", names(tab))
        res <- process_data(tab, G.attractors, tab.attractors, 
                            col.names = this.col.names, att.labels = att.labels, already.clustered = T, outname = outname)
        G.attractors <- res$G.attractors
    }


}

