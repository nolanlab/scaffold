options(stringsAsFactors = F)


process_file <- function(f, wd, col.names, num_clusters, num_samples, asinh.cofactor)
{
    setwd(wd)
    
    cluster_data <- function(tab, col.names, k, algorithm = "", ...)
    {
        m <- as.matrix(tab[, col.names])
        
        if(algorithm == "clara")
        {
            print("Performing clara clustering")
            groups <- clara(m, k, ...)$clustering
        }
        
        else if(algorithm == "hierarchical")
        {
            print("Performing hierarchical clustering")
            dend <- hclust(dist(m), ...)
            groups <- cutree(dend, k)
        }
        
        print("Clustering done")
        tab <- cbind(tab, groups)
        return(tab)
    }

    fcs.file <- read.FCS(f)
    orig.data <- exprs(fcs.file)
    tab <- convert_fcs(fcs.file, asinh.cofactor)
    colnames(tab) <- pData(parameters(fcs.file))$desc

    if(any(is.na(colnames(tab))))
    {
        w <- is.na(colnames(tab))
        colnames(tab)[w] <- pData(parameters(fcs.file))$name[w]
    }
    
    
    tab <- as.matrix(tab)
    tab[tab < 0] <- 0
    tab <- as.data.frame(tab)
    
    

    m <- cluster_data(tab, col.names, k = num_clusters, algorithm = "clara", sampsize = min(nrow(tab), 1000), samples = num_samples)
    colnames(m) <- gsub("groups", "cellType", colnames(m))
    orig.data <- cbind(orig.data, cellType = m[, "cellType"])
    
    tab.medians <- ddply(m, ~cellType, colwise(median))
    
    pop.size <- ddply(m, ~cellType, nrow)
    
    temp <- data.frame(tab.medians, sample = f, popsize = pop.size[tab.medians$cellType, "V1"], check.names = F, stringsAsFactors = FALSE)
    
    colnames(temp) <- gsub("^X", "", colnames(temp))
    m <- data.frame(m, check.names = F)
    orig.data <- data.frame(orig.data, stringsAsFactors = FALSE, check.names = FALSE)
    colnames(orig.data) <- gsub("^X", "", colnames(orig.data))
    colnames(m) <- gsub("^X", "", colnames(m))
    
    write.table(temp, paste(f, ".clustered.txt", sep = ""), row.names = F, sep = "\t", quote = F)
    my_save(m, paste(f, ".clustered.all_events.RData", sep = ""))
    #my_save(orig.data, paste(f, ".clustered.all_events.orig_data.RData", sep = ""))
}

cluster_fcs_files_in_dir <- function(wd, col.names, num_clusters, num_samples, asinh.cofactor)
{
    files.list <- list.files(path = wd, pattern = "*.fcs$")
    lapply(files.list, process_file, wd = wd, num_clusters = num_clusters, num_samples = num_samples, asinh.cofactor = asinh.cofactor)
    return(files.list)
}


