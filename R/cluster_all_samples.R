options(stringsAsFactors = F)


get_stats_by_sample <- function(tab) 
{
    tab.medians <- ddply(tab, ~cellType, colwise(median, is.numeric))
    tab.medians.by.sample <- ddply(tab, ~cellType * sample, colwise(median, is.numeric))
    pop.size <- ddply(tab, ~cellType, nrow)
    names(pop.size) <- gsub("V1", "popsize", names(pop.size))
    pop.size.by.sample <- ddply(tab, ~cellType * sample, nrow)
    names(pop.size.by.sample) <- gsub("V1", "popsize", names(pop.size.by.sample))
    tab.medians <- merge(tab.medians, pop.size, by = "cellType")
    tab.medians.by.sample <- merge(tab.medians.by.sample, pop.size.by.sample, by = c("cellType", "sample"), all.x = T)
    
    
    #Rotate the by.sample table
    temp <- melt(tab.medians.by.sample, id = c("cellType", "sample"))       
    temp$variable <- paste(temp$variable, temp$sample, sep = "@")
    temp$sample <- NULL
    temp <- cast(temp, cellType~variable)
    
    
    ret <- merge(tab.medians, temp, by = "cellType", all.x = T)
    
    return(ret)
    
}

process_files_groups <- function(files, wd, col.names, num_clusters, num_samples, asinh.cofactor, downsample.to, output_type, output.dir)
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
    tab <- NULL
    orig.data <- NULL
    
    for(f in files)
    {
        print(f)
        fcs.file <- read.FCS(f)
        temp.orig.data <- exprs(fcs.file)
        temp.tab <- convert_fcs(fcs.file, asinh.cofactor)
        colnames(temp.tab) <- pData(parameters(fcs.file))$desc
        
        if(any(is.na(colnames(temp.tab))))
        {
            w <- is.na(colnames(temp.tab))
            colnames(temp.tab)[w] <- pData(parameters(fcs.file))$name[w]
        }
        
        temp.tab <- as.matrix(temp.tab)
        temp.tab[temp.tab < 0] <- 0
        
        if(downsample.to > 0)
        {
            x <- NULL
            if(nrow(temp.tab) <= downsample.to)
            {
                print("Number of events smaller than downsampling target, taking all events")
                x <- 1:nrow(temp.tab)
            }
            else
            {
                print(sprintf("Predownsampling to %d events", downsample.to))
                x <- sample(1:nrow(temp.tab), size = downsample.to)
            }
            temp.tab <- temp.tab[x,]
            temp.orig.data <- temp.orig.data[x,]
        }
        
        temp.tab <- as.data.frame(temp.tab, check.names = F, stringsAsFactors = F)
        
        temp.tab <- data.frame(temp.tab, sample = f, check.names = F, stringsAsFactors = F)
        temp.orig.data <- data.frame(temp.orig.data, sample = f, check.names = F, stringsAsFactors = F)
        tab <- rbind(tab, temp.tab)
        orig.data <- rbind(orig.data, temp.orig.data)
    }
    
    m <- cluster_data(tab, col.names, k = num_clusters, algorithm = "clara", sampsize = min(nrow(tab), 1000), samples = num_samples)
    colnames(m) <- gsub("groups", "cellType", colnames(m))
    orig.data <- cbind(orig.data, cellType = m[, "cellType"])
    
    temp <- get_stats_by_sample(m)
    
    #colnames(temp) <- gsub("^X", "", colnames(temp))
    m <- data.frame(m, check.names = F)
    orig.data <- data.frame(orig.data, stringsAsFactors = FALSE, check.names = FALSE)
    #colnames(orig.data) <- gsub("^X", "", colnames(orig.data))
    #colnames(m) <- gsub("^X", "", colnames(m))
    
    write_clustering_output(f, temp, m, output_type, output.dir)
    #my_save(orig.data, paste(f, ".clustered.all_events.orig_data.RData", sep = ""))
}



process_file <- function(f, wd, col.names, num_clusters, num_samples, asinh.cofactor, output_type, output.dir)
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
    
    #colnames(temp) <- gsub("^X", "", colnames(temp))
    m <- data.frame(m, check.names = F)
    orig.data <- data.frame(orig.data, stringsAsFactors = FALSE, check.names = FALSE)
    #colnames(orig.data) <- gsub("^X", "", colnames(orig.data))
    #colnames(m) <- gsub("^X", "", colnames(m))
    
    
    write_clustering_output(f, temp, m, output_type, output.dir)
    #my_save(orig.data, paste(f, ".clustered.all_events.orig_data.RData", sep = ""))
}

write_clustering_output <- function(base.name, tab.medians, clustered.data, output.type, output.dir)
{
    if(output.type == "legacy")
    {
        write.table(tab.medians, paste(base.name, ".clustered.txt", sep = ""), row.names = F, sep = "\t", quote = F)
        my_save(clustered.data, paste(base.name, ".clustered.all_events.RData", sep = ""))
    }
    else if(output.type == "directory")
    {
        clusters.dir <- "clusters"
        clustered.data.dir <- "clustered.data"
        txt.file.name <- paste(base.name, ".clustered.txt", sep = "")
        full.path <- file.path(output.dir, clusters.dir, clustered.data.dir, txt.file.name)
        dir.create(full.path, recursive = T)
        
        write.table(tab.medians, file.path(output.dir, clusters.dir, txt.file.name, sep = ""), 
                    row.names = F, sep = "\t", quote = F)
        ddply(clustered.data, ~cellType, function(x) {
            saveRDS(x, file = file.path(full.path, sprintf("cluster_%d.RData", x$cellType[1])))
        })
    }
}

#' @export
cluster_fcs_files_in_dir <- function(wd, num.cores, col.names, num_clusters, num_samples, asinh.cofactor, output_type = "legacy")
{
    files.list <- list.files(path = wd, pattern = "*.fcs$")
    output.dir <- NULL
    if(output_type == "directory")
        output.dir <- sprintf("%s.scaffold", gsub(".fcs$", "", files.list[[1]]))
    
    parallel::mclapply(files.list, mc.cores = num.cores, mc.preschedule = FALSE,
             process_file, wd = wd, col.names = col.names, num_clusters = num_clusters, 
             num_samples = num_samples, asinh.cofactor = asinh.cofactor, output_type = output_type, output.dir = output.dir)
    return(files.list)
}

#' @export
cluster_fcs_files_groups <- function(wd, files.list, num.cores, col.names, num_clusters, num_samples, 
                                     asinh.cofactor, downsample.to, output_type = "legacy")
{
    #lapply(files.list,
    #                   process_files_groups, wd = wd, col.names = col.names, num_clusters = num_clusters, num_samples = num_samples, 
    #                   asinh.cofactor = asinh.cofactor, downsample.to = downsample.to)
    
    output.dir <- NULL
    if(output_type == "directory")
        output.dir <- sprintf("%s.scaffold", gsub(".fcs$", "", names(files.list)[1]))
    
    parallel::mclapply(files.list, mc.cores = num.cores, mc.preschedule = FALSE,
                       process_files_groups, wd = wd, col.names = col.names, num_clusters = num_clusters, num_samples = num_samples, 
                       asinh.cofactor = asinh.cofactor, downsample.to = downsample.to, output_type = output_type, output.dir = output.dir)
    
    return(files.list)
}



