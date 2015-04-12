options(stringsAsFactors = F)


plot_cluster <- function(data, clusters, graph.name, col.names, pool.cluster.data, plot.type)
{
    G <- data$graphs[[graph.name]]
    gated_data <- data$landmarks.data
    clustered_data <- data$clustered.data[[graph.name]]
    #print(gated_data[1:10,])
    #print(clustered_data[1:10,])
    names(clustered_data) <- gsub("^X", "", names(clustered_data))
    names(gated_data) <- gsub("^X", "", names(gated_data))
    
    #This only works if the col.names are actually present in the clustered.data
    #TODO: figure out a consistent way to deal with panel mismatches
    
    common.names <- col.names[(col.names %in% names(clustered_data)) & (col.names %in% names(gated_data))]
    clustered_data <- clustered_data[, c(col.names, "cellType")]
    gated_data <- gated_data[, c(common.names, "cellType")]
    gated_data <- scaffold:::add_missing_columns(gated_data, col.names, fill.data = NA)
    #Select only the landmark nodes that are connected to these clusters
    land <- V(G)[nei(V(G)$Label %in% clusters)]$Label
    land <- V(G)[(V(G)$Label %in% land) & V(G)$type == 1]$Label
    temp <- gated_data[gated_data$cellType %in% land,]
    clus.num <- as.numeric(gsub("c", "", clusters))
    temp.clustered <- clustered_data[clustered_data$cellType %in% clus.num, ]
    if(pool.cluster.data)
        temp.clustered$cellType <- "Clusters"
    temp <- rbind(temp, temp.clustered)
    temp <- melt(temp, id.vars = "cellType")
    temp$variable <- as.factor(temp$variable)
    if(plot.type == "Density")
        p <- ggplot(aes(x = value, color = cellType), data = temp) + geom_density() + facet_wrap(~variable, scales = "free")
    else if(plot.type == "Boxplot")
        p <- ggplot(aes(x = variable, fill = cellType, y = value), data = temp) + geom_boxplot()
    return(p)
}











