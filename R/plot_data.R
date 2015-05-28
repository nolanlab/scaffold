options(stringsAsFactors = F)



density_scatterplot  <- function(tab, x_name, y_name, grouping)
{
    m <- ddply(tab, grouping, function(m, x_name, y_name)
    {
        colramp <- grDevices::colorRampPalette(c("black", "red", "yellow"))
        dens.col <- grDevices::densCols(m[, x_name], m[, y_name], colramp = colramp)
        return(data.frame(m, dens.col = dens.col))
    }, x_name = x_name, y_name = y_name)
    
    maxx <- max(m[, x_name], na.rm = T) + 0.5
    maxy <- max(m[, y_name], na.rm = T) + 0.5
    
    (p <- ggplot(aes_string(x = x_name, y = y_name, color = "dens.col", size = 1), data = m)
     + facet_wrap(grouping)
     + geom_point()
     + scale_colour_identity() 
     + scale_size_identity()
     + xlim(0, maxx)
     + ylim(0, maxy)
    )
    
    return(p)
}


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
    p <- NULL
    if(plot.type == "Scatterplot")
    {
        p <- density_scatterplot(temp, x_name = col.names[1], y_name = col.names[2], grouping = "cellType")
    }
    else
    {
        temp <- melt(temp, id.vars = "cellType")
        temp$variable <- as.factor(temp$variable)
        if(plot.type == "Density")
            p <- ggplot(aes(x = value, color = cellType), data = temp) + geom_density() + facet_wrap(~variable, scales = "free")
        else if(plot.type == "Boxplot")
            p <- ggplot(aes(x = variable, fill = cellType, y = value), data = temp) + geom_boxplot()
    }
    return(p)
}











