#' @import plyr
#' @import flowCore
#' @import ggplot2
#' @import reshape
#' @import igraph

options(stringsAsFactors = F)
#library(plyr)
#library(flowCore)
#library(ggplot2)
#library(reshape)
#library(igraph)

plot_cluster <- function(data, cluster, graph.name, col.names)
{
    G <- data$graphs[[graph.name]]
    gated_data <- data$landmarks.data
    clustered_data <- data$clustered.data[[graph.name]]
    print(gated_data[1:10,])
    print(clustered_data[1:10,])
    


    names(clustered_data) <- gsub("^X", "", names(clustered_data))
    names(gated_data) <- gsub("^X", "", names(gated_data))
    clustered_data <- clustered_data[, c(col.names, "cellType")]
    gated_data <- gated_data[, c(col.names, "cellType")]
    print(gated_data[1:5,])
    land <- V(G)[nei(V(G)$Label == cluster)]$Label
    temp <- gated_data[gated_data$cellType %in% land,]
    clus.num <- as.numeric(gsub("c", "", cluster))
  
    temp <- rbind(temp, clustered_data[clustered_data$cellType == clus.num,])
    temp <- melt(temp, id.vars = "cellType")
    temp$variable <- as.factor(temp$variable)
    p <- ggplot(aes(x = value, color = cellType), data = temp) + geom_density() + facet_wrap(~variable, scales = "free")
    return(p)
}











