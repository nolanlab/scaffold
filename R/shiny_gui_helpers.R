

my_load <- function(f_name)
{
	con <- file(f_name, "rb")
	retval <- unserialize(con)
	close(con)
	return(retval)
}


rescale_size <- function(max.size, min.size, max.val, x)
{
    return(((max.size - min.size) * x) / max.val + min.size);
}

get_vertex_size <- function(sc.data, sel.graph, figure.width, node.size.attr, min.node.size, max.node.size, landmark.node.size)
{
    G <- sc.data$graphs[[sel.graph]]
    size.attr <- get.vertex.attribute(G, node.size.attr)
    ret <- size.attr / sum(size.attr, na.rm = T)
    ret <- rescale_size(max.node.size, min.node.size, sc.data$dataset.statistics$max.marker.vals[["popsize.relative"]], ret)
    ret[V(G)$type == 1] <- landmark.node.size
    return(ret)
}


get_sample_names <- function(sc.data, sel.graph) 
{
    G <- sc.data$graphs[[sel.graph]]
    s <- list.vertex.attributes(G)
    s <- grep("@", s, value = T)
    ret <- sapply(strsplit(s, "@"), function (x) {x[[2]]})
    return(unique(ret))
}

combine_marker_sample_name <- function(sel.marker, active.sample)
{
    if(active.sample == "All" || active.sample == "Absolute" || sel.marker == "Default")
        return(sel.marker)
    else
        return(paste(sel.marker, active.sample, sep = "@"))
    
}


get_graph_centering_transform <- function(x, y, svg.width, svg.height)
{
    padding <- 50
    G.width <- max(x) - min(x)
    G.height <- max(y) - min(y)
    scaling <- max(c(G.width / (svg.width - (padding * 2)), G.height / (svg.height - (padding * 2))))
    
    x <- x / scaling
    y <- y / scaling
    
    offset.y <- min(y) - padding
    graph.x.center <- (min(x) + max(x)) / 2
    offset.x <- graph.x.center - (svg.width / 2)
    
    return(list(offset.x = offset.x, offset.y = offset.y, scaling = scaling))
    
    
}

get_graph_table <- function(sc.data, sel.graph)
{
    G <- sc.data$graphs[[sel.graph]]
    ret <- get.data.frame(G, what = c("vertices"))
    return(ret)
}


get_summary_table <- function(sc.data, sel.graph, sel.nodes)
{
    G <- sc.data$graphs[[sel.graph]]
    col.names <- get_numeric_vertex_attributes(sc.data, sel.graph)
    tab <- get.data.frame(G, what = "vertices")
    temp <-tab[tab$Label %in% sel.nodes,]
    ret <- temp[, col.names]    
    ret <- rbind(ret, apply(ret, 2, median, na.rm = T))
    popsize <- data.frame(Cells = temp$popsize, Percentage = temp$popsize / sum(tab$popsize[tab$type == 2]))
    popsize <- rbind(popsize, colSums(popsize))
    ret <- cbind(popsize, ret)
    ret <- data.frame(Label = c(temp$Label, "Summary"), ret)
    ret$Percentage <- signif(ret$Percentage * 100, digits = 4)
    return(ret)
}


export_clusters <- function(working.dir, sel.graph, sel.nodes)
{
    d <- gsub(".txt$", ".all_events.RData", sel.graph)
    d <- file.path(working.dir, d)
    d <- my_load(d)
    clus <- as.numeric(gsub("c", "", sel.nodes))
    d <- d[d$cellType %in% clus,]
    f <- flowFrame(as.matrix(d))
    p <- sprintf("scaffold_export_%s_", gsub(".fcs.clustered.txt", "", sel.graph))
    outname <- tempfile(pattern = p, tmpdir = working.dir, fileext = ".fcs")
    print(outname)
    write.FCS(f, outname)
}

get_graph <- function(sc.data, sel.graph, trans_to_apply, node.size.attr, min.node.size, max.node.size, landmark.node.size)
{
    G <- sc.data$graphs[[sel.graph]]
    edges <- data.frame(get.edgelist(G, names = F) - 1)
    colnames(edges) <- c("source", "target")
    svg.width <- 1200
    svg.height <- 800
    svg.center <- c(svg.width / 2, svg.height / 2)
    
    x <- V(G)$x
    y <- V(G)$y
    
    y <- -1 * y
    x <- x + abs(min(x))
    y <- y + abs(min(y))
    num.landmarks <- sum(V(G)$type == 1)
    trans <- get_graph_centering_transform(x[V(G)$type == 1], y[V(G)$type == 1], svg.width, svg.height)
    
    x <- (x / trans$scaling) - trans$offset.x
    y <- (y / trans$scaling) - trans$offset.y
    
    vertex.size <- get_vertex_size(sc.data, sel.graph, svg.width, node.size.attr, min.node.size, max.node.size, landmark.node.size)
    edges <- cbind(edges, x1 = x[edges[, "source"] + 1], x2 = x[edges[, "target"] + 1])
    edges <- cbind(edges, y1 = y[edges[, "source"] + 1], y2 = y[edges[, "target"] + 1])
    edges <- cbind(edges, id = 1:nrow(edges))
    edges <- cbind(edges, is_highest_scoring = 0)
    edges <- cbind(edges, edge_type = "")
    #Set as true for the highest scoring edges of type 2 vertices
    edges[, "is_highest_scoring"][V(G)$highest_scoring_edge[V(G)$type == 2]] <- 1
    if("edge_type" %in% list.edge.attributes(G)) #Old graphs did not have this
        edges[, "edge_type"] <- E(G)$edge_type
    #print(G)
    ret <- list(names = V(G)$Label, size = vertex.size / trans$scaling, type = V(G)$type, highest_scoring_edge = V(G)$highest_scoring_edge, X = x, Y = y, trans_to_apply = trans_to_apply)
    ret <- c(ret, edges = list(edges))
    return(ret)
}

get_color_for_marker <- function(sc.data, sel.marker, rel.to.sample, sel.graph, active.sample, color.scaling)
{
    G <- sc.data$graphs[[sel.graph]]
    if(sel.marker == "Default")
    {
        ret <- rep("#4F93DE", vcount(G))
        ret[V(G)$type == 1] <- "#FF7580"
        return(ret)
    }
    else
    {
        norm.factor <- NULL
        v <- get.vertex.attribute(G, combine_marker_sample_name(sel.marker, active.sample))
        
        a = "#E7E7E7"
        b = "#E71601"
        f <- colorRamp(c(a, b), interpolate = "linear")
        #browser()
        if(rel.to.sample != "Absolute")
        {
            rel.to.marker <- combine_marker_sample_name(sel.marker, rel.to.sample)
            #v <- v / (get.vertex.attribute(G, rel.to.marker) + 0.001)
            v <- v / (get.vertex.attribute(G, rel.to.marker))
            v[is.infinite(v)] <- NA
            print("Forcing local color scaling")
            color.scaling <- "local"
        }
        if(color.scaling  == "global")
            norm.factor <- sc.data$dataset.statistics$max.marker.vals[[sel.marker]]
        else if(color.scaling == "local")
            norm.factor <- max(v, na.rm = T)
        v <- f(v / norm.factor) #colorRamp needs an argument in the range [0, 1]
        v <- apply(v, 1, function(x) {sprintf("rgb(%s)", paste(round(x), collapse = ","))})
        return(v)
    }
}

get_numeric_vertex_attributes <- function(sc.data, sel.graph)
{
    G <- sc.data$graphs[[sel.graph]]
    d <- get.data.frame(G, what = "vertices")
    #Don't consider attributes which are only present in the landmarks
    d <- d[d$type == 2,]
    num <- sapply(d, function(x) {is.numeric(x) && !any(is.na(x))})
    v <- list.vertex.attributes(G)[num]
    exclude <- c("x", "y", "cellType", "type", "groups", "r", "g", "b", "size", "DNA1", "DNA2", "BC1", "BC2", "BC3", "BC4", "BC5", "BC6", "Time", "Cell_length", "Cisplatin", "beadDist", "highest_scoring_edge")
    return(v[!(v %in% exclude)])
}

get_number_of_cells_per_landmark <- function(sc.data, sel.graph)
{
    G <- sc.data$graphs[[sel.graph]]
    land <- V(G)[V(G)$type == 1]$Label
    ee <- get.edgelist(G)
    ee <- ee[V(G)[V(G)$type == 2]$highest_scoring_edge,]
    vv <- V(G)[as.numeric(ee[,2])]
    popsize <- V(G)[vv]$popsize
    dd <- data.frame(Landmark = ee[,1], popsize)
    dd <- ddply(dd, ~Landmark, function(x) {sum(x["popsize"])})
    dd <- cbind(dd, Percentage = dd$V1 / sum(dd$V1))
    names(dd) <- c("Landmark", "Cells", "Percentage")
    dd$Percentage <- signif(dd$Percentage * 100, digits = 4)
    return(dd)
}

get_fcs_col_names <- function(working.directory, f.name)
{
    fcs.file <- read.FCS(paste(working.directory, f.name, sep = "/"))
    ret <- as.vector(pData(parameters(fcs.file))$desc)
    
    if(any(is.na(ret)))
    {
        w <- is.na(ret)
        ret[w] <- as.vector(pData(parameters(fcs.file))$name[w])
    }
    
    return(ret)
}





#get_pubmed_references <- function(sc.data, sel.graph, node.label)
#{
#    G <- sc.data$graphs[[sel.graph]]
#    ret <- ""
#    if("desc" %in% list.vertex.attributes(G))
#    {
#        ret <- sprintf("List of references for landmark %s:<br>", gsub(".fcs", "", node.label))
#        v <- strsplit(V(G)[V(G)$Label == node.label]$desc, ",")[[1]]
#        v <- paste(sapply(v, function(x) {sprintf("PMID: <a href='http://www.ncbi.nlm.nih.gov/pubmed/%s' target='_blank'>%s</a><br>", x, x) }), collapse = "")
#        ret <- paste(ret, v, sep = "")
#    }
#    return(HTML(ret))
#}





