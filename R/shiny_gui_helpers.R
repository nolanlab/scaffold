

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

get_vertex_size <- function(sc.data, sel.graph, figure.width)
{
    G <- sc.data$graphs[[sel.graph]]
    ret <- V(G)$popsize / sum(V(G)$popsize, na.rm = T)
    ret <- rescale_size(60, 2, sc.data$dataset.statistics$max.marker.vals[["popsize.relative"]], ret)
    ret[V(G)$type == 1] <- 8
    return(ret)
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


get_graph <- function(sc.data, sel.graph, trans_to_apply)
{
    G <- sc.data$graphs[[sel.graph]]
    edges <- get.edgelist(G, names = F) - 1
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
    
    vertex.size <- get_vertex_size(sc.data, sel.graph, svg.width)
    edges <- cbind(edges, x1 = x[edges[, "source"] + 1], x2 = x[edges[, "target"] + 1])
    edges <- cbind(edges, y1 = y[edges[, "source"] + 1], y2 = y[edges[, "target"] + 1])
    edges <- cbind(edges, id = 1:nrow(edges))
    edges <- cbind(edges, is_highest_scoring = 0)
    #Set as true for the highest scoring edges of type 2 vertices
    edges[, "is_highest_scoring"][V(G)$highest_scoring_edge[V(G)$type == 2]] <- 1
    print(G)
    list(names = V(G)$Label, size = vertex.size / trans$scaling, type = V(G)$type, highest_scoring_edge = V(G)$highest_scoring_edge, links = edges, X = x, Y = y, trans_to_apply = trans_to_apply)
}

get_color_for_marker <- function(sc.data, sel.marker, sel.graph, color.scaling)
{
    G <- sc.data$graphs[[sel.graph]]
    if(sel.marker == "Default")
    return(rep("", vcount(G)))
    else
    {
        norm.factor <- NULL
        v <- get.vertex.attribute(G, sel.marker)
        if(color.scaling  == "global")
        norm.factor <- sc.data$dataset.statistics$max.marker.vals[[sel.marker]]
        else if(color.scaling == "local")
        norm.factor <- max(v)
        
        a = "#E7E7E7"
        b = "#E71601"
        f <- colorRamp(c(a, b), interpolate = "linear")
        
        v <- f(v / norm.factor) #colorRamp needs an argument in the range [0, 1]
        v <- apply(v, 1, function(x) {sprintf("rgb(%s)", paste(round(x), collapse = ","))})
        return(v)
    }
}

get_numeric_vertex_attributes <- function(sc.data, sel.graph)
{
    G <- sc.data$graphs[[sel.graph]]
    d <- get.data.frame(G, what = "vertices")
    num <- sapply(d, function(x) {is.numeric(x) && !any(is.na(x))})
    v <- list.vertex.attributes(G)[num]
    exclude <- c("x", "y", "cellType", "type", "groups", "popsize", "r", "g", "b", "size", "DNA1", "DNA2", "BC1", "BC2", "BC3", "BC4", "BC5", "BC6", "Time", "Cell_length", "Cisplatin", "beadDist", "highest_scoring_edge")
    return(v[!(v %in% exclude)])
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





