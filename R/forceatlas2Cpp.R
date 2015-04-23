options(stringsAsFactors = F)


adaptive_expand <- function(G, max.iter)
{
    print("Starting adaptive expansion")
    x <- V(G)$x
    y <- V(G)$y
    m <- cbind(x, y)
    ss <- outer(V(G)$size, V(G)$size, "+")
    
    for(i in 1:max.iter)
    {
        dd <- as.matrix(dist(m), method = "euclidean")
        dd <- dd - ss
        dd <- dd[upper.tri(dd)]
        if(all(dd >= 0))
            break
        else
            m <- m * 1.2
    }
    
    print(sprintf("Expansion stopped at iteration: %d", i))
    V(G)$x <- m[, "x"]
    V(G)$y <- m[, "y"]
    
    return(G)
}



layout.forceatlas2 <- function(G, ew_influence = 1, kgrav = 1, iter = 1000, prevent.overlap = FALSE, fixed = rep(FALSE, vcount(G)), stopping_tolerance = 0.001, barnes_hut = FALSE)
{
    if(vcount(G) >= 2000)
        barnes_hut <- TRUE
    if(vcount(G) > 2000)
        stopping_tolerance <- 0.01
    else if(vcount(G) > 800)
        stopping_tolerance <- 0.005
    else
        stopping_tolerance <- 0.001

	if(is.null(get.vertex.attribute(G, "x")))
	{
       lay <- cbind(x = rnorm(vcount(G)), y = rnorm(vcount(G)))
	}
	else
    {
        lay <- cbind(x = V(G)$x, y = V(G)$y)
    }
		

	#This is only used with prevent.overlap
	if(is.null(get.vertex.attribute(G, "size")))
		V(G)$size <- rep(10, vcount(G))
	mass <- 1 + degree(G)
	F_att <- (E(G)$weight ^ ew_influence)
    edge_list <- get.edgelist(G, names = F) - 1 #This is gonna be used in the C code where the indexing is 0-based
    
	avg_displ <- numeric(iter)
    max_displ <- numeric(iter)

	print(system.time(layout_forceatlas2Cpp(lay, F_att, mass, V(G)$size, edge_list, avg_displ,
        kgrav,  iter, prevent.overlap, fixed, max_displ, stopping_tolerance, barnes_hut)))

	return(list(lay = lay, avg_displ = avg_displ, max_displ = max_displ))
}

complete.forceatlas2 <- function(G, first.iter = 1000, overlap.iter, overlap_method = NULL, ...)
{
 
    print("First iteration")
	ret <- layout.forceatlas2(G, prevent.overlap = FALSE, iter = first.iter, ...)
    lay <- ret$lay
    #plot(ret$avg_displ, type = "l")
    #lines(ret$max_displ, col = "red")
    G <- set.vertex.attribute(G, name = "x", value = lay[, 1])
	G <- set.vertex.attribute(G, name = "y", value = lay[, 2])
    if(!is.null(overlap_method))
    {
        if(overlap_method == "repel")
        {
            print("Second iteration with prevent overalp")
	        ret <- layout.forceatlas2(G, prevent.overlap = TRUE, iter = overlap.iter, ...)
	        lay <- ret$lay
            if(any(is.na(lay)))
            {
                print("Prevent overlap iteration failed")
            }
            #plot(ret$avg_displ, type = "l")
            #lines(ret$max_displ, col = "red")
            else
            {
                G <- set.vertex.attribute(G, name = "x", value = lay[, 1])
	            G <- set.vertex.attribute(G, name = "y", value = lay[, 2])
            }
        }
        else if(overlap_method == "expand")
            G <- adaptive_expand(G, overlap.iter)
    }
	return(G)
}
	


							
	

	
	
	
	
	
