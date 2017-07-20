run_citrus_analysis <- function(citrus.features, endpoint, working.directory, model.type) {
    family <- NULL
    
    if(is.character(endpoint))
        family <- "classification"
    else
        family <- "continuoous"

    citrus.res <- citrus::citrus.endpointRegress(
        model.type,
        citrus.foldFeatureSet = citrus.features,
        labels = endpoint,
        family = family
    )

    plot(
        citrus.res,
        working.directory,
        citrus.foldClustering = NULL,
        citrus.foldFeatureSet = citrus.features,
        citrus.combinedFCSSet = NULL,
        "stratifyingFeatures"
    )
    return(citrus.res)
    
}


calculate_abundance_features <- function(input.tab) {
    col.names <- grep("popsize@", names(input.tab), value = T)
    s <- strsplit(col.names, "@")
    
    m <- t(as.matrix(input.tab[, col.names]))
    
    row.names(m) <- sapply(s, "[", 2)
    v <- unique(sapply(s, "[", 1))
    colnames(m) <- paste("cluster", 1:ncol(m), v)
    m[is.na(m)] <- 0
    m <- m / rowSums(m)
    
    ret <- list(allFeatures = m, nFolds = 1)
    
    return(ret)
    
    
}

calculate_cluster_features <- function(input.tab, features.type = c("abundance", "expression"), baseline = NULL) {
    features.type <- match.arg(features.type)
    ret <- NULL
    
    if(features.type == "abundance") 
        ret <- calculate_abundance_features(input.tab)
    return(ret)
}

run_citrus <- function(working.directory, input.tab, 
                       features.type = c("abundance", "expression"), endpoint, model.type, baseline = NULL) {
    citrus.features <- calculate_cluster_features(input.tab, features.type, baseline)
    run_citrus_analysis(citrus.features, endpoint, working.directory, model.type)
}