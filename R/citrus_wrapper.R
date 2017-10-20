run_citrus_analysis <- function(citrus.features, endpoint, working.directory, model.type) {
    family <- NULL
    
    if(is.character(endpoint) || is.factor(endpoint)) {
        family <- "classification"
        endpoint <- as.factor(endpoint)
        
    }
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

# The matrix has to contain a single feature for multiple samples
transpose_feature_matrix <- function(m) {
    s <- strsplit(colnames(m), "@")
    v <- unique(sapply(s, "[", 1))
    stopifnot(length(v) == 1)
    
    m <- t(m)
    colnames(m) <- paste("cluster", 1:ncol(m), v, sep = "_")
    row.names(m) <- sapply(s, "[", 2)
    
    return(m)
}


reshape_cluster_features <- function(input.tab, features) {
    col.names <- sapply(features, paste, "@", sep = "")
    col.names <- paste(col.names, collapse = "|")
    col.names <- grep(col.names, names(input.tab), value = T)
    
    m <- as.matrix(input.tab[, col.names])
    
    
    ret <- lapply(features, function(s) {
        temp <- m[, grep(s, colnames(m))]
        temp[is.na(temp)] <- 0
        
        if(s == "popsize") {
            temp <- t(temp)
            temp <- temp / rowSums(temp)
            temp <- t(temp)
        }
        
        temp <- transpose_feature_matrix(temp)
        temp[!is.finite(temp)] <- 0
        
        return(temp)
        
    })
    ret <- do.call(cbind, ret)
    return(ret)
    
}

### Working code
#tab <- read.table("Patient20_diseased_unstim.fcs.clustered.txt", header = T, sep = "\t", check.names = F,
#                  stringsAsFactors = F)
#metadata.tab <- read.table("test_metadata.txt", header = T, sep = "\t", check.names = F,
#                           stringsAsFactors = F)

#m <- reshape_cluster_features(tab, c("FunctionalMarker1", "FunctionalMarker2", "popsize"))


#df <- melt(m, varnames = c("file", "variable"))

#df <- merge(df, metadata.tab, by = "file")

#feature_tab <- cast(df, variable + condition + day ~ sample)

calculate_cluster_features <- function(tab, metadata.tab, features.names, predictors, endpoint.grouping) {
    m <- reshape_cluster_features(tab, features.names)
    
    df <- reshape::melt(m, varnames = c("file", "variable"))
    
    df <- merge(df, metadata.tab, by = "file")
    
    formula.exp <- as.formula(sprintf("%s ~ %s", paste(c("variable", predictors), collapse = "+"),
                           paste(endpoint.grouping, collapse = "+")))
    
    ret <- reshape::cast(df, formula.exp, 
            fun.aggregate = function(x) {stop("The combination of response grouping and predictors does not uniquely identify each file")})
    print(ret[1:5,])
    return(ret)
}

merge_metadata_information <- function(input.tab, metadata.tab, features) {
    col.names <- sapply(features, paste, "@", sep = "")
    col.names <- paste(col.names, collapse = "|")
    col.names <- grep(col.names, names(input.tab), value = T)
    
    m <- as.matrix(t(input.tab[, col.names]))
    
    col.names <- strsplit(col.names, "@")
    var.name <- sapply(col.names, "[", 1)
    file.name <- sapply(col.names, "[", 2)
    
    
    df <- data.frame(var.name, file.name, m, check.names = F, stringsAsFactors = F)
    
    ret <- merge(metadata.tab, df, by.x = "file", by.y = "file.name")
    
    
    
}


calculate_cluster_features_old <- function(input.tab, metadata.tab, features, baseline.condition = NULL) {
    tab <- merge_metadata_information(input.tab, metadata.tab, features)
    
    
    
    ret <- lapply(features, function(s) {
        temp <- m[, grep(s, colnames(m))]
        temp[is.na(temp)] <- 0
        
        if(s == "popsize") {
            temp <- t(temp)
            temp <- temp / rowSums(temp)
            temp <- t(temp)
        }
        
        if(!is.null(baseline)) {
            bl <- paste(s, baseline, sep = "@")
            temp <- temp / temp[, bl]
        }
        
        temp <- transpose_feature_matrix(temp)
        temp[!is.finite(temp)] <- 0
        
        return(temp)
        
    })
    ret <- do.call(cbind, ret)
    return(ret)
    
}


run_citrus <- function(working.directory, input.tab, features, endpoint, model.type, baseline = NULL) {
    citrus.features <- calculate_cluster_features(input.tab, features, baseline)
    run_citrus_analysis(citrus.features, endpoint, working.directory, model.type)
}