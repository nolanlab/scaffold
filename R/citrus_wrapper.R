run_citrus_analysis <- function(citrus.features, endpoint, working.directory, model.type) {
    family <- NULL
    
    if(is.character(endpoint) || is.factor(endpoint)) {
        family <- "classification"
        endpoint <- as.factor(endpoint)
        
    }
    else
        family <- "continuous"

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
#tab <- read.table("Patient20_diseased_unstim.fcs.clustered.txt", header = T, sep = "\t", check.names = F, stringsAsFactors = F)
#metadata.tab <- read.table("test_metadata.txt", header = T, sep = "\t", check.names = F, stringsAsFactors = F)

#m <- reshape_cluster_features(tab, c("FunctionalMarker1", "FunctionalMarker2", "popsize"))


#df <- melt(m, varnames = c("file", "variable"))

#df <- merge(df, metadata.tab, by = "file")

#feature_tab <- cast(df, variable + condition + day ~ sample)

convert_to_citrus_featureset <- function(tab, endpoint.grouping) {
    rnames <- do.call(paste, list(tab[, endpoint.grouping], sep = "_"))
    cnames <- setdiff(colnames(tab), endpoint.grouping)
    ret <- as.matrix(tab[, cnames])
    row.names(ret) <- rnames
    colnames(ret) <- cnames
    
    return(list(allFeatures = ret, nFolds = 1))
    
    return(ret)
}

calculate_cluster_features <- function(tab, metadata.tab, features.names, predictors, endpoint.grouping) {
    m <- reshape_cluster_features(tab, features.names)
    
    df <- reshape::melt(m, varnames = c("file", "variable"))
    
    df <- merge(df, metadata.tab, by = "file")
    
    formula.exp <- as.formula(sprintf("%s ~ %s", paste(endpoint.grouping, collapse = "+"), 
                                      paste(c("variable", predictors), collapse = "+")))
    
    #Stop if the combination of response grouping and predictors does not uniquely identify each file
    stopifnot(nrow(df) == nrow(unique(df[, c("variable", predictors, endpoint.grouping)])))
    ret <- reshape::cast(df, formula.exp)
    
    return(ret)
}

#' Normalize numeric data in multiple steps
#' 
#' This function normalizes numeric data in a table in multiple steps, based on groups defined by different combinations of categorical variables. The table
#' is assumed to be in molten format (e.g. see \code{reshape::melt}), with a single value column. This function will then proceed to normalize the data
#' based on the value identified by a categorical variable, then normalize the normalized data again by another value etc. (see below for Details)
#' 
#' For this function to work the inputs need to satisfy a number of conditions
#' 
#'   \itemize{
#'     \item{The input table needs to be in molten format (i.e. see \code{reshape::melt}). There need to be \code{variable} and
#'       \code{value} columns identifying the variables that will be normalized and their values}
#'     \item{The remaining columns of the input table should all be categorical variables (either strings or factors), identifying different
#'       subsets of rows}
#'     \item{At each step of the normalization the table is grouped using the \code{variable}, \code{subject.var} and all the columns
#'       in \code{names(norm.template)}. After this grouping, for every group, there can be only one row for the value of the current grouping
#'       variable that has been selected as a basis for normalization. In other words the function will not allow you to normalize a vector of values
#'       by another vector of values, it will only allow normalization of a vector by an individual number. This is done to prevent the result to depend
#'       on the ordering of the table.}
#'   }
#' An example should help clarify the working of this function. Assume you have a dataset where different variables have been measured for multiple subjects,
#' under different stimulation conditions, and at different timepoints. For each variable you want the data at each timepoint to be normalized by the value in 
#' the "unstim" condition. Then you want this data to be further normalized by the value at the "baseline" timepoint. Assume \code{tab} is in molten
#' format and has the following columns
#'   \itemize{
#'     \item{\code{variable}}: identifies the variable
#'     \item{\code{value}}: the corresponding value of the variable
#'     \item{\code{timepoint}}: categorical variable that identifies the timepoint
#'     \item{\code{condition}}: categorical variable that identified the condition
#'     \item{\code{subject}}: categorical variable that identifies data belonging to the same subject (all the normalization is done within subject)
#'   }
#' To achieve the result described above, you would invoke this function as \code{multistep_normalize(tab, list(condition = "unstim", timepoint = "baseline"), "subject")}.
#' Note that the function would fail if you only specify a single variable (either \code{condition} or \code{timepoint}), because a single variable is not enough
#' to identify a single value for normalization, since you have multiple conditions for each timepoint and viceversa. 
#' 
#' @param tab The input \code{data.frame} See the details for assumption about its structure
#' @param norm.template A named list idenfying which categorical variables should be used to group data for normalization. The values in the list
#'   represent the value of the corresponding variable that identify the rows that are used as reference for normalization at each step. The data will be normalized
#'   in the same order specified by this list (i.e. data will be normalized according to the first variable, then again according to the second etc.)
#' @param subject.var The name of the column that identifies different subjects in \code{tab}. All normalization operations are done within the subgroups
#'   identified by this variable (i.e. data will never be normalized across subsets idenfied by different values of subject.var)
#' 
#' 
#'
#' @export 
multistep_normalize <- function(tab, norm.template, subject.var) {
    var.names <- names(norm.template)
    var.values <- unlist(norm.template, use.names = F)
    
    ret <- tab
    
    for(i in 1:length(var.names)) {
        variable.names <- c("variable", subject.var, var.names[var.names != var.names[i]])
        mutate.s <- sprintf("value / value[%s == '%s']", var.names[i], var.values[i])
        filter.s <- sprintf("%s != '%s'", var.names[i], var.values[i])
        
        # Check for uniqueness of normalization values
        dplyr::group_by_(ret, .dots = variable.names) %>%
            dplyr::do({
                x <- .[[var.names[i]]]
                if(length(x[x == var.values[i]]) != 1)
                    stop("This combination of variables does not identify a single reference value for normalization")
                .
            })
        
 
        ret <- dplyr::group_by_(ret, .dots = variable.names) %>%
                dplyr::mutate_(.dots = setNames(mutate.s, "value")) %>%
                dplyr::filter_(.dots = filter.s)
        
    }
    
    return(ret)
}



run_citrus <- function(working.directory, input.tab, features, endpoint, model.type, baseline = NULL) {
    citrus.features <- calculate_cluster_features(input.tab, features, baseline)
    run_citrus_analysis(citrus.features, endpoint, working.directory, model.type)
}