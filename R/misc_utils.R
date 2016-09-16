check_names <- function(dir.name, col.names)
{
  for(f in list.files(path = dir.name, pattern = "*.fcs", full.names = T))
  {
    fcs.file <- read.FCS(f)
    tab <- convert_fcs(fcs.file)
    colnames(tab) <- pData(parameters(fcs.file))$desc
    
    
    s <- paste("File", f, "is missing variables:")
    print(paste(s, col.names[!(col.names %in% names(tab))]))
  }
}

#Get the subset of columns that are common to multiple files
get_common_columns <- function(working.dir, files.list)
{
    l <- list()
    for(f in files.list)
    {
        temp <- read.table(file.path(working.dir, f), header = T, sep = "\t", check.names = F, quote = "")
        l <- c(l, list(names(temp)))
    }
    return(Reduce(intersect, l))
}