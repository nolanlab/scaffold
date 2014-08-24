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