pollutantmean <- function(directory, pollutant, id = 1:332) {
    names <- list.files(directory)[id]
    read <- lapply(paste(directory,"/",names, sep = ""),read.csv)
    return(mean(unlist(lapply(read, function(x){x[,pollutant]})), na.rm=T))
}