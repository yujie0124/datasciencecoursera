pollutantmean <- function(directory, pollutant, id = 1:332) {
    names <- list.files(directory)[id]
    print(names)
    read <- lapply(paste(directory,"/",names, sep = ""),read.csv)
    print(read)
    return(mean(unlist(lapply(read, function(x){x[,pollutant]})), na.rm=T))
}