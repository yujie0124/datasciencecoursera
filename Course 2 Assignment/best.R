best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  states <- data[, 7]
  state_name <- FALSE
  for(i in 1:length(states)){
    if(state == states[i]){
      state_name <- TRUE
    }
  }
  if(state_name == FALSE){
    stop("invalid state")
  }
  
  if (!((outcome == "heart attack") | (outcome == "heart failure")
        | (outcome == "pneumonia"))) {
    stop ("invalid outcome")
  }
  
  if(outcome == "heart attack"){
    col <-  11
  }
  else if(outcome == "heart failure"){
    col <- 17
  }
  else if(outcome == "pneumonia"){
    col <- 23
  }
  
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[,2] <- as.character(data[,2])
  read_statedata <- data[grep(state, data$State), ]
  order_data <- read_statedata[order(read_statedata[,col], read_statedata[,2], na.last=NA),]
  order_data[1,2]
  
}