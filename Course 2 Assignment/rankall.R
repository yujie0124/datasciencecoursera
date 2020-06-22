rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
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
  output <- vector()
  
  states <- levels(data[, 7])
  
  for(i in 1:length(states)) {
    statedata <- data[grep(states[i], data$State), ]
    orderdata <- statedata[order(statedata[, col], statedata[, 2], 
                                 na.last = NA), ]
    hospital <- if(num == "best") {
      orderdata[1, 2]
    } else if(num == "worst") {
      orderdata[nrow(orderdata), 2]
    } else{
      orderdata[num, 2]
    }
    output <- append(output, c(hospital, states[i]))
  }
  
  output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
  colnames(output) <- c("hospital", "state")
  rownames(output) <- states
  output
  
}