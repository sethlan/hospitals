rankall <- function(outcome, num = "best") {
  measures <- c("heart attack", "heart failure", "pneumonia")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that outcome are valid
  if(!(outcome %in% measures)) {
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  uniqstate <- sort(unique(data[[7]]))
  # result <- 
  # for (state in uniqstate) {
  #   if (outcome == "heart attack") {
  #     dataless <- data[,c(2, 7, 11)]
  #   } else if (outcome == "heart failure") {
  #     dataless <- data[,c(2, 7, 17)]
  #   } else {
  #     dataless <- data[,c(2, 7, 23)]
  #   }
  #   givenstate <- data[[7]] == state
  #   datastate <- dataless[givenstate,]
  #   na <- datastate[, 3] == "Not Available"
  #   datastatena <- datastate[!na,]
  #   sortrates <- order(as.numeric(datastatena[[3]]), datastatena[[1]])
  #   sortdata <- datastatena[sortrates, c(1, 2)]
  #   cbind(result, c(sortdata[num, 1], sortdata[num, 2]))
  # }
    funct <- function(state) {
    # print(state)
    if (outcome == "heart attack") {
      dataless <- data[,c(2, 7, 11)]
    } else if (outcome == "heart failure") {
      dataless <- data[,c(2, 7, 17)]
    } else {
      dataless <- data[,c(2, 7, 23)]
    }
    givenstate <- data[[7]] == state
    datastate <- dataless[givenstate,]
    na <- datastate[, 3] == "Not Available"
    datastatena <- datastate[!na,]
    if (num == "worst") {
      sortrates <- order(as.numeric(datastatena[[3]]), decreasing = TRUE)
      return(datastatena[sortrates[1], c(1, 2)])
    }
    if (num == "best") {
      sortrates <- order(as.numeric(datastatena[[3]]))
      return(datastatena[sortrates[1], c(1, 2)])
    }
    sortrates <- order(as.numeric(datastatena[[3]]), datastatena[[1]])
    sortdata <- datastatena[sortrates, c(1, 2)]
    c(sortdata[num, 1], sortdata[num, 2])
  }
  lapply(uniqstate, funct)
  # orderres <- order(result[[2]])
  # print(orderres)
  # result[orderres,]
  # ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}