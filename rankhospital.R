rankhospital <- function(state, outcome, num = "best") {
  measures <- c("heart attack", "heart failure", "pneumonia")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states = data[[7]]
  ## Check that state and outcome are valid
  if (num == "best") {
    return(best(state, outcome))
  }
  if (!(state %in% states)) {
    stop("invalid state")
  }
  if(!(outcome %in% measures)) {
    stop("invalid outcome")
  }
  ## Take mortality rates given the outcome of given state
  givenstate <- states == state
  if (outcome == "heart attack") {
    rates <- data[[11]][givenstate]
  } else if (outcome == "heart failure") {
    rates <- data[[17]][givenstate]
  } else {
    rates <- data[[23]][givenstate]
  }
  na <- rates == "Not Available"
  numbers <- rates[!na]
  numrate <- as.numeric(numbers)
  if (num == "worst") {
    max <- max(numbers)
    hospitalofstate <- data[[2]][givenstate][!na]
    hosp <- hospitalofstate[numbers == max]
    sortlist <- sort(hosp)
    return(sortlist[1])
  }
  if (outcome == "heart attack") {
    dataless <- data[,c(2, 7, 11)]
  } else if (outcome == "heart failure") {
    dataless <- data[,c(2, 7, 17)]
  } else {
    dataless <- data[,c(2, 7, 23)]
  }
  datastate <- dataless[givenstate,]
  na <- datastate[, 3] == "Not Available"
  datastatena <- datastate[!na,]
  sortrates <- order(as.numeric(datastatena[[3]]), datastatena[[1]])
  result <- datastatena[sortrates,]
  result[[1]][num]
}