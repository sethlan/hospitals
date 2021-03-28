best <- function(state, outcome) {
  measures <- c("heart attack", "heart failure", "pneumonia")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states = data[[7]]
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
  # na <- is.na(rates)
  # print(rates)
  na <- rates == "Not Available"
  num <- rates[!na]
  # print(num)
  numrate <- as.numeric(num)
  min <- min(numrate)
  # print(min)
  hospitalofstate <- data[[2]][givenstate][!na]
  # print(hospitalofstate)
  hospbest <- hospitalofstate[numrate == min]
  sortlist <- sort(hospbest)
  ## Return hospital name in that state with lowest 30-day death
  ## rate2
  sortlist[1]
}
