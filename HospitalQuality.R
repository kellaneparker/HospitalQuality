###### Hospital Quality R Programming Assignment #####

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)  # Number of columns
nrow(outcome)  # Number of rows
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])




### 'best'

best <- function(state, outcome) {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_states <- unique(outcome$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  ## Extract relevant data and find the hospital with the lowest 30-day death rate
  outcome[, 11] <- as.numeric(outcome[, 11])
  valid_data <- !is.na(outcome[, 11])
  filtered_data <- outcome[valid_data, ]
  best_hospital <- filtered_data[which.min(filtered_data[, 11]), "Hospital.Name"]
  
  return(best_hospital)
}
### 'rankhospital'


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_states <- unique(outcome$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  ## Extract relevant data and rank hospitals
  outcome[, 11] <- as.numeric(outcome[, 11])
  valid_data <- !is.na(outcome[, 11])
  filtered_data <- outcome[valid_data, ]
  
  if (num == "best") {
    rank_order <- order(filtered_data[, 11], filtered_data[, "Hospital.Name"])
  } else if (num == "worst") {
    rank_order <- order(-filtered_data[, 11], filtered_data[, "Hospital.Name"])
  } else {
    rank_order <- order(filtered_data[, 11], filtered_data[, "Hospital.Name"])
  }
  
  ranked_hospital <- filtered_data[rank_order[num], "Hospital.Name"]
  
  return(ranked_hospital)
}

  ### 'rankall'
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  ## Extract relevant data and rank hospitals for each state
  outcome[, 11] <- as.numeric(outcome[, 11])
  valid_data <- !is.na(outcome[, 11])
  filtered_data <- outcome[valid_data, ]
  
  rank_result <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  
  for (state in unique(filtered_data$State)) {
    state_data <- filtered_data[filtered_data$State == state, ]
    
    if (num == "best") {
      rank_order <- order(state_data[, 11], state_data[, "Hospital.Name"])
    } else if (num == "worst") {
      rank_order <- order(-state_data[, 11], state_data[, "Hospital.Name"])
    } else {
      rank_order <- order(state_data[, 11], state_data[, "Hospital.Name"])
    }
    
    ranked_hospital <- state_data[rank_order[num], "Hospital.Name"]
    
    rank_result <- rbind(rank_result, data.frame(hospital = ranked_hospital, state = state))
  }
  
  return(rank_result)
}

  
  
  
  
  

  
  
  
