helper <- function(data, col_num, state) {
     state_subset <- data[data[, 7]==state, ]
     outcome_arr <- state_subset[, col_num]
     min <- min(outcome_arr, na.rm=T)
     min_index <- which(outcome_arr == min)
     hosp_name <- state_subset[min_index, 2]
     return(hosp_name)
}

best <- function(state, outcome) {
     
     ## Read the outcome data
     dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     ## Check that state and outcome are valid
     if (!state %in% unique(dat[, 7])) {
          stop("invalid state")
     }
     switch(outcome, `heart attack` = {
          col = 11
     }, `heart failure` = {
          col = 17
     }, pneumonia = {
          col = 23
     }, stop("invalid outcome"))
     ## Return hospital name in that state with lowest 30-day death rate
     df = dat[dat$State == state, c(2, col)]
     df[which.min(df[, 2]), 1]
}

#Finding the best hospital in a state