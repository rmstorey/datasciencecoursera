rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")[,c(2, 7, 11, 17, 23)]

    ## Check that outcome is valid
    if (outcome == "heart attack") columns <- c(1, 2, 3)
    else if (outcome == "heart failure") columns <- c(1, 2, 4)
    else if (outcome == "pneumonia") columns <- c(1, 2, 5)
    else stop("invalid outcome")
    
    ## Check that num is valid
    if(class(num) == "character"){
        if (! (num == "best" || num == "worst")){
            stop("invalid number")
        }
    }
    
    ## Subset the data
    outcome_data <- data[, columns]
    names(outcome_data)[3] <- "Deaths"
    
    ## Convert to numeric and eliminate NAs
    outcome_data$Deaths <- suppressWarnings(as.numeric(outcome_data$Deaths))
    outcome_data <- outcome_data[complete.cases(outcome_data),]

    ## For each state, find the hospital of the given rank
    data_by_state <- split(outcome_data, outcome_data$State)

    rank_hospital <- function(state_data, num) {
        ordered <- order(state_data$Deaths, state_data$Hospital.Name)
        
        if (num == "best") state_data$Hospital.Name[ordered[1]]
        else if (num == "worst") state_data$Hospital.Name[ordered[length(ordered)]]
        else state_data$Hospital.Name[ordered[num]]
    }

    ranking <- lapply(data_by_state, rank_hospital, num)

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data.frame(hospital = unlist(ranking), state = names(ranking), row.names = names(ranking))
}