rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")[,c(2, 7, 11, 17, 23)]
    
    ## Check that outcome is valid
    if (outcome == "heart attack") columns <- c(1, 3)
    else if (outcome == "heart failure") columnns <- c(1, 4)
    else if (outcome == "pneumonia") columns <- c(1, 5)
    else stop("invalid outcome")
    
    ## Check that num is valid
    if(class(num) == "character"){
        if (! (num == "best" || num == "worst")){
            stop("invalid number")
        }
    }
    
    ## Subset the state data and check state is valid
    state_data <- data[data$State == state, columns]
    if (nrow(state_data) == 0) stop("invalid state")
    names(state_data)[2] <- "Deaths"
    
    ## Convert to numerics and eliminate NAs
    state_data$Deaths <- suppressWarnings(as.numeric(state_data$Deaths))
    state_data <- state_data[complete.cases(state_data),]

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best") num <- 1
    else if (num == "worst") num <- nrow(state_data)
    
    ranking <- order(state_data$Deaths, state_data$Hospital.Name)
    hospital <- state_data$Hospital.Name[ranking[num]]
    
    return(hospital)
}