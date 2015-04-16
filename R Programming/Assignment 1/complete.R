complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## create a vector to hold the results
  nobs <- vector()
  
  for (i in 1:length(id)) {
    ## read the file into an object
    filename <- paste(formatC(id[i], width=3, flag=0), ".csv", sep="")
    filepath <- file.path(directory, filename, fsep = .Platform$file.sep)
    data <- read.csv(filepath)
    
    nobs[i] <- sum(complete.cases(data))
  }
  
  complete.data <- data.frame(cbind(id, nobs))
  return(complete.data)
    
}