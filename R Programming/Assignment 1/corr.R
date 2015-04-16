corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## Loop through all files and use complete.R to build a vector of which ones meet the threshold
  source("complete.R")
  nitrate <- vector()
  sulfate <- vector()
  corrs <- as.numeric(vector())
  j <- 1
  
  for (i in 1:332) {
    compl <- complete("specdata", i)
    if (compl$nobs > threshold) {
      ## read the file into an object
      filename <- paste(formatC(i, width=3, flag=0), ".csv", sep="")
      filepath <- file.path(directory, filename, fsep = .Platform$file.sep)
      data <- read.csv(filepath)
      
      nitrate <- data[complete.cases(data),]$nitrate
      sulfate <- data[complete.cases(data),]$sulfate
      
      corrs[j] <- cor(nitrate, sulfate)
      j <- j + 1
    }
  }

  return(corrs)

}