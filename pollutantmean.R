pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## create a vector to hold all the data points
  stat <- vector()
  
  for (i in 1:length(id)) {
    ## read the file into an object
    filename <- paste(formatC(id[i], width=3, flag=0), ".csv", sep="")
    filepath <- file.path(directory, filename, fsep = .Platform$file.sep)
    data <- read.csv(filepath)

    stat <- c(stat, data[[pollutant]][!is.na(data[[pollutant]])])
  }

  mean(stat)
}