# 1. Write function 'pollutantmean' across list of monitors

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
  
  spec.files <- list.files(directory, full.names = TRUE)
  spec.data <- data.frame()
  for (i in id) {
    spec.data <- rbind(spec.data, read.csv(spec.files[i]))
  }
  mean(spec.data[, pollutant], na.rm = TRUE)
}

# 2. Write a function that reads a directory full of files and reports the number of completely observed cases in each data file

spec.complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return a data frame of the form: id nobs 1 117 2 1041,  where 'id' is
  ## the monitor ID number and 'nobs' is the number of complete cases
  
  nobs <- numeric()
  for (i in id) {
    
    newRead <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    nobs <- c(nobs, sum(complete.cases(newRead)))
  }
  return(data.frame(id, nobs))
}
spec.complete("specdata", c(2, 4, 8, 10, 12))

# 3. Write a function that takes a directory of data files and a threshold for complete cases 
 # and calculates the correlation between sulfate and nitrate for monitor locations 
 # where the number of completely observed cases (on all variables) is greater than the threshold.
 
spec.cor <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the number of
  ## completely observed observations (on all variables) required to compute
  ## the correlation between nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  df <- spec.complete(directory)
  ids <- df[df["nobs"] > threshold, ]$id
  spec.cor <- numeric()
  for (i in ids) {
    
    newRead <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff <- newRead[complete.cases(newRead), ]
    spec.cor <- c(spec.cor, cor(dff$sulfate, dff$nitrate))
  }
  return(spec.cor)
}
spec.cor150 <- spec.cor("specdata", 150)
head(spec.cor150)
