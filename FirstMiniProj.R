#1 pollutantmean() function
pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Create a list of files (directory is prepended to the returned names)
  files_list <- list.files(directory, full.names = TRUE)
  # Create an empty data frame
  data <- data.frame()
  
  # For each id passed as a parameter
  for (i in id) {
    # Read the file and accumulate the data to the main data
    data <- rbind(data, read.csv(files_list[i]))
  }
  # Calculate the mean (NA values are stripped)
  mean(data[,pollutant], na.rm=TRUE)
}

# Sammple inputs
pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")

# ------------------------------------------------------------------

#2 complete() function
complete <- function(directory, id = 1:332) {
  # Create a list of files
  files_list <- list.files(directory, full.names= TRUE)
  # Create an empty data frame 
  data <- data.frame()
  
  # For each id passed as a parameter
  for (i in id) {
    # Read the file
    # file_data <- rbind(data, read.csv(files_list[i]))
    file_data <- read.csv(files_list[i])
    # Total number of all the cases
    nobs <- sum(complete.cases(file_data))
    # Bind the results by row
    temp <- data.frame(i, nobs)
    # Accumulate complete cases and enumerates by index
    data <- rbind(data, temp)
  }
  # Set column names
  colnames(data) <- c("id", "nobs")
  # Return the result
  return(data)
}

# Sample inputs
complete ("specdata", 1)

complete ("specdata", c(2, 4, 8, 10, 12))

complete ("specdata", 30:25)

complete ("specdata", 3)

# ------------------------------------------------------------------

#3 corr() function
corr <- function(directory, threshold = 0)
{
  # Create a list of all files
  files_full <- list.files(directory, full.names= TRUE)
  # Create an empty data set
  data <- vector(mode = "numeric", length = 0)
  for(i in 1:length(files_full))
  {
    # Read the file
    file_data <- read.csv(files_full[i])
    
    #Calculate the csum    
    csum <- sum((!is.na(file_data$sulfate)) & (!is.na(file_data$nitrate)))
    if (csum > threshold)
    {
      #Extract data of nitrate and sulfate and calculate correlation between them
      so <- file_data[which(!is.na(file_data$sulfate)), ]
      no <- so[which(!is.na(so$nitrate)), ]
      data <- c(data, cor(no$sulfate, no$nitrate))
    }
  }
  data
}

# Sample inputs
cr <- corr("specdata", 150)
head(cr); summary(cr)

cr <- corr("specdata", 400)
head(cr); summary(cr)

cr <- corr("specdata", 5000)
head(cr); summary(cr); length(cr)

cr <- corr("specdata")
head(cr); summary(cr); length(cr)

# ------------------------------------------------------------------

#4 Histogram
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

# Creates a histogram
hist(outcome[, 11],
     xlab="Deaths",
     main ="Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     col='skyblue',border='black')
