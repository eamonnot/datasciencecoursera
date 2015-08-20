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
  ## NOTE: Do not round the result!
  
  ## Step 1: Check to see if there is a / at the end of directory
  ## If not, add one
  if(substr(directory,nchar(directory), nchar(directory)) != "/" ){
    directory = paste(directory, "/", sep="")
  }
  
  ## Step 2: Initialise running mean and record count
  mean <- 0
  recordCount <- 0
  
  ## Step 3: Loop through the files and calculate the mean for
  ## the variable pollutant
  for(i in 1:length(id)){
    ## Firstly build the filename and file path
    monitor = id[i];
    fileName = ""
    if(monitor < 10){
        fileName <- paste("00",monitor,".csv",sep="")
    }else if (monitor >= 10 && monitor < 100) {
        fileName <- paste("0",monitor,".csv", sep="")  
    }else{
        fileName <- paste(monitor,".csv", sep="")
    }
    fullName <- paste(directory, fileName, sep="")
    
    ## Now retrieve the file into memory 
    readData <- read.csv(file=fullName, sep="," , header=TRUE)
    colValues <- readData[,colnames(readData) == pollutant]
    oldTotal = mean * recordCount
    newTotal = oldTotal + sum(colValues[is.na(colValues) == FALSE])
    recordCount = recordCount + length(colValues[is.na(colValues) == FALSE])
    mean = newTotal / recordCount
  }
  
  # Step 4: Return the computed mean
  mean
}