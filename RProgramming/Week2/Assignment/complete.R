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
  
  if(substr(directory,nchar(directory), nchar(directory)) != "/" ){
    directory = paste(directory, "/", sep="")
  }
  
  ## Step 2: Initialise record count dataframe
  recordCount <- data.frame("id" = integer(length(id)), "nobs"= integer(length(id)))
    
  
  ## Step 3 Now loop through the files
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
    completed <- readData[complete.cases(readData),]
    #print(length(completed[,1]))
    
    recordCount[i,1] <- monitor
    recordCount[i,2] <- nrow(completed)
    #recordCount <- recordCount + length(completed)
  }
  
  recordCount
}