corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  if(substr(directory,nchar(directory), nchar(directory)) != "/" ){
    directory = paste(directory, "/", sep="")
  }
  
  completedf <- complete(directory)
  filesToSearch <- completedf[completedf$nobs > threshold,]$id
  
  returnVector <- numeric(length=length(filesToSearch))
  
  if(length(filesToSearch) > 0){
    for(i in 1:length(filesToSearch)){
      monitor = filesToSearch[i];
      fileName = ""
      if(monitor < 10){
        fileName <- paste("00",monitor,".csv",sep="")
      }
      else if (monitor >= 10 && monitor < 100) {
        fileName <- paste("0",monitor,".csv", sep="")  
      }
      else{
        fileName <- paste(monitor,".csv", sep="")
      }
      fullName <- paste(directory, fileName, sep="")
      
      readData <- read.csv(file=fullName, sep="," , header=TRUE)
      readData <- readData[complete.cases(readData),]
      if(nrow(readData) > threshold){
        x <- readData[,colnames(readData) == "sulfate"]
        y <- readData[,colnames(readData) == "nitrate"]
        returnVector[i] <- cor(x,y)
      }
    }
  }
  returnVector
}