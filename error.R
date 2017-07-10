calculateErrorRL <- function(RL, testData){
  errorList <- list()
  nameData <- names(testData)
  i = 1
  for(vec in c(1:length(testData))){
    iname <- nameData[i]
    test <- as.data.frame(t(testData[[iname]]))
    errorList[[iname]] <- errorRL(RL[[iname]], test)
    i = i + 1
  }
  rm(nameData)
  rm(iname)
  rm(test)
  return(errorList)
}


errorRL <-function(RL, testData){
  result <- predict(RL, testData[-1])
  rmse <- sqrt(mean(testData$Class - result)^2)
  rm(result)
  return (rmse)
}


calculateErrorNNR <- function(NN, testData){
  errorList <- list()
  nameData <- names(testData)
  i = 1
  for(vec in c(1:length(testData))){
    iname <- nameData[i]
    test <- as.data.frame(t(testData[[iname]]))
    errorList[[iname]] <- errorNNR(NN[[iname]], test)
    i = i + 1
  }
  rm(nameData)
  rm(iname)
  rm(test)
  return(errorList)
}


errorNNR <- function(NN, testData){
  testDataMean <- mean(unlist(testData))
  testDataVar <- var(unlist(testData))
  
  testDataScale <- (testData - testDataMean)/(testDataVar + 1)
  
  result <- compute(NN, testDataScale[-1])
  
  result <- result$net.result * (testDataVar + 1) + testDataMean
  rmse <- sqrt(mean(testData$Class - result)^2)
  
  rm(testDataMean)
  rm(testDataVar)
  rm(testDataScale)
  rm(result)
  
  return (rmse)
}

