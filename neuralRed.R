NNExecution <- function(prepareData){
  
  means <- rowMeans(prepareData)
  variance <- lapply(c(1:nrow(prepareData)), function(x) var(prepareData[,x]))
  variance <- unlist(variance)
  
  dataScale <- (prepareData-means)/(variance + 1)
  
  n <- colnames(dataScale)
  ss <- paste(n[!n %in% "Class"], collapse = " + ")
  f <- as.formula(paste("Class~", ss))
  
  net.sqrt <- neuralnet(f, data=dataScale , threshold=0.01, act.fct="tanh")
  
  rm(means)
  rm(variance)
  rm(dataScale)
  rm(n)
  rm(ss)
  rm(f)
  
  return (net.sqrt)	
}


NNExecutionWithWeights <- function(prepareData, weights){
  means <- rowMeans(prepareData)
  variance <- lapply(c(1:nrow(prepareData)), function(x) var(prepareData[,x]))
  variance <- unlist(variance)
  
  dataScale <- (prepareData-means)/(variance + 1)
  
  n <- colnames(dataScale)
  ss <- paste(n[!n %in% "Class"], collapse = " + ")
  f <- as.formula(paste("Class~", ss))
  
  net.sqrt <- neuralnet(f, data=dataScale , hidden=c(49, 24, 4, 1), threshold=0.01, act.fct="tanh", startweights=weights)
  
  
  rm(means)
  rm(variance)
  rm(dataScale)
  rm(n)
  rm(ss)
  rm(f)
  return(net.sqrt)
  
}


learning <- function(prepareData){
  NNE <- list()
  dataName <- names(prepareData)
  for(name in dataName){
    print(name)
    NNE[[name]] <- NNExecution(prepareData[[name]])
  }
  
  rm(dataName)
  
  return (NNE)
}


learningWithWeights <- function(prepareData, weights){
  NNE <- list()
  dataName <- names(prepareData)
  for(name in dataName){
    NNE[[name]] <- NNExecutionWithWeights(prepareData[[name]], weights[[name]])
  }
  rm(dataName)
  return (NNE)
}

onlyWeights <- function(NN){
  nameN <- names(NN)
  weightsN <- list()
  for(name in nameN){
    weightsN[[name]] <- NN[[name]]$generalized.weights
  }
  return(weightsN)
}

