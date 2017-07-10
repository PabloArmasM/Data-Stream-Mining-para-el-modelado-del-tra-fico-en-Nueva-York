

## HoeffdingTree example
## 

## Predict using the HoeffdingTree on the iris dataset

learningHoefftingFirst <- function(prepareData, model){
  hdt <- list()
  dataName <- names(prepareData)
  for(name in dataName){
    hdt[[name]] <- hdtExecution(prepareData[[name]], model)
  }
  rm(dataName)
  return (hdt)
}


learningHoeffting <- function(prepareData, model){
	hdt <- list()
	dataName <- names(prepareData)
	for(name in dataName){
		hdt[[name]] <- hdtExecution(prepareData[[name]], model[[name]]$model)
	}
	rm(dataName)
	return (hdt)
}

hdtExecution <- function(prepareData, hdt){
	#means <- rowMeans(prepareData)
	#variance <- lapply(c(1:nrow(prepareData)), function(x) var(prepareData[,x]))
	#variance <- unlist(variance)
	
	#dataScale <- (prepareData-means)/(variance + 1)

  unq <- length(unique(prepareData$Class))
  if(unq > 4){
    dis <- discretize(prepareData$Class, categories = 5, method = "frequency", onlycuts=TRUE)
    prepareData$Class <-factor(discretize(prepareData$Class, method="fixed", categories = c(dis), labels = c(0:5)))
  }else if(unq <= 1){
    prepareData$Class <-factor(prepareData$Class)
  }else{
    dis <- discretize(prepareData$Class, categories = unq, method = "frequency", onlycuts=TRUE)
    prepareData$Class <-factor(discretize(prepareData$Class, method="fixed", categories = c(dis), labels = c(0:unq)))
  }
  
  strD <- datastream_dataframe(data = prepareData)
	n <- colnames(strD$data)
	ss <- paste(n[!n %in% "Class"], collapse = " + ")
  f <- as.formula(paste("Class~",ss))
  mymodel <- trainMOA(model = hdt, formula = f, data = strD)
		
  rm(n)
  rm(ss)
  rm(f)
	return(mymodel)
}






calculateErrorHDT <- function(hdt, testData){
  errorList <- list()
  nameData <- names(testData)
  i = 1
  for(name in nameData){
    test <- as.data.frame(t(testData[[name]]))
    errorList[[name]] <- errorHDT(hdt[[name]], test)
    i = i + 1
  }
  rm(nameData)
  rm(test)
  print(errorList)
  return(errorList)
}


errorHDT <- function(hdt, testData){
  rscores <- predict(hdt, newdata=testData, type="response")

  
  return (rscores)
}

