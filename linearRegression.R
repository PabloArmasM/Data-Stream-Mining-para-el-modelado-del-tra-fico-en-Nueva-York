
linearRegresion <- function(listDataFrame){
	first = TRUE
	linearList = list()
	dataName <- names(listDataFrame)
	i = 1
	for (vec in listDataFrame){
		dataX <- data.frame(vec)
		linear <- lm(dataX$Class ~., data = dataX[-1])
		name <- dataName[i]
		linearList [[name]] <- linear
		i = i + 1
		#matrixAux <- data.frame(c(as.double(as.character(dataX$v1)), as.double(as.character(dataX$v2)), as.double(as.character(dataX$v3)), as.double(as.character(dataX$v4))))
	}
	rm(dataName)
	rm(dataX)
	rm(linear)
	rm(name)
	return (linearList)
}


linearRegresionADWIN <- function(listDataFrame, sizeRL){
	first = TRUE
	linearList = list()
	dataName <- names(listDataFrame)
	i = 1
	for (vec in listDataFrame){
		name <- dataName[i]
		dataX <- data.frame(vec[sizeRL[[name]]+1:length(vec),])
		linear <- lm(dataX$Class ~., data = dataX[-1])
		linearList [[name]] <- linear
		i = i + 1
	}
	rm(dataName)
	rm(dataX)
	rm(linear)
	rm(name)
	return (linearList)
}

