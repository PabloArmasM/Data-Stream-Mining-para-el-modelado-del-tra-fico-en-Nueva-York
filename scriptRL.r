sizeRl <- list()
err <- list()
first = 0
last = 0 + 99
dataFrame <- chargeData(first, last)
dataFrameAux <- prepareData(dataFrame)

linear <- linearRegresion(dataFrameAux)


test <- chargeData(first + 50,  last + 1)
test <- prepareDataTest(test)
errorRLList <- calculateErrorRL(linear, test)
cP<-errorRLList
err <- errorRLList
errlist <- addElement(err, errorRLList)
errorRLList <- as.data.frame(t(errorRLList))
sizeRL <- sizeRLInit(names(errorRLList))
returns <- proportionalAdwin(err, errlist, sizeRL, cP)
cP <- returns$eControlPoint
sizeRL <- returns$sizeRL

siezeHistory <- as.data.frame(t(sizeRL))



for(i in c(1:300)){

	first = first + 1
	last = last + 1

	dataFrame <- chargeData(first, last)
	dataFrameAux <- prepareData(dataFrame)
	print("OTRA ITERACION")	
	print("OTRA ITERACION")	
	print("OTRA ITERACION")	
	print("OTRA ITERACION")	
	print("OTRA ITERACION")	
	print("OTRA ITERACION")	

	linear <- linearRegresionADWIN(dataFrameAux, sizeRL)


	test <- chargeData(first + 50,  last + 1)
	test <- prepareDataTest(test)

	newErrorLR <- calculateErrorRL(linear, test)
	err <- newErrorLR
	errlist <- addElement(err, newErrorLR)
	newErrorLR <- as.data.frame(t(newErrorLR))
	
	returns <- proportionalAdwin(err, errlist, sizeRL, cP)
	
	cP <- returns$eControlPoint
  	sizeRL <- returns$sizeRL
  	siezeHistory <- rbind(siezeHistory, as.data.frame(t(sizeRL)))
  	errorRLList <- rbind(errorRLList, newErrorLR)


}