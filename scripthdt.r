sizeRl <- list()
err <- list()
first = 0
last = 0 + 99
dataFrame <- chargeData(first, last)
dataFrameAux <- prepareData(dataFrame)	

#hdt <- OzaBoost(baseLearner = "trees.HoeffdingTree", ensembleSize = 30

hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver", 'n')

#NN <- learning(dataFrameAux)
#NW <- onlyWeights(NN)

hdt <- learningHoefftingFirst(dataFrameAux, hdt)
test <- chargeData(first + 50,  last + 1)
test <- prepareDataTest(test)
#errorNNRList <- as.data.frame(t(calculateErrorNNR(NN, test)))
errorHdtList <- calculateErrorHDT(hdt, test)
for(i in c(1:334)){
	first = first + 1
	last = last + 1

	dataFrame <- chargeData(first, last)
	dataFrameAux <- prepareData(dataFrame)	
	hdt <- learningHoeffting(dataFrameAux, hdt)

	#NN <- learningWithWeights(dataFrameAux, NW)
	#NW <- onlyWeights(NN)

	test <- chargeData(first + 50,  last + 1)
	test <- prepareDataTest(test)

	#newErrorNNR <- calculateErrorNNR(NN, test)
	#errorNNRList <- rbind(errorNNRList, as.matrix(t(newErrorNNR)))

	errorhdtC <- calculateErrorHDT(hdt, test)
	errorHdtList <- rbind(errorHdtList, as.matrix(t(errorhdtC)))

	#hdt <- OzaBoost(baseLearner = "trees.HoeffdingTree", ensembleSize = 30)




}	
animate_plot(errorRLList, errorNNRList, siezeHistory, errorHdtList)