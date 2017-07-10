require('XML')
library("neuralnet")
library("animation")
require("ggplot2")

## Installation from github
## 

library("devtools")

require(RMOA)
## 
library("arules")

setwd("/home/loedded/Escritorio/XML")

f <- as.formula("Class ~ c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10 + c11 + c12 + c13 + c14 + c15 + c16 + c17 + c18 + c19 + c20 + c21 + c22 + c23 + c24 + c25 + c26 + c27 + c28 + c29 + c30 + c31 + c32 + c33 + c34 + c35 + c36 + c37 + c38 + c39 + c40 + c41 + c42 + c43 + c44 + c45 + c46 + c47 + c48 + c49 + c50")

errorInit <- function(names){
	err <- list()
	for(name in names){
		err[[name]] = rep(0,50)
	}
	return (err)
}

addElement <- function(err, error){
	namesE <- names(error)
	for (name in namesE) {
		err [[name]] <- c(err[[name]], error[[name]])
		au <- err[[name]]
	}
	return (err)
}

main <- function(){
	sizeRl <- list()
	err <- list()
	first = 0
	last = 0 + 99
	dataFrame <- chargeData(first, last)
	dataFrameAux <- prepareData(dataFrame)
	rm(dataFrame)
	err <-  errorInit(names(dataFrameAux))
	hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
	
	linear <- linearRegresion(dataFrameAux)
	
	NN <- learning(dataFrameAux)
	NW <- onlyWeights(NN)
	
	#hdt <- learningHoefftingFirst(dataFrameAux, hdt)
  rm(dataFrameAux)
	
	test <- chargeData(first + 50,  last + 1)
	test <- prepareDataTest(test)
	errorRLList <- calculateErrorRL(linear, test)
	cP<-errorRLList
	err <- errorRLList
	errlist <- addElement(err, errorRLList)
	errorRLList <- as.data.frame(t(errorRLList))

	errorNNRList <- as.data.frame(t(calculateErrorNNR(NN, test)))
	
	#errorHdtList <- as.data.frame(t(calculateErrorHDT(hdt, test)))
	rm(test)
	sizeRL <- sizeRLInit(names(errorRLList))
	returns <- proportionalAdwin(err, errlist, sizeRL, cP)
	cP <- returns$eControlPoint
	sizeRl <- returns$sizeRL

	siezeHistory <- as.data.frame(t(sizeRL))
	
	rm(linear)
	#res <- manualRegresion(as.vector(as.matrix(result$net.result)) * maxF,(firstElement[-1]*maxF)-lastmaxF)
	
	for(i in c(1:400)){
		
		first = first + 1
		last = last + 1
		
		dataFrame<-chargeData(first, last)
		dataFrameAux <- prepareData(dataFrame)
		rm(dataFrame)
		linear <- linearRegresionADWIN(dataFrameAux, sizeRL)
		NN <- learningWithWeights(dataFrameAux, NW)
		NW <- onlyWeights(NN)
		
		#hdt <- learningHoeffting(dataFrameAux, hdt)
		rm(dataFrameAux)
		
		test <- chargeData(first + 50,  last + 1)
		test <- prepareDataTest(test)
		newErrorLR <- calculateErrorRL(linear, test)
		cP<-newErrorLR
		err <- newErrorLR
		errlist <- addElement(err, newErrorLR)
		newErrorLR <- as.data.frame(t(newErrorLR))
		
		returns <- proportionalAdwin(err, errlist, sizeRL, cP)
		
		cP <- returns$eControlPoint
	  sizeRl <- returns$sizeRL
		newErrorNNR <- calculateErrorNNR(NN, test)
		rm(test)
		rm(linear)
		print(last)
		print("ULTIMO asifja ipjai")
		
		siezeHistory <- rbind(siezeHistory, as.data.frame(t(sizeRL)))
		errorRLList <- rbind(errorRLList, newErrorLR)
		errorNNRList <- rbind(errorNNRList, as.matrix(t(newErrorNNR)))
		#errorhdtC <- calculateErrorHDT(hdt, test)
		#errorHdtList <- rbind(errorHdtList, as.matrix(t(errorhdtC)))
	  rm(newErrorLR)
		rm(newErrorNNR)
	}
	animate_plot(errorRLList, errorNNRList, siezeHistory)
	
	#plotter(errorRLList, errorNNRList)
}  	


#Añadir graficas de media varianza, y el error resta del error de la rg y de la nnr, y añadir tamaño de la ventana 

