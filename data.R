chargeData <- function(firstElement, lastElement){ #0 - 100 inicial
	First = TRUE
	for(i in firstElement:lastElement){
		name = paste('xml', i, '.xml', sep="")
		xml <- xmlToList(name)
		dataFrame <- data.frame(xml)
		rm(xml)
		linkId <- dataFrame['linkId', ]
		vec <- as.vector(as.matrix(linkId))
		colnames(dataFrame) <- vec
		rm(vec)
		rm(linkId)
		linkSpeed <- dataFrame['linkSpeed', ]
    rm(dataFrame)
		if(length(linkSpeed) > 152){
			linkSpeed$'4620315' <- NULL
		}

		if(First){
			First = FALSE
			finalDataFrame <- linkSpeed
			rm(linkSpeed)
		}else{
			finalDataFrame <- rbind(finalDataFrame, linkSpeed)
			rm(linkSpeed)
		}
	}
	return(finalDataFrame <- lapply(finalDataFrame, function(X) as.double(as.character(X[1 : length(X)]))))
}



createVector <- function(vec){
	first = TRUE
	while(length(vec) > 50){
		oldSpeed <- vec[1:50]
		newSpeed <- vec[51]
		vec <- vec[2:length(vec)]
		if(first){
			vecAux <- append(as.vector(newSpeed), as.vector(oldSpeed))
			matrixDataFrame <- c(vecAux)
			first = FALSE
		}else{
			vecAux <- append(as.vector(newSpeed), as.vector(oldSpeed))
			matrixDataFrame <- rbind(matrixDataFrame, vecAux)
		}
		rm(vecAux)
		rm(oldSpeed)
		rm(newSpeed)
	}
	rm(vec)
	return (matrixDataFrame)
}



createVectorTest <- function(vec){
	oldSpeed <- vec[1:50]
	newSpeed <- vec[51]
	vec <- vec[2:length(vec)]
	vecAux <- append(as.vector(newSpeed), as.vector(oldSpeed))
	matrixDataFrame <- c(vecAux)
	rm(oldSpeed)
	rm(newSpeed)
	rm(vec)
	rm(vecAux)
	return (matrixDataFrame)
}


prepareData <- function(dataFrame){
	
	First = TRUE
	i = 1
	listDataFrame <- list()
	
	for(vec in dataFrame){
		name <- names(dataFrame[i])
		matrixDataFrame <- createVector(vec)
		numRow = nrow(matrixDataFrame)
		rownames(matrixDataFrame) <- (1:numRow)
		if(First){
			First = FALSE
			globalDataFrame <- as.data.frame(matrixDataFrame)
			xx <- c(1:50)
			xx <- lapply(xx, function(X) paste("c", X, sep=""))
			colnames(globalDataFrame) <- c("Class", unlist(xx))
			#print(globalDataFrame)
			listDataFrame[[name]] <- globalDataFrame 
		}else{
			globalDataFrame <- as.data.frame(matrixDataFrame)
			xx <- c(1:50)
			xx <- lapply(xx, function(X) paste("c", X, sep=""))
			colnames(globalDataFrame) <- c("Class", unlist(xx))
			listDataFrame[[name]] <- globalDataFrame
		}
		rm(matrixDataFrame)
		rm(xx)
		rm(globalDataFrame)
		rm(numRow)
		i = i + 1
	}
	return(listDataFrame)
}



prepareDataTest <- function(dataFrame){
	First = TRUE
	i = 1
	listDataFrame <- list()
	
	for(vec in dataFrame){
		name <- names(dataFrame[i])
		#print(name)
		matrixDataFrame <- createVectorTest(vec)
		#numRow = nrow(matrixDataFrame)
		#rownames(matrixDataFrame) <- (1:numRow)
		xx <- c(1:50)
		xx <- lapply(xx, function(X) paste("c", X, sep=""))
		names(matrixDataFrame) <- c("Class", unlist(xx))
		listDataFrame[[name]] <- matrixDataFrame
		i = i + 1
		rm(name)
		rm(matrixDataFrame)
		rm(xx)
	}
	return(listDataFrame)	
}

