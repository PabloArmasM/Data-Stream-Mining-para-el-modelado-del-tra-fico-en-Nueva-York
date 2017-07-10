sizeRLInit <- function(namesRL){
	sizeRL <- list()
	for(name in namesRL){
		sizeRL[[name]] = 0
	}
	return (sizeRL)
}

calculeADWIN <- function(error, sizeRL){
	nameError <- names(sizeRL)
	for(name in  nameError){
	  print(name)
		if(error[[name]] > 1 && sizeRL[[name]] < 40){
			sizeRL[[name]] = sizeRL[[name]] + 1
		}else if(error[[name]] < 1 && sizeRL[[name]] > 0){
			sizeRL[[name]] = sizeRL[[name]] - 1
		}
	}
	
	rm(nameError)
	
	return(sizeRL)
}


proportionalAdwin <- function(error, errorList, sizeRL, eControlPoint){
  nameError <- names(sizeRL)
  returnValues <- 1
  for(name in nameError){
  	w <- 0
    errL <- errorList[[name]]
    if(length(errL) < 51){
      errL <- errL[1:length(errL)]
    }else{
      errL <- errL[(length(errL) - (50+sizeRL[[name]])):length(errL)] 
    }
    #errL <- errL[1:length(errL)]
    err <- error[[name]]
    cP <- eControlPoint[[name]]
    eM <- max(unlist(errL))
    em <- min(unlist(errL))
    
    R <- (eM - em)
    E <- sqrt(((R^2)*log(1/0.05))/2*length(errL))
    
    if(abs(err - cP) > E){
      
    	if(sizeRL[[name]] < 40){
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    	  print("CAMBIO DE CONCEPTO")
    		sizeRL[[name]] <- sizeRL[[name]] + 1
    		eControlPoint[[name]] <- err
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    		print(sizeRL[[name]])
    	}
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])
      print(sizeRL[[name]])

    }else{
    	if(sizeRL[[name]] > 0){
    		sizeRL[[name]] = sizeRL[[name]] - 1
    	}
    }
    
  }
  newList <- list("eControlPoint" = eControlPoint, "sizeRL" = sizeRL)
  return (newList)
}

