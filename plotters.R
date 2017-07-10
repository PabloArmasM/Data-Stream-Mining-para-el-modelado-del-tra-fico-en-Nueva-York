plotter <- function(errorLR, errorNNR){
	errorname <- names(errorLR)
	err <- 0
	for(name in errorname){
		errLR <- unlist(errorLR[name])
		names(errLR) <- c(1:length(errLR))
		errNNR <- unlist(errorNNR[name])
		names(errNNR) <- c(1:length(errNNR))
		name = paste("/Users/Loedded/Desktop/DataPlot/graf", name, ".png", sep="")
		png(filename=name)
		
		err$size <- c(1:length(errLR))
		err$LR <- errLR
		err$NNR <- errNNR
		
		plot(range(err$size, na.rm=T), range(c(err$LR, err$NNR), na.rm=T), type='n')
	   lines(err$size, err$LR, col="red")
	   lines(err$size, err$NNR, col="green")

		dev.off()
	}
	gc()
}


animate_plot <- function(errorL, errorN, siezeHistory) {
	
	errorL[["4362244"]] <- NULL
	errorN[["4362244"]] <- NULL
	errorNam <- names(errorL)
	for(name in errorNam){
	  print(name)
	  
	  #if(name == "4616235"){
	  #}else{
	 if(name == "4763650"){
	  size <- as.vector(unlist(siezeHistory[[name]]))
		errorLA <- as.vector(unlist(errorL[name]))
		errorNA <- as.vector(unlist(errorN[name]))
		maxL = max(errorLA)
		maxN = max(errorNA)
		if(maxL > maxN){
		  maximo <- maxL
		}else{
		  maximo <- maxN
		}
		maximo <- maxN*3
		Error <- c(1:maximo)
		nm <- ""
		for(i in c(1:length(errorNA))){	
		  
			if (i<=20){
       			subL <- errorLA[1:i]
       			subN <- errorNA[1:i]
       		}else{
       			subL <- errorLA[1:i]
       			subN <- errorNA[1:i]
      }
			joint_aux <- list()
			joint_aux$LL <- subL
			joint_aux$NN <- subN
			
			joint_aux <- as.data.frame(joint_aux)
			#joint_aux <- cbind(subL, subN)
			
			if (i<=20){
			  minT = 1
			  maxT = 20
			  Time = c(1:20)
			}else{ 
			  minT = (i-20)
			  maxT = i
			  Time <- c((i-20) : i)
			}
			
			lle <- paste("Linear Regression Error\n Window size:", 50 - size[i])
			Time = c(1:length(subL))
			gr <- paste("gr", i, sep = "")
			gr <- paste(gr, ".png", sep = "")
			nm <- paste(nm, gr)
		  png(filename=gr)
		  if(i > 1 && (size[i-1] < size[i] ||  size[i] == 40)){
		    pl <- ggplot(joint_aux, aes(x=Time, y=Error))+
		      scale_y_continuous(limits = c(0,maximo/10))+
		      scale_x_continuous(limits = c(minT,maxT))+
		      geom_line(aes(y=NN,color="Neural Network Error"),size=2,alpha=0.5, data=joint_aux)+
		      geom_line(aes(y=LL,color=lle), size=2,alpha=0.5, data=joint_aux)+
		      scale_colour_manual(values = c("red", "blue"))
		    print(pl)
		    dev.off()
		  }else{
  			pl <- ggplot(joint_aux, aes(x=Time, y=Error))+
  			  scale_y_continuous(limits = c(0,maximo/10))+
  			  scale_x_continuous(limits = c(minT,maxT))+
  			  geom_line(aes(y=NN,color="Neural Network Error"),size=2,alpha=0.5, data=joint_aux)+
  			  geom_line(aes(y=LL,color=lle), size=2,alpha=0.5, data=joint_aux)+
  			  scale_colour_manual(values = c("green", "blue"))
  			print(pl)
  			dev.off()
		  }
		}
		gifn <- paste(name, "AV5.gif", sep="")
		namegif <- paste("convert -delay 20", nm)
		namegif <- paste(namegif, gifn)
		system(namegif)
	  }
	}	 
	
} 	

