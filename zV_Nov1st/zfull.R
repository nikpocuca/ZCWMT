cat('loading zcwm function \n')
#Beginning of zwm function 
zcwm <- function(data, formulaZP, np, runC, normModels){
  
  
  #initializations of zero space.
  data.z <<- data
  data.z$NB <<- as.integer(data.z$NB <= 0) 
  
  
  cat('Beginning Partitioning using Cwm','\n')
  cat('--------------------------------','\n')
  cat(' \n')
  
  #First Partition of space. (Poisson) 
  
  dclareCWM <- function(){
    cat('Poisson Model', '\n')
    cwm_poisson <<- cwm(formulaY = formulaZP,
                        data= data,
                        familyY = poisson(link="log"),
                        Xnorm = Xnorms,	
                        Xpois = Xpoises,
                        k = np)
    
    
    
    #Declare global for second partition. 
    declare_g(data.z)
    #Second Partition of space. (Binomial)
    
    cat('Binomial Model', '\n')
    cwm_binomial <<- cwm(formulaY = formulaZP,
                         data = data.z,
                         Xnorm = Xnorms,
                         Xpois = Xpoises,
                         familyY = binomial(link = "logit"),
                         k = np)
    
  }
  
  
  if(runC == TRUE){ dclareCWM()}
  

  
  nPara <- length(getParGLM(cwm_binomial)[1])
  
  c_pois <<- getCluster(cwm_poisson)
  c_bin <<- getCluster(cwm_binomial)
  lex <<- paste(c_pois, c_bin, sep= "")
  test <<- cbind(c_pois,c_bin,lex)
  partitions <<- match(lex,unique(lex))

 
  
  # Prediction Values
  bestModel <<- getBestModel(cwm_poisson)

  predicts <<- data.frame(1:length(data.z[,1]))
 
  for( i in 1:np ){
	 holdGLM <- bestModel$models[[1]]$GLModel[[i]]$model
  	 holdY <- data$ClaimNb
	
	 holdGLM$y <- holdY #adjusting for ingrassias lack of consistency

	 holdPredictions <<- predprob(holdGLM)
	 

	 holdNames <- c()
	 for(j in sort(unique(holdY),decreasing=FALSE)){
		
		spaceTag <- paste("M",i,sep="")

		holdNames <- c(holdNames, paste(spaceTag,j ,sep="_"))
	 }

	 colnames(holdPredictions) <- holdNames


	 predicts <<- cbind(predicts,(holdPredictions))
  }

#print(predicts)


  cuts <<- data.frame(c_pois,c_bin,lex,partitions)
  cuts <<- cbind(cuts, predicts[,-1])



  pois_coef <<- getParGLM(cwm_poisson)
  bin_coef <<- getParGLM(cwm_binomial)
  
  
  poissonVectors <- returnVectors(pois_coef)
  binomialVectors <- returnVectors(bin_coef)
  
  
  #Begin gluing partitions through zero inflated poisson. 
  data_space <<- cbind(data,cuts)

  partSpace <<- genData(dSpace = data_space, 
                       pVectors = poissonVectors,
                       bVectors = binomialVectors)
  # PARTITIONED SPACE IS COMPLETED 
  
  
  numberOfPartitions <<- length(table(data_space$partitions))
  partitiontable <<- table(data_space$partitions)
  cat(paste('Number of Partitions:', numberOfPartitions))
  
  
 
  result <<- c()
 zeroR <- function(){


 # CUT AND PASTE PROBABILITIES


for(j in  1:max(unique(partitions))){
	
	partSpaceData <<- partSpace[[j]]$data
	pushNumber <<- partSpaceData$c_pois[1] -1





	#QUICK FUNCTION FOR ADJUSTMENT
	checkPush <- function(pushNumber) {
		    
		    if(pushNumber == 0){
			            indexPath <- ((pushNumber*5)+27)
	        return(indexPath)
		    }
	    
	    indexPath <- (((pushNumber*5)+27))

	        return(indexPath)
	}



	indexPath <<- checkPush(pushNumber)

	polProbs <<- partSpaceData[,(indexPath:(indexPath+4))]

#	print(head(polProbs))


        newData <<- cbind(partSpace[[j]]$data[,1:26],polProbs)

	partSpace[[j]] <<- updateSub(partSpace[[j]] ,newData)
						     
#	print(head(partSpace[[j]]$data)) 

}






counter <<- 1

holdModels <<- c()

  for(i in partSpace){
    #print(i)
    
    


    
    cat(    '\n '             )
    cat(  '\n '                )
    cat(paste("Now attempting maximization number",counter), '\n')
    cat("---------------------------",'\n')
    
    cat(paste('Number of observations:',length(i$data[,1])))
    
    
    if ( length(i$data[,1])>=0 ){
    tryCatch({
	
	   
	   
	holdModel <<-  z_mk1(formula = formulaZP, 
                          zipModelE = i,
                           data = i$data,
                           dist = "poisson"
			   #,control = zeroinfl.control(EM=TRUE)
			   )    
     
	holdProb <- predprob(holdModel)	
	

	holdNamesZ <- colnames(holdProb)


	probCWM <- i$data[,27:31]
	yCWM <<- i$data$ClaimNb
	

	probZIP <- predprob(holdModel)
	yZIP <<- holdModel$y


	holdNamesCWM <- colnames(probCWM)

	holdNewNames <- c()
	for (name in holdNamesCWM) {
	newName <- substring(name,4)

	holdNewNames <- c(holdNewNames,newName)
	}

	colnames(probCWM) <- holdNewNames 

	probNewZIP <- NULL
	probNewZIP <- data.frame(rep(0, length(yCWM)),
			 rep(0, length(yCWM)),
			 rep(0, length(yCWM)),
			 rep(0, length(yCWM)),
			 rep(0, length(yCWM)))
       
	colnames(probNewZIP) <- holdNewNames


	for(zNames in colnames(probZIP)){

#	print(zNames)

	indexZ <- match(zNames, holdNewNames)
	indexB <- match(zNames, colnames(probZIP))

	probNewZIP[,indexZ] <- probZIP[,indexB]

	}
	
#	print(head(probNewZIP))
	cat('\n')
	print(dim(probZIP))
	probNewZIP <- matrix(as.numeric(unlist(probNewZIP)),ncol=5)
	colnames(probNewZIP) <- holdNewNames

	
	probCWM <- matrix(as.numeric(unlist(probCWM)),ncol=5)
	colnames(probCWM) <- holdNewNames

	#print(head(as.double(unlist(probCWM))))


	 #vuongA(yCWM,yZIP,probCWM,probNewZIP)	 
    	
	vTest <<- vuongData(yCWM = yCWM,
			    yZIP = yZIP,
			    probCWM = probCWM,
			   probNewZIP = probNewZIP)	
	Sys.sleep(1)
	print("Testing object ")
	print(counter)

	vuongA(vuongObject = vTest)
      assign(paste('vuongNikT',counter,sep=''), vTest, envir = .GlobalEnv)
      assign(paste('zeroNikT', counter,sep=''), holdModel, envir = .GlobalEnv)
      
   print(summary(holdModel))



      
    },error = function(e) print(e) )
    }
    
    else{
      cat(    '\n '             )
      cat('Partition is too small')
    }
    
    counter <<- counter + 1
    
   
  } #End of for loop
 }  
 
zeroR()












  #Begin the Zero Inflated Poisson Here. 
  #class(result) <- "zcwm"
  
  #sreturn(result)
} 
#End of zcwm function



