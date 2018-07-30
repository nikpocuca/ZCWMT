#sinstall.packages("CASdatasets")
#install.packages("dplyr")
#install.packages("GGally")


loadData <- function() {
  
  #try not to copy above this if you have the packages installed.
  library(CASdatasets)
  library(flexCWM)
  #help(CASdatasets)
  data(freMTPLfreq) # This is CONTRACTS Data Set Pg. 477
  head(freMTPLfreq) 
  
  data(freMTPLsev) 
  head(freMTPLsev) 
  
  library(dplyr)
  fr <- freMTPLfreq
  dim(fr)
  head(fr)
  sev <- freMTPLsev
  
  #Aggregate losses by PolicyID
  library(plyr)
  ag_loss <- ddply(sev,.(PolicyID),summarize, AggClaimAmount=sum(ClaimAmount))
  dim(ag_loss)
  head(ag_loss)
  
  m <- merge(ag_loss, fr, by="PolicyID")
  head(m)
  dim(m)
  summary(m)
  
  
  m$LogAggClaimAmount <- log(m$AggClaimAmount)
  m$LogDensity <- log(m$Density)
  m$CatDriverAge <- m$DriverAge
  m$CatDriverAge[m$DriverAge < 23 ]<- 5
  m$CatDriverAge[m$DriverAge > 22 & m$DriverAge < 27] <- 4
  m$CatDriverAge[m$DriverAge > 26 & m$DriverAge < 43] <- 3
  m$CatDriverAge[m$DriverAge > 42 & m$DriverAge < 75] <- 2
  m$CatDriverAge[m$DriverAge > 74] <- 1
  
  
  m$CatCarAge <- m$CarAge
  m$CatCarAge[m$CarAge < 1] <- 1
  m$CatCarAge[m$CarAge > 0 & m$CarAge < 5] <- 2
  m$CatCarAge[m$CarAge > 4 & m$CarAge < 10] <- 3
  m$CatCarAge[m$CarAge > 9  & m$CarAge < 15] <- 4 
  m$CatCarAge[m$CarAge > 14] <- 5
  
  
  m$powerF <- factor(1*(m$Power%in%letters[4:6])+
                       + 2*(m$Power%in%letters[7:8]),labels=c("other","DEF","GH"))
  
  #m24 <- m[m$Region == 'R24',]
  #return(m24)
  return(m) 
}

loadDataServer <- function() {
  return(read.csv("m24.csv")[,-1])
}



m <- loadData()
#m24 <- loadDataServer()

set.seed(101)
runSev <- function(dataInput) {
  
  attach(dataInput)
   fitLognormal <-  cwm(formulaY= LogAggClaimAmount ~ Density + factor(CatCarAge) + factor(CatDriverAge) + Region + powerF + Gas,
                        k=1:5, data=dataInput,
                        familyY=gaussian(link="identity"),
                        Xnorm = cbind(Density),
                        iter.max = 500,
                        modelXnorm = 'V'
                        )
  
 
  fitLognormalt <- cwm(formulaY= LogAggClaimAmount ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge) + Region + powerF + Gas,
                       k=1:5, data=dataInput,
                       familyY=gaussian(link="identity"),
                       Xnorm = cbind(LogDensity),
                       iter.max = 500,
                       modelXnorm = 'V'
                       )
  
  detach(dataInput)
  
  return(list(u = fitLognormal,
              t = fitLognormalt))
}

# 

# Run the above function. 
Results <- runSev(m)

# save the results. 
u_model <- Results$u
t_model <- Results$t
save(u_model,file = "u_model")
save(t_model, file = "t_model")

# Uncomment this code so you dont have to run above again. 
#load("u_model")
#load("t_model")


getIC(u_model)
getIC(t_model)


# Some Plots for Analysis 
# ==========================================================================================

# Red and Green show to be the two highest levels of volitility, 
# teal and blue show the two least levels of volitility.  
plot(m$LogDensity,m$LogAggClaimAmount,col = getCluster(Results$t) + 1,pch = 19,cex = 0.5)


# Green and Red show to be the two newest car age owners but with two different volitilities.
plot(m$CatDriverAge,m$LogAggClaimAmount,col = getCluster(Results$t) + 1,pch = 19,cex = 0.5)
plot(m24$powerF,m24$LogAggClaimAmount,col = getCluster(Results$t) + 1,pch = 19,cex = 0.5)
plot(m24$DriverAge,m24$LogAggClaimAmount,col = getCluster(Results$t) + 1,pch = 19,cex = 0.5)

# ==========================================================================================
# Extra functions for later. 
#function part 1 conditional test
covI_check <- function(names, data_name)
{
  for (i in names) {
    if (i==data_name) return(TRUE) 
    else
      a <- 0
  }
  return(FALSE)
}


#function part 2 cwm full.
set.seed(3)
cwmt <-function(formulaY = NULL, familyY = gaussian, Xnorm = NULL, Xbin = NULL, Xpois = NULL, Xmult = NULL, modelXnorm = NULL, Xbtrials = NULL, k = 1:3, initialization = c("random.soft", "random.hard", "kmeans", "mclust", "manual"), start.z = NULL, seed = NULL, maxR = 1, iter.max = 1000, threshold = 1e-04, eps = 1e-100, parallel = FALSE , data =NULL, covI=NULL)
{
  if(is.null(data))
    stop("No dataset selected")
  if (is.null(covI))
    
    Index_matrix <- factor(colnames(data))
  Xnorm_matrix <- factor(colnames(Xnorm))
  Match_Covariates <- covI
  changed_data <- data
  
  for (i in (1:length(colnames(data)))){
    
    if (covI_check(covI,Index_matrix[i])) 
    {
      changed_data[,i] <- log(c(changed_data[,i]))
    }
    else{}
  }
  #calling global environment variables for gaussian.
  
  for (i in (1:(length(colnames(Xnorm))))){
    if (covI_check(covI, Xnorm_matrix[i])){
      #print(head(changed_data[,which(colnames(changed_data)==Xnorm_matrix[i])]))
      Xnorm[,i] <- changed_data[,which(colnames(changed_data)==Xnorm_matrix[i])]
    }
    
    else
      Xnorm[,i] <- changed_data[,which(colnames(changed_data)==Xnorm_matrix[i])]
  }
  
  data <- changed_data
  
  cwm (formulaY = formulaY, familyY = familyY, Xnorm = Xnorm, Xbin = Xbin, Xpois = Xpois, Xmult = Xmult, modelXnorm = modelXnorm, Xbtrials = Xbtrials, k = k, initialization = initialization, start.z = start.z, seed = seed, maxR = maxR, iter.max = iter.max, threshold = threshold, eps = eps, parallel = parallel , data=changed_data)
  
}

