#sinstall.packages("CASdatasets")
#install.packages("dplyr")
#install.packages("GGally")

#try not to copy above this if you have the packages installed.
library(CASdatasets)
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

library(GGally)

#ggpairs(m[2:11])

#ClusterWeighted
library(flexCWM)

#transformation


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



#-------------------------------------------------------------------|
# The two following models are Gamma and Lognormal. 	                |
# They are estimated without the transformed variable and then with.|
#-------------------------------------------------------------------|


#put in categories
#m$DriverAge[m$DriverAge < 26] <- 4
#m$DriverAge[m$DriverAge > 25 & m$DriverAge < 30] <- 3
#m$DriverAge[m$DriverAge > 29 & m$DriverAge < 50] <- 2
#m$DriverAge[m$DriverAge > 49 & m$DriverAge < 75] <- 1
#m$DriverAge[m$DriverAge > 74] <- 1





m$LogAggClaimAmount <- log(m$AggClaimAmount)
m$LogDensity <- log(m$Density)
# filtering through 

#mr <- subset(m, m$Region == "R24")

# Only need to run this once. 
attach(mr)


fitLognormal <- cwm(formulaY= LogAggClaimAmount ~ Density + CarAge + DriverAge ,
                     k=1:3, data=mr,
                     familyY=gaussian(link="identity"),
                     initialization='random.hard',
                     Xnorm = cbind(LogDensity,DriverAge),
                     Xpois = CarAge,
                     iter.max = 300)

fitLognormalt <- cwm(formulaY= LogAggClaimAmount ~ LogDensity + CarAge + DriverAge ,
                     k=1:3, data=mr,
                     familyY=gaussian(link="identity"),
                     initialization='random.hard',
                     Xnorm = cbind(LogDensity,DriverAge),
                     Xpois = CarAge, 
                     iter.max = 300)
detach(mr)
getIC(fitLognormal)
getIC(fitLognormalt)


