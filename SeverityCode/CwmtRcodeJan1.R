#sinstall.packages("CASdatasets")
#install.packages("dplyr")
#install.packages("GGally")
setwd("~/ZCWMT/SeverityCode/")

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
  return(read.csv("m.csv")[,-1])
}


# Load data depending on where you are, you can either do it from the package, or locally. 
#m <- loadData()
#library(flexCWM)
library(flexCWMz)
set.seed(101)
#set.seed(501)
#set.seed(1000)
runSev <- function(dataInput) {
  
  attach(dataInput)
  
  fitLognormalt <- cwm(formulaY= LogSeverity ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge)+ factor(Region)+ powerF, # + Region + powerF + Gas,
                       k=4, data=dataInput,
                       familyY = gaussian(link="identity"),
                       Xnorm = cbind(LogDensity),
                       #iter.max = 1000,
                       start.z = "random.soft",
                       modelXnorm = 'V')
  
  detach(dataInput)
  
  return(list(u = fitLognormalt))
}

# 
#Results <- runSev(m)
#ClaimGlobal <- m$ClaimNb
# Run the above function. 
#Results <- runSev(m)

#t_model <- Results$u
#save(u_model,file = "u_model")
#save(t_model, file = "t_model")
# ============================================================

library(ggplot2)
detach(package:flexCWM)
library(flexCWMz)
m <- loadDataServer()
m$Severity <- m$AggClaimAmount/m$ClaimNb
m$LogSeverity <- log(m$Severity)
ClaimGlobal <- m$ClaimNb
Results <- runSev(m)
m$clusters <- getCluster(Results$u) +1
c_new <- m$clusters
c_new[c_new == 2] <- 'limegreen'
c_new[c_new == 3] <- 'red'
c_new[c_new == 4] <- '#ffb62f'
c_new[c_new == 5] <- 'blue'
m$c_new <- c_new

p <-  ggplot(m, aes(x=LogDensity, y=LogSeverity)) +
     theme( axis.line = element_line(colour = "black"),
            panel.background = element_blank()) + 
  geom_point(color=c_new)  + xlab("Density") + ylab("Severity") 
p


#plot(m$LogDensity, m$LogSeverity, col = m$clusters, main = "Individually Weighted")
table(getCluster(Results$u))
print("RUNNING INDIVIDUALLY WEIGHTED")

# ============================================================
detach(package:flexCWMz)
library(flexCWM)
library(ggplot2)
Results <- runSev(m)
table(getCluster(Results$u))
m$clusters <- getCluster(Results$u) + 1
c_new <- m$clusters
c_new[c_new == 2] <- 'blue' 
c_new[c_new == 3] <- 'orange'
c_new[c_new == 4] <- 'red'
c_new[c_new == 5] <- 'limegreen'
m$c_new <- c_new
p <- NULL
q <-  ggplot(m, aes(x=LogDensity, y=LogSeverity)) +
  geom_point(color=c_new)  + xlab("Density") + ylab("Severity") 
q
table(getCluster(Results$u))
table(c_new)
print("RUNNING ClUSTER WEIGHTED")

# Get Volatility 

#Green
m1 <- m[m$clusters -1 == 1,]
m1_vol <- c(min(m1$Severity), mean(m1$Severity), max(m1$Severity), sd(m1$Severity))
m1_vol

# Red
m2 <- m[m$clusters -1 == 2,]
m2_vol <- c(min(m2$Severity), mean(m2$Severity), max(m2$Severity), sd(m2$Severity))
m2_vol


# Orange
m3 <- m[m$clusters - 1 == 3,]
m3_vol <- c(min(m3$Severity), mean(m3$Severity), max(m3$Severity), sd(m3$Severity))
m3_vol

# Blue
m4 <- m[m$clusters - 1 == 4,]
m4_vol <- c(min(m4$Severity), mean(m4$Severity), max(m4$Severity), sd(m4$Severity))
m4_vol






# =============================================================

getIC(u_model)
getIC(t_model)

u_model_table <- data.frame()
# Extracting tables of IC. 
for (i in 1:length(unlist(getIC(u_model)[,1]))){
  u_model_table <- rbind(u_model_table,unlist(getIC(u_model))[i,]); 
  
}
colnames(u_model_table) <- c("AIC","AICc","AICu","AIC3","AWE","BIC","CAIC","ICL")

u_model_table_AIC <- u_model_table$AIC
u_model_table_BIC <- u_model_table$BIC
u_model_table_components <- 1:6
u_model_table_final <- cbind(u_model_table_components,
                             u_model_table_AIC,
                             u_model_table_BIC)
colnames(u_model_table_final) <- c("k","AIC","BIC")

stargazer(u_model_table_final,summary = FALSE)




t_model_table <- data.frame()
# Extracting tables of IC. 
for (i in 1:length(unlist(getIC(t_model)[,1]))){
  t_model_table <- rbind(t_model_table,unlist(getIC(t_model))[i,]); 
  
}
colnames(t_model_table) <- c("AIC","AICc","AICu","AIC3","AWE","BIC","CAIC","ICL")

t_model_table_AIC <- t_model_table$AIC
t_model_table_BIC <- t_model_table$BIC
t_model_table_components <- 1:6
t_model_table_final <- cbind(t_model_table_components,
                             t_model_table_AIC,
                             t_model_table_BIC)
colnames(t_model_table_final) <- c("k","AIC","BIC")

stargazer(t_model_table_final,summary = FALSE)

# Some Plots for Analysis 
# ==========================================================================================

# Red and Green show to be the two highest levels of volitility, 
# teal and blue show the two least levels of volitility.  
plot(m24$LogDensity,m24$LogSeverity,col = getCluster(t_model) + 1,pch = 19,
     #main = "Claims vs. Density Transformed",
     xlab = "Density",
     ylab = "Loss Amount",
     cex = 0.5)


getStatistics <- function(inputData) {
  m_stat <- mean(as.numeric(inputData))
  sd_stat <- sd(as.numeric(inputData))
  mx_stat <- max(as.numeric(inputData))
  mn_stat <- min(as.numeric(inputData))
  size_stat <- length(inputData)
  return(list(m_t = m_stat,
       sd_t = sd_stat,
       mx_t = mx_stat,
       mn_t = mn_stat,
       sz_t = size_stat))
}


m$Clusters <- getCluster(t_model)

m1 <- m[m$Clusters == 1,]
m2 <- m[m$Clusters == 2,]
m3 <- m[m$Clusters == 3,]
m4 <- m[m$Clusters == 4,]
  
stat_1 <- getStatistics(m1$AggClaimAmount) # red
stat_2 <- getStatistics(m2$AggClaimAmount) # green
stat_3 <- getStatistics(m3$AggClaimAmount) # blue
stat_4 <- getStatistics(m4$AggClaimAmount) # teal 
stat_total <- stat_1$sz_t + stat_2$sz_t + stat_3$sz_t + stat_4$sz_t
stat_perc <- c(stat_1$sz_t/stat_total,
               stat_2$sz_t/stat_total,
               stat_3$sz_t/stat_total,
               stat_4$sz_t/stat_total)


# ===== stargazering the summary table. ================#




# ==========================================================================================
# Extra functions for building the package in the future. 
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

m$Density <- LogDensity
attach(m)
holdModel <-  cwm(formulaY= LogAggClaimAmount ~ LogDensity + CatCarAge + CatDriverAge + Region + powerF + Gas,
                     k=1, data=m,
                     familyY=gaussian(link="identity"),
                     Xnorm = cbind(Density),
                     iter.max = 500,
                     modelXnorm = 'V')




getFitted <- function(object, ...){
  best <- getBestModel(object,...)
  obj  <- best$models[[1]]
  if (!is.null(obj$GLModel)){
    lr <- lapply(seq_len(obj$k), function(i){
      par <- obj$GLModel[[i]]
      c(list(fitted=par$model$fitted.values),par[-1])
    })
    names(lr) <- paste0("GLMComp.",seq_len(obj$k))
    lr
  } else NULL
}


getResiduals <- function(object, ...){
  best <- getBestModel(object,...)
  obj  <- best$models[[1]]
  if (!is.null(obj$GLModel)){
    lr <- lapply(seq_len(obj$k), function(i){
      par <- obj$GLModel[[i]]
      c(list(resid=par$model$residuals),par[-1])
    })
    names(lr) <- paste0("GLMComp.",seq_len(obj$k))
    lr
  } else NULL
}

fit_u <- glm(formula = formula(LogSeverity ~ Density + factor(CatCarAge) + factor(CatDriverAge) + Region + powerF),
             family = gaussian(link="identity"),
             data = m )

fit_t <- glm(formula = formula(LogSeverity ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge) + Region + powerF),
             family = gaussian(link="identity"),
             data = m)
