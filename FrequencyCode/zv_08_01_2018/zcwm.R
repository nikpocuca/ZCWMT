# Function definition for zcwm

# Dependencies 
library(flexCWM)
library(pscl)

zcwm <- function(inputdata, formulaZP, np, Xnorms, ...){
  
  # Create zero space. 
  data_z <- inputdata
  
    #Get dependent variable.  
    dep_v_name <- as.character(formulaZP[2])
    dep_v_index <- match(dep_v_name,colnames(inputdata))
    
  
  data_z[,dep_v_index] <- as.integer(data_z[,dep_v_index] <= 0)
    
  cat('\n Beginning Partition using CWM','\n')
  cat(' ==============================','\n\n')

  # First Partition Poisson
  
  cat('\n')
  cat(' Poisson Model\n\n')
  
  attach(inputdata)
  cwm_poisson <- cwm(formulaY = formulaZP,
                     data = inputdata,
                     familyY = poisson(link= "log" ), Xnorm = Xnorms, ...)
  detach(inputdata)
  
  # Second Partition Bernoulli 
  
  cat('\n')
  cat('Bernoulli Zero Inflation Model \n\n')
  
  attach(data_z)
  cwm_bernoulli <- cwm(formulaY = formulaZP,
                     data = data_z,
                     familyY = binomial(link= "logit" ), Xnorm = Xnorms, ...)
  detach(data_z)
  
  
  # Match Partitions. 
  cat('\n Beginning Zero inflated CWM','\n')
  cat(' ==============================','\n\n')
  
  
  
  
  
  
  return(list(poisson_model = cwm_poisson,
              bernoulli_model = cwm_bernoulli ))
}