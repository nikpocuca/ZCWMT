# Function definition for zcwm

# Dependencies 
library(flexCWM)
library(pscl)


#| FULL ZCWM FUNCTION 
#| =============================================================================================|                                                                  |
#| Nik Pocuca August 1st - 2018                                                                 |
#| Returns a zero-inflated cwm object                                                           |
#| =============================================================================================|

zcwm <- function(inputdata, formulaP, formulaZI,runC, np, Xnorms, ...){
  
  if (runC == 1){
  # Create zero space. 
  data_z <- inputdata
  
    #Get dependent variable.  
    dep_v_name <- as.character(formulaZI[2])
    dep_v_index <- match(dep_v_name,colnames(inputdata))
    
  
  data_z[,dep_v_index] <- as.integer(data_z[,dep_v_index] <= 0)
    
  cat('Beginning Partition using CWM','\n')
  cat('==============================','\n\n')

  # First Partition Poisson
  
  cat('\n')
  cat(' Poisson Model\n\n')
  
  attach(inputdata)
  cwm_poisson <<- cwm(formulaY = formulaP,
                     data = inputdata,
                     familyY = poisson(link= "log" ), Xnorm = Xnorms, modelXnorm = "V" ,k = np, ...)
  detach(inputdata)
  
  # Second Partition Bernoulli 
  
  cat('\n')
  cat('Bernoulli Zero Inflation Model \n\n')
  
  attach(data_z)
  cwm_bernoulli <<- cwm(formulaY = formulaZI,
                     data = data_z,
                     familyY = binomial(link= "logit" ), modelXnorm = "V", Xnorm = Xnorms, k = np, ...)
  detach(data_z)
  }
  
  # Match Partitions. 
  cat('Beginning Zero inflated CWM','\n')
  cat('==============================','\n\n')
  
  # Gather labels for splitting
  c_pois <- getCluster(cwm_poisson)
  c_bern <- getCluster(cwm_bernoulli)
  lex <- paste(c_pois,c_bern, sep="")
  partitions <<- match(lex, unique(lex))
  
  dataspace <- cbind(inputdata,c_pois,c_bern,partitions)
  
  # Gather Vectors
  glm_pois <- getParGLM(cwm_poisson)
  glm_bern <- getParGLM(cwm_bernoulli)
  
  v_pois <- returnVectors(glm_pois)
  v_bern <- returnVectors(glm_bern)
 
  
  subSpaces <- genSubspaces(dataspace = dataspace,
                          vectors_p = v_pois,
                          vectors_b = v_bern)

  new_zeroinflated_formula <- split_formulas(formula_P = formulaP,
                                             formula_Z = formulaZI)
 
  # I need to parallelize this section in the future 
  count <- 1
  hold_models <- list() 
  for (sub_space in subSpaces){
   
   cat(paste('Optimizing Partition - ', count),'\n')  
    zero_model <- optimizeZeroInflation(subspace = sub_space,formulaZ = new_zeroinflated_formula)
    hold_models[[count]] <- zero_model
    
   print(summary(hold_models[[count]]))
   
    count <- count + 1
  }
  
  return(hold_models)
}


