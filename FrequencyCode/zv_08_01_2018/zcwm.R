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
    
  cat('Beginning Partition using CWM','\n')
  cat('==============================','\n\n')

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
  cat('Beginning Zero inflated CWM','\n')
  cat('==============================','\n\n')
  
  # Gather labels for splitting
  c_pois <- getCluster(cwm_poisson)
  c_bern <- getCluster(cwm_bernoulli)
  lex <- paste(c_pois,c_bern, sep="")
  partitions <- match(lex, unique(lex))
  
  dataspace <- cbind(inputData,c_pois,c_bern,partitions)
  
  # Gather Vectors
  glm_pois <- getParGLM(cwm_poisson)
  glm_bern <- getParGLM(cwm_bernoulli)
  
  v_pois <- returnVectors(glm_pois)
  v_bern <- returnVectors(glm_bern)
  
  subSpace <- genSubspace(dataspace = dataspace,
                          vectors_p = v_pois,
                          vectors_b = v_bern)
  
  return(subSpace)
}






#| RESTRUCTURING COEFFICIENTS
#| =============================================================================================|
#| RETURN VECTORS FUNCTION                                                                      |
#| Nik Pocuca July 19th - 2017                                                                  |
#| Returns glm vectors in the form of a dataframe.                                              |
#| =============================================================================================|
returnVectors <- function(cwmGLM){
  
  # Get names
  vNames <-  names(cwmGLM$GLMComp.1$coefficients)
  
  
  # Placeholder dataframe.
  dataPlaceholder <-  data.frame()
  for(i in cwmGLM){
    dataPlaceholder <- rbind(dataPlaceholder, i$coefficients)
  }
  
  
  # Set Names for dataframe
  colnames(dataPlaceholder) <- vNames
  
  return(dataPlaceholder)} # END OF RETURN VECTORS FUNCTION
#| =============================================================================================|



#| GENERATING SUBSPACES FOR DATA
#| =============================================================================================|
#| GENERATE SUBSPACE FUNCTION                                                                   |
#| Nik Pocuca August 1st - 2018                                                                 |
#| Returns object with subspaces and their respective vectors.                                  |
#| =============================================================================================|

genSubspace <- function(dataspace, vectors_p, vectors_b){
  
  subspace <- list()
  
  for (i in unique(dataspace$partitions)){
  
    s_space <- dataspace[partitions == i,]
    subspace[[i]] <- list(dta = s_space,
                          p_vector = vectors_p[s_space$c_pois[1],],
                          b_vector = vectors_b[s_space$c_bern[1],])
    
  }
  
  return(subspace)
}



