
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
