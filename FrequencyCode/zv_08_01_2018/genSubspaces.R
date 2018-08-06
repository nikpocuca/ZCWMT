
#| GENERATING SUBSPACES FOR DATA
#| =============================================================================================|
#| GENERATE SUBSPACE FUNCTION                                                                   |
#| Nik Pocuca August 1st - 2018                                                                 |
#| Returns object with subspaces and their respective vectors.                                  |
#| =============================================================================================|

genSubspaces <- function(dataspace, vectors_p, vectors_b){
  
  subspace <- list()
  
  for (i in unique(dataspace$partitions)){
    
    s_space <- dataspace[dataspace$partitions == i,]
    subspace[[i]] <- list(dta = s_space,
                          p_vector = vectors_p[s_space$c_pois[1],],
                          b_vector = vectors_b[s_space$c_bern[1],])
  }
  
  
  return(subspace)
}


