getModels <- function (object, ...) 
{
  best <- getBestModel(object, ...)
  obj <- best$models[[1]]
  if (!is.null(obj$GLModel)) {
  
   # print(obj$GLModel)
    
      lr <- lapply(seq_len(obj$k), function(i) {
      par <- obj$GLModel[[i]]
      c(list(probs = predprob(par$model)))
    })
    names(lr) <- paste0("GLMComp.", seq_len(obj$k))
    lr
    
    
  }
  else NULL
}