library(flexCWM) 



# Modified LR test referenced by Wilson et al. 2018, see paper for details.
modified_wilson <- function(zcwm_model,input_data,formula_P,formula_Z,input_Xnorms){
  
  
  # Estimate single Poisson CWM model on partition. 
  attach(input_data)
  cwm_model_single <- cwm(formulaY = formula_P, data = input_data,
                   Xnorm = input_Xnorms,
                   familyY = poisson(link="log"),
                   k = 1)
  
  detach(input_data)
  
  hold_best <- getBestModel(cwm_model_single)
  cwm_logLik <- unlist(hold_best$models)$logLik
  
  zcwm_logLik <- zcwm_model$loglik
  
  #Calculate Test Statistic
  chi_test_sample <- -2*(cwm_logLik - zcwm_logLik)

  # Get number of zero-inflation parameters.
  splitted_pluses <- strsplit(as.character(formula_Z),split = " + ")
  splitted_tilda <- unlist(splitted_pluses)[-1]
  zero_df <- length(splitted_tilda)
  
  #Theoritcal ChiSquare 
  chi_test_theo <- qchisq(p = 0.90, df = zero_df)
  
  if( chi_test_theo < chi_test_sample) {
    cat("Modified LR - Test",'\n')
    cat("==================",'\n\n')
    cat(chi_test_theo ," < ", chi_test_sample)
    cat("We reject the non zero-inflated model")
  }
  
  if( chi_test_theo >= chi_test_sample){
    cat("Modified LR - Test",'\n')
    cat("==================",'\n\n')
    cat(chi_test_theo ," => ", chi_test_sample)
    cat("We do not reject the non zero-inflated model")
  }
  
}