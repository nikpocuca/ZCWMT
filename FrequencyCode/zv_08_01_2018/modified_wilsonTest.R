library(flexCWM) 



# Modified LR test referenced by Wilson et al. 2018, see paper for details.
modified_wilson <- function(zero_model,input_data,formula_P,formula_Z){
  
  # Estimate single Poisson CWM model on partition. 
  attach(input_data)
  cwm_model_single <<- cwm(formulaY = formula_P, data = input_data,
                   #Xnorm = input_Xnorms,
                   familyY = poisson(link="log"),
                   k = 1)
  
  #glm_single <<- glm(formula = formula_P, data = input_data,
                 #Xnorm = input_Xnorms,
   #              familyY = poisson(link="log"))
  
  
  detach(input_data)
  
  hold_best <- getBestModel(cwm_model_single)
  cwm_logLik <- unlist(hold_best$models)$logLik
  cwm_df <- unlist(getBestModel(cwm_model_single)$models)$df
    
  zcwm_logLik <- zero_model$loglik
  
  #Calculate Test Statistic
  chi_test_sample <- -2*(cwm_logLik - zcwm_logLik)

  # Get number of zero-inflation parameters.
  splitted_pluses <- strsplit(as.character(formula_Z),split = " + ")
  splitted_tilda <- unlist(splitted_pluses)[-1]
  zero_df <- (zero_model$n - zero_model$df.residual)
  
  diff_df = zero_df - cwm_df
  print(zero_df)
  print(diff_df)
  #Theoritcal ChiSquare 
  chi_test_theo <- qchisq(p = 0.90, df = diff_df)
  
  # BIC Calculation 
  cwmpBIC <- -1*getIC(hold_best)[6]
  
  zcwmBIC <- log(zero_model$n)*(zero_model$n - zero_model$df.residual) - 2*zero_model$loglik
  
  
  if( chi_test_theo < chi_test_sample) {
    cat("Modified LR - Test",'\n')
    cat("==================",'\n\n')
    cat(chi_test_theo ," < ", chi_test_sample,"\n")
    cat("We reject the non zero-inflated model\n")
    
  }
  
  if( chi_test_theo >= chi_test_sample){
    cat("Modified LR - Test",'\n')
    cat("==================",'\n\n')
    cat(chi_test_theo ," => ", chi_test_sample,"\n")
    cat("We do not reject the non zero-inflated model\n")
  }
  
  cat(" CWM:  ",cwmpBIC,"  ", "ZCWM:  ",zcwmBIC,"\n")
  
}