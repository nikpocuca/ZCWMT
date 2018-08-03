

split_formulas <- function(formula_P, formula_Z){
  
  strings_p <- as.character(formula_P)
  strings_z <- as.character(formula_Z)
  
  zero_inflated_formula <- formula(paste(strings_p[2],strings_p[1],strings_p[3],"|",strings_z[3] ))
  
  return(zero_inflated_formula)
}
