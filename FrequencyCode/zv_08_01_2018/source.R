# Run this for loading all scripts. 
sourceCode <- function() {
  source("zcwm.R")
  source("optimizeZero.R")
  source("genSubspaces.R")
  source("returnVectors.R")
  source("split_formulas.R")
  source("modified_wilsonTest.R")
  m <<- read.csv("m.csv")
}
