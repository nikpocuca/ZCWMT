# Run this for loading all scripts. 
sourceCode <- function() {
  source("zcwm.R")
  source("optimizeZero.R")
  source("genSubspaces.R")
  source("returnVectors.R")
  m <<- read.csv("m.csv")
}