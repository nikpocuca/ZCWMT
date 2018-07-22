

# ZERO INFLATED CLUSTER WEIGHTED POISSON MODEL
# June 13th - 2017 
# Creator: Nik Pocuca 
# The following is code written using two libraries and their respective dependencies.
# If you had not yet installed them, please do so now, run the following function.

initLibrary <- function(){
  install.packages('flexCWM')
  install.packages('pscl') }

# IMPORT OF LIBRARIES.
library(flexCWM)
library(pscl)


load_book <- function(){    
  CONTRACTS.f <- read.csv(file = "CONTRACTS.csv")
  CONTRACTS.f$powerF <- factor(1*(CONTRACTS.f$POWER%in%letters[4:6])+ 2*(CONTRACTS.f$POWER%in%letters[7:8]),labels=c("other","DEF","GH"))
  CONTRACTS.f$powerF <- factor(1*(CONTRACTS.f$POWER%in%letters[4:6])+ 2*(CONTRACTS.f$POWER%in%letters[7:8]),labels=c("other","DEF","GH"))
  CONTRACTS.f$GAS <- CONTRACTS.f$Gas
  CONTRACTS.f$EXPOSURE <- CONTRACTS.f$Exposure
  CONTRACTS.f$NB  <- CONTRACTS.f$ClaimNb
  CONTRACTS.f <<- CONTRACTS.f }

#  This needs to be replaced with attach later on. 
declare_g <- function(data){
  AGECAR <<- data$AGECAR
  AGEDRIVER <<- data$AGEDRIVER
  Brand <<- data$Brand
  BRAND <<- data$BRAND
  brandF <<- data$brandF
  CarAge <<- data$CarAge
  ClaimNb <<- data$ClaimNb
  Density <<- data$Density
  DENSITY <<- data$DENSITY
  DriverAge <<- data$DriverAge
  data$powerF <- factor(1*(data$Power%in%letters[4:6])+
                       + 2*(data$Power%in%letters[7:8]),labels=c("other","DEF","GH"))
  Exposure <<- data$Exposure
  EXPOSURE <<- data$EXPOSURE
  Gas <<- data$Gas 
  GAS <<- data$GAS
  NB <<- data$NB 
  Power <<- data$Power
  POWER <<- data$POWER
  powerF <<- data$powerF
  Region <<- data$Region
  logDENSITY <<- data$logDENSITY
}

#Extract data from region24. 
create24 <- function() {
  c_24 <<- CONTRACTS.f[CONTRACTS.f$Region=="R24",]
  c_24$logDENSITY <<- log(c_24$Density)
}

# Creates catagories that are used for actuarial pricing. 
createCat <- function(data) {
  data$CatDriverAge <- data$DriverAge
  data$CatDriverAge[data$DriverAge < 26] <- 4
  data$CatDriverAge[data$DriverAge > 25 & data$DriverAge < 30] <- 3
  data$CatDriverAge[data$DriverAge > 29 & data$DriverAge < 50] <- 2
  data$CatDriverAge[data$DriverAge > 49 & data$DriverAge < 75] <- 1
  data$CatDriverAge[data$DriverAge > 74] <- 1
  CatDriverAge <<- data$CatDriverAge
  return(data)
}


# Extra CWM addons that are not included in the package. 

getFitted <- function(object, ...){
  best <- getBestModel(object,...)
  obj  <- best$models[[1]]
  if (!is.null(obj$GLModel)){
    lr <- lapply(seq_len(obj$k), function(i){
      par <- obj$GLModel[[i]]
      c(list(fitted=par$model$fitted.values),par[-1])
    })
    names(lr) <- paste0("GLMComp.",seq_len(obj$k))
    lr
  } else NULL
}
getResiduals <- function(object, ...){
  best <- getBestModel(object,...)
  obj  <- best$models[[1]]
  if (!is.null(obj$GLModel)){
    lr <- lapply(seq_len(obj$k), function(i){
      par <- obj$GLModel[[i]]
      c(list(resid=par$model$residuals),par[-1])
    })
    names(lr) <- paste0("GLMComp.",seq_len(obj$k))
    lr
  } else NULL
}


# Initialize. 
load_book()
create24()
c_24 <- createCat(c_24)
attach(c_24)


# Formula for GLM model.  
fregzi <- NB ~ CatDriverAge + CarAge + logDENSITY + powerF

# Declare independent variables in cwm. 
Xnorms <- logDENSITY
Xpoises <- CarAge

# Run Z runs all the cwm models from scratch, and generates the inflated models. 
runZ <- function(k){
    zeroRun <- zcwm(data = c_24,
                formulaZP = fregzi,
                runC = TRUE, 
                np = k) }

# Run ZA uses the existing cwm_poisson, and cwm_binomial models, takes a lot less, then runs the inflated models. 
runZA <- function(k){
  zeroRun <- zcwm(data = c_24, 
       formulaZP = fregzi, 
       runC = FALSE, 
       np = k) }
