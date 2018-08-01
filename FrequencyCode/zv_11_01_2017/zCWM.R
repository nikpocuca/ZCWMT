

# ZERO INFLATED CLUSTER WEIGHTED POISSON MODEL
# June 13th - 2017 
# Creator: Nik Pocuca 
# The following is code written using two libraries and their respective dependencies.
# If you had not yet installed them, please do so now, run the following function.

initLibrary <- function(){

  install.packages('flexCWM')
  install.packages('pscl')
  
  
}

# initFunctions()

# IMPORT OF LIBRARIES.
#library(flexCWM)
library(pscl)
library(flexCWM)

load_book <- function(){
    
  CONTRACTS.f <- read.csv(file = "CONTRACTS.csv")
  #BOOK#
  # factor(CONTRACTS.f$BRAND=="F",labels=c("other","F"))
  CONTRACTS.f$powerF <- factor(1*(CONTRACTS.f$POWER%in%letters[4:6])+ 2*(CONTRACTS.f$POWER%in%letters[7:8]),labels=c("other","DEF","GH"))
  CONTRACTS.f$powerF <- factor(1*(CONTRACTS.f$POWER%in%letters[4:6])+ 2*(CONTRACTS.f$POWER%in%letters[7:8]),labels=c("other","DEF","GH"))
  CONTRACTS.f$GAS <- CONTRACTS.f$Gas
  CONTRACTS.f$EXPOSURE <- CONTRACTS.f$Exposure
  CONTRACTS.f$NB  <- CONTRACTS.f$ClaimNb
  CONTRACTS.f <<- CONTRACTS.f
}
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
create24 <- function() {
  c_24 <<- CONTRACTS.f[CONTRACTS.f$Region=="R24",]
  c_24$logDENSITY <<- log(c_24$Density)
}

#CWM Addons, not found in CWM package



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



#Initialize
load_book()
create24()
attach(c_24)


fregzi <- NB ~ DriverAge + CarAge + logDENSITY + powerF

Xnorms <- cbind(DriverAge,logDENSITY,CarAge)
Xpoises <- CarAge


runZ <- function(k){

zeroTEST <- zcwm(data = c_24,
             formulaZP = fregzi,
             runC = TRUE, 
             np = k)

}


runZA <- function(k){
  zeroTEST <- zcwm(data = c_24, 
       formulaZP = fregzi, 
       runC = FALSE, 
       np = k)


}
