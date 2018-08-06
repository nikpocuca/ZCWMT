# Testing script for ZCWM. 


# Load files and dataset. 
sourceCode() 

# Create formula
fr_p <- formula(ClaimNb ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge))
fr_z <- formula(ClaimNb ~ LogDensity + factor(CatDriverAge) + powerF )

fr_pr <- formula(ClaimNb ~ LogDensity + Region + powerF )
fr_zr <- formula(ClaimNb ~ LogDensity + factor(CatDriverAge) )


# Regional Data
m24 <- m[m$Region == "R24",]
m11 <- m[m$Region == "R11",]
m23 <- m[m$Region == "R23",]
m25 <- m[m$Region == "R25",]
m31 <- m[m$Region == "R31",]
m72 <- m[m$Region == "R72",]


# Run script. 
runFull <- function(input_data) {

  zero_models <<-  zcwm(inputdata = input_data, 
                        formulaP = fr_p,
                        formulaZI = fr_z,
                        Xnorms = c(LogDensity),
                        runC = 1,
                        np = 1:4)
  
}

runZero <- function(input_data){

zero_models <<- zcwm(inputdata = input_data, 
             formulaP = fr_p,
             formulaZI = fr_z,
	     runC = 0,
             Xnorms = c(LogDensity),
             np = 1:4)

}



runFullR <- function(input_data) {

  zero_models <<-  zcwm(inputdata = input_data, 
                        formulaP = fr_pr,
                        formulaZI = fr_zr,
                        Xnorms = c(LogDensity),
                        runC = 1,
                        np = 1:4)
}



runZeroR <- function(input_data){

zero_models <<- zcwm(inputdata = input_data, 
             formulaP = fr_pr,
             formulaZI = fr_zr,
	     runC = 0,
             Xnorms = c(LogDensity),
             np = 1:4)

}





