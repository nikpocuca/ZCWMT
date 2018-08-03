# Testing script for ZCWM. 


# Load files and dataset. 
sourceCode() 

# Create formula
fr_p <- formula(ClaimNb ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge))
fr_z <- formula(ClaimNb ~ LogDensity + factor(CatCarAge) + powerF )

# Regional Data
m24 <- m[m$Region == "R24",]
m11 <- m[m$Region == "R11",]


# Run script. 
runFull <- function(input_data) {

  zero_models <<-  zcwm(inputdata = input_data, 
                        formulaP = fr_p,
                        formulaZI = fr_z,
                        Xnorms = c(LogDensity),
                        runC = 1,
                        np = 1:3)
  
}

runZero <- function(input_data){

runZ <- zcwm(inputdata = m24, 
             formulaP = fr_p,
             formulaZI = fr_z,
             Xnorms = c(LogDensity),
             np = 1:3)

}
