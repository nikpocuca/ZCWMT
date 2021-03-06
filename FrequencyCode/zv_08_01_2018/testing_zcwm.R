# Testing script for ZCWM. 
setwd("~/GitRepos/ZCWMT/FrequencyCode/zv_08_01_2018/")

# Load files and dataset. 
sourceCode() 

# Create formula
fr_p <- formula(ClaimNb ~ LogDensity + offset(EXPOSURE) + powerF)
fr_z <- formula(ClaimNb ~  factor(CatCarAge) + offset(EXPOSURE))

fr_pr <- formula(ClaimNb ~ LogDensity + Region + powerF + offset(EXPOSURE))
fr_zr <- formula(ClaimNb ~ LogDensity + factor(CatDriverAge) + offset(EXPOSURE))


# Regional Data
m24 <- m[m$Region == "R24",]
m11 <- m[m$Region == "R11",]
m23 <- m[m$Region == "R23",]
m25 <- m[m$Region == "R25",]
m31 <- m[m$Region == "R31",]
m72 <- m[m$Region == "R72",]


# Run script. 
runFull <- function(input_data,method,g) {
  
  print(fr_p)
  print(fr_z)
  zero_models <<-  zcwm(inputdata = input_data, 
                        formulaP = fr_p,
                        formulaZI = fr_z,
                        Xnorms = c(LogDensity),
                        runC = 1,
                        method = method, 
                        np = g)
  
}

runZero <- function(input_data,method,g){

print(fr_p)
print(fr_z)
zero_models <<- zcwm(inputdata = input_data, 
             formulaP = fr_p,
             formulaZI = fr_z,
	     runC = 0,
	            method, 
             Xnorms = c(LogDensity),
             np = g)

}



runFullR <- function(input_data,method,g) {
  
  print(fr_pr)
  print(fr_zr)
  zero_models <<-  zcwm(inputdata = input_data, 
                        formulaP = fr_pr,
                        formulaZI = fr_zr,
                        Xnorms = c(LogDensity),
                        runC = 1,
                        method = method, 
                        np = g)
}



runZeroR <- function(input_data,method,g){
print(fr_pr)
print(fr_zr)
zero_models <<- zcwm(inputdata = input_data, 
             formulaP = fr_pr,
             formulaZI = fr_zr,
	           runC = 1,
             Xnorms = c(LogDensity),
	           method = method, 
             np = g)

}


# Toy Example Takes 5 minutes to run. 
in_data = m

runFull(in_data,method = "BP",g = 1:6)
# Note that BIC's are very close, the LR test shows a 
space <- 1
modified_wilson(zero_model = zero_models[[space]], input_data = in_data[partitions == space,],
                formula_P = fr_p,
                formula_Z = fr_z)


space <- 2
modified_wilson(zero_model = zero_models[[space]], input_data = in_data[partitions == space,],
                formula_P = fr_p,
                formula_Z = fr_z)




