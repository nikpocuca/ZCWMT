

# Testing script for ZCWM. 

# Load files and dataset. 
sourceCode() 

fr_zi <- formula(ClaimNb ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge) )

m24 <- m[m$Region == "R24",]

runZ <- zcwm(inputdata = m24, 
             formulaZP = fr_zi,
             Xnorms = c(LogDensity),
             np = 1:3)


