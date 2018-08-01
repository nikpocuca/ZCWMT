

# Testing script for ZCWM. 

# Load files and dataset. 
sourceCode() 

fr_zi <- formula(ClaimNb ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge) + Region)


#runZ <- zcwm(inputdata = m, 
#             formulaZP = fr_zi,
#             Xnorms = c(LogDensity),
#             np = 3)

load('cwm_poisson')
load('cwm_bern')
