print("Make sure you run sourceCode() first")


fr_1 <- formula(ClaimNb ~ LogDensity + factor(CatCarAge) + factor(CatDriverAge) + log(offset(EXPOSURE)))


fr_z <- formula(ClaimNb ~ LogDensity + powerF)



zero_models <<- zcwm()




