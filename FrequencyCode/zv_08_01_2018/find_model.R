
fr_p_v1  <- formula(ClaimNb ~ LogDensity + offset(EXPOSURE) + powerF)
fr_p_v2  <- formula(ClaimNb ~ LogDensity + offset(EXPOSURE) + powerF + Gas)
fr_p_v3  <- formula(ClaimNb ~ LogDensity + offset(EXPOSURE) + powerF + brandF )


fr_z_v1 <- formula(ClaimNb ~  factor(CatCarAge))
fr_z_v2 <- formula(ClaimNb ~  factor(CatDriverAge))
fr_z_v3 <- formula(ClaimNb ~  offset(EXPOSURE) + factor(CatCarAge))

fr_p <- list(fr_p_v1,fr_p_v2,fr_p_v3)
fr_z <- list(fr_z_v1, fr_z_v2,fr_z_v3)

setwd("Results/")
in_data = m23
count_p <- 0 
count_z <- 0 
for (p_form in fr_p) {
  count_p <- count_p + 1
  for (z_form in fr_z){
    library(pscl)
    library(flexCWM)
    count_z <- count_z + 1
    sink(paste("results_",count_p,"_",count_z,".txt",sep = ""))
    print(p_form)
    print(z_form)
     tryCatch(
       {
         zero_models <<- zcwm(inputdata = in_data, 
                              formulaP = p_form,
                              formulaZI = z_form,
                              runC = 1,
                              "BP", 
                              Xnorms = c(LogDensity),
                              np = 1:3)
         print((lapply(zero_models, summary)))
         for (i in unique(partitions)) {
           modified_wilson(zero_models[[i]],in_data,p_form,z_form)
         }
         
       },
       error = function(e){
         print(e)
       }
     ) 
    sink()
    detach()
    }
}
setwd("..")