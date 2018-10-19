library(flexCWM)
setwd('~/GitRepos/ZCWMT/SeverityCode')


loadDataServer <- function() {
  return(read.csv("m.csv")[,-1])
}

m <- loadDataServer()



runSim <- function() {
  
  
  
}



# Cluster 1 
# Density mean  5.658162 (take exp ) sd 1.855599
# CatCarAge p values 0.03565062 0.30005942 0.31550802 0.26322044 0.08556150 
# CatDriverAge 0.03267974 0.49851456 0.35472371 0.05882353 0.05525847 
# 1683

#Cluster 2

# Density mean 6.156586 sd 1.974088 take exp 
# CatCarAge 0.04318418 0.30124870 0.32344780 0.24384322 0.08827610
# CatDriverAge 0.03243149 0.49080819 0.35414499 0.06763788 0.05497745 
#  5766

#Cluster 3 mean 5.75925 sd  1.923846 take exp
# CatCarAge 0.03773585 0.26886792 0.35259434 0.24056604 0.10023585 
# CatDriverAge 0.01768868 0.47405660 0.37617925 0.07311321 0.05896226 
# 800


# Component 1 
#############################################################################################

dens1 <- exp(rnorm(5.658162,sd = 1.923846,n = 1600))

prob1 <- c(0.03565062, 0.30005942, 0.31550802, 0.26322044, 0.08556150)
genCar1 <- sample(x = c(1,2,3,4,5),
                  prob = prob1,replace= TRUE,size = 1600)


prob1 <-c(0.03267974, 0.49851456,0.35472371, 0.05882353, 0.05525847)
genDrive1 <- sample(x = c(1,2,3,4,5)
                    ,prob = prob1 ,replace = TRUE,size = 1600)

mGen1 <- data.frame(dens1, genCar1, genDrive1)
#(Intercept)            7.8767124  0.1370546 57.4713 < 2.2e-16 ***
#LogDensity            -0.0318398  0.0092906 -3.4271 0.0006117 ***
#  factor(CatCarAge)2    -0.1721934  0.0803818 -2.1422 0.0321935 *  
#  factor(CatCarAge)3    -0.3961593  0.0802410 -4.9371 8.012e-07 ***
#  factor(CatCarAge)4    -0.6426737  0.0815938 -7.8765 3.591e-15 ***
#  factor(CatCarAge)5    -0.5009054  0.0906247 -5.5273 3.306e-08 ***


yLoss1 <- c()
carmatrix <- diag(x = c(1,-0.172, -0.396, -0.643, -0.501),nrow = 5, ncol =5 )

retCarCol <- function(entry){
  zeros <- c(0,0,0,0,0)
  zeros[entry] <- entry
  return(zeros)
}



for(i in 1:length(genDrive1)){
  
  
  print(retCarCol(genCar1[i]))
  print(sum(retCarCol(mGen1$genCar1[i])*carmatrix))
  y_new <- 7.880 - 0.032*mGen1$dens1[i] + sum(retCarCol(mGen1$genCar1[i])*carmatrix)
  
  yLoss1 <- append(yLoss1,exp(y_new))
}

# Component 2
#############################################################################################
dens2 <- exp(rnorm(6.156586,sd = 1.974088, n = 5700))

prob2 <- c(0.04318418, 0.30124870, 0.32344780,0.24384322, 0.08827610)
genCar2 <- sample(x = c(1,2,3,4,5),
                  prob = prob2, replace = TRUE, size = 5700)


prob2 <- c(0.03243149, 0.49080819, 0.35414499, 0.06763788, 0.05497745)
genDrive2 <- sample(x = c(1,2,3,4,5)
                    ,prob = prob2 ,replace = TRUE,size = 5700)



# Component 3 
#############################################################################################

dens3 <- exp(rnorm(5.75925,sd = 1.923846, n = 800))

prob3 <- c(0.03773585, 0.26886792, 0.35259434 ,0.24056604 ,0.10023585)
genCar3 <- sample(x = c(1,2,3,4,5),
                  prob = prob3, replace = TRUE, size = 800)
prob3 <-  c(0.01768868,0.47405660,0.37617925,0.07311321,0.05896226)
genDrive3 <- sample(x = c(1,2,3,4,5)
                    ,prob = prob3 ,replace = TRUE,size = 800)



# Density Generation 
dens <- c(dens1,dens2,dens3)

# Categorical Car Age Generation 
genCar <- c(genCar1,genCar2,genCar3)

# Categorical Driver Age Generation
genDrive <- c(genDrive1,genDrive2,genDrive3)




mGen <- data.frame(dens,genDrive,genCar)
colnames(mGen) <- c("Density","CatDriverAge","CatCarAge")




hist(rnorm(5.76925,sd = 1.923846,n = 800))
hist(m3$LogDensity,add=TRUE,col = "red")



genModel <- function(c_fs,pars_c_fs) {
  c_fs  
  
}
