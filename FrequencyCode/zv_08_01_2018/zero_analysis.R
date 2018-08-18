new_m <- read.csv("new_m.csv")
attach(new_m)

plot(LogDensity, ClaimNb,
     col = Clusters + 1,
     xlab = "Density",
     ylab = "Claim Nb",
     type = "p", 
     pch = 19,
     cex = 0.8)


cluster3 <- new_m[Clusters == 3,]
cluster2 <- new_m[Clusters == 2,]
cluster1 <- new_m[Clusters == 1,]


plot(cluster1$LogDensity,cluster1$ClaimNb, col = 2)
plot(cluster2$LogDensity, cluster2$ClaimNb, col = 3)
plot(cluster2$LogDensity, cluster2$ClaimNb, col = 4)


plot(cluster1$LogDensity, cluster1$ClaimNb, col = cluster1$Clusters + 1)

plot(cluster2$LogDensity, cluster2$ClaimNb, col = cluster2$Clusters + 1)

plot(cluster3$LogDensity, cluster3$ClaimNb, col = cluster3$Clusters + 1)

"
cluster 2 percentages 
0            1            2 
0.9735295377 0.0258588527 0.0006116096 
"

c1_l <- cluster1$LogDensity
c2_l <- cluster2$LogDensity
c3_l <- cluster3$LogDensity

cat(min(c1_l),mean(c1_l),max(c1_l),sd(c1_l),"\n")
cat(min(c2_l),mean(c2_l),max(c2_l),sd(c2_l),"\n")
cat(min(c3_l),mean(c3_l),max(c3_l),sd(c3_l),"\n")
detach()

