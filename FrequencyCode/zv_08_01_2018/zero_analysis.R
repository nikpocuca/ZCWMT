setwd("~/GitRepos/ZCWMT/FrequencyCode/zv_08_01_2018")
new_m <- read.csv("new_m.csv")
attach(new_m)

c_new <- new_m$partitions
c_new[c_new == 1] <- 'limegreen'
c_new[c_new == 2] <- 'red'
#c_new[c_new == 3] <- '#ffb62f'
c_new[c_new == 3] <- 'blue'
new_m$c_new <- c_new


getDensityTable <- function(in_df){
  in_log <- in_df$LogDensity
  return(c(min(in_log),mean(in_log),max(in_log),sd(in_log)))
}




#p <-  ggplot(new_m, aes(x=LogDensity, y=ClaimNb)) +
#  theme( axis.line = element_line(colour = "black"),
#         panel.background = element_blank()) + 
#  geom_point(color=c_new,
#             size = partitions/2
#             )  + xlab("Density") + ylab("Frequency") 
#p



#plot(LogDensity, ClaimNb,
#     col = partitions + 1,
#     xlab = "Density",
#     ylab = "Claim Nb",
#     type = "p", 
#     pch = 19,
#     cex = 0.8)


cluster3 <- new_m[partitions == 3,]
cluster2 <- new_m[partitions == 2,]
cluster1 <- new_m[partitions == 1,]


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

