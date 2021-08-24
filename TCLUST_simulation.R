################################# 
######### SIMULATION ############ 
################################# 
par(mfrow=c(1,1)) 
library(mvtnorm) 
 
#generate multivariate normal data 
set.seed (10) 
sigma1 <- diag (2) ## EigenValues: 1, 1 
sigma2 <- diag (2) * 8 - 2 ## EigenValues: 8, 4 
sigma3 <- diag (2) * 50 ## EigenValues: 50, 50 
sigma4 <- diag(2) * 10-4 
mixt <- rbind (rmvnorm (360, mean = c (0.0, 0), sigma = sigma1), 
  rmvnorm (540, mean = c (5.0, 10), sigma = sigma2), 
  rmvnorm (100, mean = c (2.5, 5), sigma = sigma3), 
  rmvnorm(75, mean = c(20,35), sigma = sigma4)) 
 
plot(mixt, main = "Simulation", xlab = bquote(x[1]), ylab = bquote(x[2])) 
mixt2 = data.frame(cbind(mixt, true_clust = c(rep(2,360), rep(1,540), rep(0,100)))) 
## Warning in cbind(mixt, true_clust = c(rep(2, 360), rep(1, 540), rep(0, 
## 100))): number of rows of result is not a multiple of vector length (arg 2) 
names(mixt2) = c("x","y","true_clust") 
 
#select k , alpha 
#ctl-curves 
plot(ctlcurves(mixt, k = 1:4, alpha = seq(0, 0.20, by =  0.05))) 
## Depending on arguments x, k and alpha, this function needs some time to compute. 
## (Remove this message by setting "trace = 0") 
#kmeans 4 clusters 
sim_kmeans4 <- tclust(mixt, k = 4, alpha = 0, equal.weights = TRUE, restr.fact = 1) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 1. 

plot(sim_kmeans4,main = "K-means", xlab = bquote(x[1]), ylab = bquote(x[2]), ylim = c(-15,50), 
     xlim = c(-20,30)) 
#calculate misclassification rate 
k_means4_mr=sum(sim_kmeans4$cluster!=mixt2$true_clust)/length(mixt2$true_clust) 
 
#kmeans 3 clusters 
sim_kmeans3 <- tclust(mixt, k = 3, alpha = 0, equal.weights = TRUE, restr.fact = 1) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 1. 
plot(sim_kmeans3,main = "K-means", xlab = bquote(x[1]), ylab = bquote(x[2]), ylim = c(-15,50), 
     xlim = c(-20,30)) 
#calculate misclassification rate 
k_means3_mr=sum(sim_kmeans3$cluster!=mixt2$true_clust)/length(mixt2$true_clust) 
 
#trimmed kmeans 
sim_tkmeans <- tclust(mixt, k = 3, alpha = 0.05, equal.weights = TRUE, restr.fact = 1) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 1. 
plot(sim_tkmeans, main = "Trimmed K-means") 
#calculate misclassification rate 
tkmeans_mr <- sum(sim_tkmeans$cluster!=mixt2$true_clust)/length(mixt2$true_clust) 
 
 
#tclust 
## eigen, restr.fact = 5 
sim_tclust_eigen1 <- tclust (mixt, k = 3, alpha = 0.05, restr.fact = 5) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 5. 
plot (sim_tclust_eigen1) 
 
#calculate misclassification rate 
tclust_eigen_mr1 <- sum(sim_tclust_eigen1$cluster!=mixt2$true_clust)/length(mixt2$true_clust) 
 
## eigen, restr.fact = 50 
sim_tclust_eigen2 <- tclust (mixt, k = 3, alpha = 0.05, restr.fact = 50) 
plot (sim_tclust_eigen2) 
 
#calculate misclassification rate 
tclust_eigen_mr2 <- sum(sim_tclust_eigen2$cluster!=mixt2$true_clust)/length(mixt2$true_clust) 
 
## misclassification rate table 
misclass_table <- cbind(k_means4_mr, k_means3_mr, tkmeans_mr, tclust_eigen_mr1, tclust_eigen_mr2) 
colnames(misclass_table) <- c("K-means(k=4)", "K-means(k=3)", "TkMeans", "TCLUST(upp_bound=5)", 
"TCLUST(upp_bound=50)") 
misclass_table 
##      K-means(k=4) K-means(k=3)   TkMeans TCLUST(upp_bound=5) 
## [1,]    0.9962791    0.1665116 0.1423256           0.1209302 
##      TCLUST(upp_bound=50) 
## [1,]                 0.12 