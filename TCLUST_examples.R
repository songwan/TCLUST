############################### 
### Robust Cluster Analysis ### 
############################### 
# Pan Charoensuk 
# Songwan Joun 
# Stat 454/556 - Robust Statistics 
# Project 1 - Robust Clustering 
############################### 
 
 
############################### 
######### EXAMPLES ############ 
############################### 
 
 
############################### 
#KMEANS 
############################### 
set.seed(5) 
k_iris = kmeans(iris[,c(1,3)], centers =3) 
 
par(mfrow=c(1,1)) 
#Let's see 1st and 2nd column 
plot(iris[,1], iris[,3], col=k_iris$cluster, pch=19, 
     xlab="Sepal.Length", ylab="Petal.Length", main="K-means (k=3)") 
points(k_iris$centers[,1], k_iris$centers[,2], col="blue", pch=20, cex=5) 
idx_kmeans = which(as.numeric(iris$Species)!=k_iris$cluster) 
points(iris[idx_kmeans,1], iris[idx_kmeans,3], col=iris[idx_kmeans,5], cex=2, pch=1) 
legend("bottomright", legend = c("True", "Kmeans","Cluster center"),  
       pch=c(1,19,19), col=c("black","black","blue")) 
text(k_iris$centers, c("51","58","41"),col="white") 
 
############################### 
#Trimming 
############################### 
#install.packages("tclust") 
library ("tclust") 
## Warning: package 'tclust' was built under R version 3.5.2 
set.seed(5) 
tk_iris <- tkmeans (iris[,c(1,3)], k = 3, alpha = 0.1) 
par(mfrow=c(1,1)) 
plot (tk_iris, xlab="Sepal.Length", ylab="Petal.Length", main="Trimmed K-means (k=3)") 
 
points(tk_iris$centers[1,], tk_iris$centers[2,], col="black", pch=20, cex=5) 
text(tk_iris$centers[1,],tk_iris$centers[2,], 
c(sum(tk_iris$cluster==1),sum(tk_iris$cluster==2),sum(tk_iris$cluster==3)),col="white") 
 
idx_tkmeans = which(as.numeric(iris$Species)!=tk_iris$cluster & tk_iris$cluster!=0) 
points(iris[idx_tkmeans,1], iris[idx_tkmeans,3], col=rep(c("green","blue"), c(5,11)), 
     pch=1, cex=2) 
legend("bottomright", legend = c("True", "Kmeans"), pch=c(1,19)) 
par(mfrow=c(2,2)) 
  for(alpha in c(0.05, 0.1, 0.2, 0.4)){ 
    tk_iris <- tkmeans (iris[,c(1,3)], k = 3, alpha = alpha) 
    plot (tk_iris, xlab="Sepal.Length", ylab="Petal.Length", main="Trimmed K-means (k=3)") 
    points(tk_iris$centers[1,], tk_iris$centers[2,], col="black", pch=20, cex=5) 
    text(tk_iris$centers[1,],tk_iris$centers[2,], 
c(sum(tk_iris$cluster==1),sum(tk_iris$cluster==2),sum(tk_iris$cluster==3)),col="white") 
     
  } 
############################### 
#TCLUST 
############################### 
  
#constraint on eigenvalues 
set.seed(0) 
a = tclust(iris[,c(1,3)], alpha=0, rest=c("eigen"), restr.fact=1) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 1. 
b = tclust(iris[,c(1,3)], alpha=0, rest=c("eigen"), restr.fact=2.5) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 2.5. 
c = tclust(iris[,c(1,3)], alpha=0, rest=c("eigen"), restr.fact=30) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 30. 
par(mfrow=c(2,2)) 
plot(iris[,c(1,3)], col=c(rep("green", sum(iris$Species=="setosa")), 
                          rep("red", sum(iris$Species=="versicolor")), 
                          rep("blue", sum(iris$Species=="virginica"))), 
     main = "Actual cluster",xlim=c(3,9), ylim=c(0,9)) 
plot(a, main="Relative size of the axes",xlim=c(3,9), ylim=c(0,9)) 
plot(b, main="Relative size of the axes",xlim=c(3,9), ylim=c(0,9)) 
plot(c, main="Relative size of the axes",xlim=c(3,9), ylim=c(0,9)) 

#constraint on determinants 
set.seed(0) 
a = tclust(iris[,c(1,3)], alpha=0, rest=c("deter"), restr.fact=1) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 1. 
b = tclust(iris[,c(1,3)], alpha=0, rest=c("deter"), restr.fact=2.5) 
## Warning in .tclust.warn(O, O$ret): The result is artificially constrained 
## due to restr.fact = 2.5. 
c = tclust(iris[,c(1,3)], alpha=0, rest=c("deter"), restr.fact=10) 
par(mfrow=c(2,2)) 
plot(iris[,c(1,3)], col=c(rep("green", sum(iris$Species=="setosa")), 
                          rep("red", sum(iris$Species=="versicolor")), 
                          rep("blue", sum(iris$Species=="virginica"))), 
     main = "Actual cluster",xlim=c(3,9), ylim=c(0,9)) 
plot(a, main="Relative volumnes of the axes",xlim=c(3,9), ylim=c(0,9), col=c("red","blue","green")) 
#default had bad cluster colors 

plot(b, main="Relative volumnes of the axes",xlim=c(3,9), ylim=c(0,9)) 
plot(c, main="Relative volumnes of the axes",xlim=c(3,9), ylim=c(0,9)) 
#forcing clusters to be the same 
set.seed(0) 
#a = tclust(iris[,c(1,3)], alpha=0, rest="sigma") 
par(mfrow=c(1,2)) 
plot(iris[,c(1,3)], col=c(rep("green", sum(iris$Species=="setosa")), 
                          rep("blue", sum(iris$Species=="versicolor")), 
                          rep("red", sum(iris$Species=="virginica"))), 
     main = "Actual cluster",xlim=c(3,9), ylim=c(0,9)) 
plot(a, main="Exact same clusters",xlim=c(3,9), ylim=c(0,9)) 
#TCLUST gives an error..this explains why this example did not show the correct result in our the 
presentation... 
###################################################################################### 
############################## MULTIDIMENSIONAL IRIS ################################# 
###################################################################################### 
 
set.seed(5) 
 
# plotting CTL-Curves to determine k and alpha 
plot(ctlcurves(iris[,1:4], k = 1:5, alpha = seq(0,0.3, by = 0.03))) 
## Depending on arguments x, k and alpha, this function needs some time to compute. 
## (Remove this message by setting "trace = 0") 
#no significant improvement when increase k from 3 to 4,5. Choose k = 3 
 
# defining true clusters 
true_iris = data.frame(cbind(iris[,1:4], true_clust = c(rep(2,50), rep(1,50), rep(3,50)))) 
names(true_iris) = c("x1","x2","x3","x4","true_clust") 
 
#10 simulations 
num_sim <- 10 
misclass_table <- matrix(NA, nrow = num_sim, ncol = 4) 
for(i in 1:num_sim){ 
  set.seed(i) 
  #kmeans 3 clusters 
  iris_kmeans <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = TRUE, restr.fact = 1) 
  #calculate misclassification rate 
  k_means_mr=sum(iris_kmeans$cluster!=true_iris$true_clust)/length(true_iris$true_clust) 
   
   
  #trimmed kmeans 
  iris_tkmeans <- tclust(iris[,1:4], k = 3, alpha = 0.03, equal.weights = TRUE, restr.fact = 1) 
  #calculate misclassification rate 
  tk_means_mr=sum(iris_tkmeans$cluster!=true_iris$true_clust)/length(true_iris$true_clust) 
   
  #tclust 
  ## eigen, restr.fact = 5 
  iris_tclust.5 <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = FALSE, restr.fact = 5) 
  #calculate misclassification rate 
  tclust.5_mr=sum(iris_tclust.5$cluster!=true_iris$true_clust)/length(true_iris$true_clust) 
   
  ## eigen, restr.fact = 10 
  iris_tclust.10 <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = FALSE, restr.fact = 10) 
  #calculate misclassification rate 
  tclust.10_mr=sum(iris_tclust.10$cluster!=true_iris$true_clust)/length(true_iris$true_clust) 
   
  ## misclassification rate table 
  misclass_table[i,1] <- k_means_mr 
  misclass_table[i,2] <- tk_means_mr 
  misclass_table[i,3] <- tclust.5_mr 

  misclass_table[i,4] <- tclust.10_mr 
 
} 
colnames(misclass_table) <- c("K-means", "TkMeans", "TCLUST(upp_bound=5)", "TCLUST(upp_bound=10)") 
misclass_table 
##         K-means TkMeans TCLUST(upp_bound=5) TCLUST(upp_bound=10) 
##  [1,] 0.1066667    0.14          0.09333333           0.08666667 
##  [2,] 0.1066667    0.14          0.09333333           0.64666667 
##  [3,] 0.1066667    0.14          0.09333333           0.64000000 
##  [4,] 0.1066667    0.14          0.09333333           0.08666667 
##  [5,] 0.1066667    0.14          0.09333333           0.64666667 
##  [6,] 0.1066667    0.14          0.09333333           0.08666667 
##  [7,] 0.1066667    0.14          0.09333333           0.64000000 
##  [8,] 0.1066667    0.14          0.09333333           0.64666667 
##  [9,] 0.1066667    0.14          0.09333333           0.08666667 
## [10,] 0.1066667    0.14          0.09333333           0.08666667 
#TkMeans and TCLUST gives inconsistent results. We will investigate more. 
