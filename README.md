# Trimming approach to cluster analysis 
- Robust Statistics class project, 2019

## Preview of report_trimming_approach_to_cluster_analysis.pdf  
Clustering is a method of finding groups within data points that are similar to each other. One of the most famous clustering algorithm is **K-means** algorithm. However, in terms of robustness, K-means is heavily influenced by extreme observations. To solve this problem, we introduce robust clustering algorithm,  TCLUST  which  has  a  restriction  on  cluster  scatter  matrices.  We  will  describe  the motivation of TCLUST algorithm and mathematical derivation of it on eigenvalue ratio restrictions. Then, we will show how TCLUST is implemented in R and the simulation study results. 
<p align="middle">
<img src="https://user-images.githubusercontent.com/50762980/130583665-bec943b4-a4f8-407f-9be3-edb0764e4a98.png" width="700px">
</p>

## Preview of report_applying_robust_covariance_matrix_on_tclust.pdf  
Clustering is an unsupervised machine learning algorithm used for grouping data points based on their distance metrics. TCLUST provides general trimming approach to robust cluster analysis. However, TCLUST uses random samples to form initial scatter matrices of clusters. Also, this algorithm updates parameters based on eigenvalue-ratio restrictions. Instead, in this report, I will substitute cluster centers and scatter matrices in initialization and maximization step of TCLUST algorithm as robust covariance matrix location estimators and robust covariance matrices. This approach will prevent clusters from being seriously affected by extreme or biased data points. In the simulation study, I compared the performance of clustering by four different robust covariance matrices. The result showed that, while all of the robust covariance estimation methods compared showed  fairly  good  performance,  “Donoho-Stahel  projection  based  estimator”  showed  the minimum misclassification rate. 
<p align="middle">
<img src="https://user-images.githubusercontent.com/50762980/130585020-4f1c457a-7ee3-4551-aafe-a70000bb126b.png" width="700px">
</p>
