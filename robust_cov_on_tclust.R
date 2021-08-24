rm(list=ls())
library(mvtnorm);library(robustbase);library(robust)
##########
# define function
##########
add <- function(x) Reduce("+", x)

formmatrix = function(a,b,c,d,e,f,p){
  m1 = c(0,8,rep(0, p-2))
  m2 = c(8,0,rep(0, p-2))
  m3 = c(-8,-8,rep(0, p-2))

  sig1 = diag(c(1,a, rep(1, p-2)))
  sig2 = diag(c(b,c, rep(1, p-2)))
  sig3 = matrix(c(d,e,e,f),2,2,byrow=T)
  sig3 = cbind(sig3, matrix(rep(0,2*(p-2)),2))
  sig3 = rbind(sig3, cbind(t(matrix(rep(0,2*(p-2)),2)), diag(rep(1, p-2))))
   
  return(list(m1=m1, m2=m2, m3=m3, sig1=sig1, sig2=sig2, sig3=sig3))
}

formdata = function(n, sim1){
  dat1 = rmvnorm(n = n, mean = sim1$m1, sigma = sim1$sig1)
  dat2 = rmvnorm(n = n, mean = sim1$m2, sigma = sim1$sig2)
  dat3 = rmvnorm(n = n, mean = sim1$m3, sigma = sim1$sig3)

  dat1 = cbind(dat1, rep(1,n))
  dat2 = cbind(dat2, rep(2,n))
  dat3 = cbind(dat3, rep(3,n))

  data = rbind(dat1, dat2, dat3)
  return(data)
}

##########
# simulation study
##########
for(xx in 1:4){
  set.seed(1234)
  par(mfrow=c(1,1))
  k = 3 #number of cluster
  p=6 #dimension #########################
  alpha = 0.1#trimming level
  q = 10 ######################
  numsim = 2000
  estim=c("mcd", "weighted", "donostah", "M")[xx]
  sim1 = formmatrix(1,1,1,1,0,1, p)
  sim2 = formmatrix(5,1,5,1,0,5, p)
  sim3 = formmatrix(5,5,1,3,-2,3, p)
  sim4 = formmatrix(1,20,5,15,-10,15, p)
  sim5 = formmatrix(1, 45, 30, 15, -10, 15, p)
  sim=sim1
  simnum=1

  ##########
  # plot data
  ##########
  data2 = formdata(numsim, sim)
  data = data2[,-(p+1)]
  n = dim(data)[1]

  ##########
  # Algorithm
  ##########
  #1. Initialization
  # Randomly choose weights w
  rnum = runif(k)
  w = rnum/sum(rnum)

  for(ii in 1:100){
    # randomly select init.num =  k*(p+1)
    init.num =  k*(p+q)
    idx = sample(1:dim(data)[1], init.num, replace = F)
    init.pt = data[idx,]

    # validate with kmeans
    km = kmeans(init.pt, k)
    if(sum(table(km$cluster)>=rep((p+3),k))==k){
      print("Properly sampled initial points")
      break;
    }
  }

  init.idx3 = lapply(1:k, function(x){which(x==km$cluster)})

  #(Version 1) calculate initial sample cov: tmat
  tmat = lapply(init.idx3, function(x){covClassic(init.pt[x,])})

  #(Version 2) use robust cov matrix for initial S
  smat = lapply(init.idx3, function(x){covRob(init.pt[x,], estim = estim)})

  #visualize
  model = lapply(init.idx3, function(x){
    fit.models(list(Robust="covRob", Classical="covClassic"),
               data=init.pt[x,])
  })

  #calculate initial mean mu
  mu = sapply(smat, function(x){x$center})

  #### Calculate distance metric (with w(weight), mu, smat)
  D = vector("list", k)
  for(ii in 1:k){
    D[[ii]] = w[ii]*dmvnorm(data, mean = mu[,ii], sigma = smat[[ii]]$cov)
  }

  D.nrom = add(D)
  post.p = sapply(D, function(x){x/D.nrom}) #data will be assigned to max post.p
  max.p = apply(post.p, 1, max)
  new.assign = unlist(apply(post.p, 1, which.max))

  #trim [n*alpha] observations with smallest max.p
  if(floor(n*alpha)!=0){
    idx.trim = order(max.p)[1:floor(n*alpha)]
    new.assign[idx.trim] = 0
  }

  #points(data, col=factor(new.assign))
  old_assign = new.assign

  ##########
  #Loop until convergence / max.iter
  ##########
  max.iter=9
  png(filename = paste0("C://Users//User//Dropbox//2019.Spring//STAT556_1_Robust//05_Project2//image//",
                        simnum, "_", p,"_",q,"_",estim,"_",alpha,"_2.png"), width = 900, height = 900)
  par(mfrow=c(3,3))

  for(nn in 1:max.iter){
    #update w
    w = table(new.assign)/length(new.assign)

    #update smat
    smat = lapply(1:k, function(x){covRob(data[new.assign==x,])})
    #covRob(data[new.assign==3,])

    #update mu
    #mu = sapply(1:k, function(x){colMeans(data[new.assign==x,])})
    mu = sapply(smat, function(x){x$center})

    #visualize
    model = lapply(1:k, function(x){
      fit.models(list(Robust="covRob", Classical="covClassic"),
                 data=data[new.assign==x,])
    })

    #### Calculate distance metric (with w(weight), mu, smat)
    D = vector("list", k)
    for(ii in 1:k){
      D[[ii]] = w[ii]*dmvnorm(data, mean = mu[,ii], sigma = smat[[ii]]$cov)
    }

    D.nrom = add(D)

    post.p = sapply(D, function(x){x/D.nrom}) #data will be assigned to max post.p

    max.p = apply(post.p, 1, max)
    new.assign = apply(post.p, 1, which.max)

    #trim [n*alpha] observations with smallest max.p
    if(floor(n*alpha)!=0){
      idx.trim = order(max.p)[1:floor(n*alpha)]
      new.assign[idx.trim] = 0
    }
    plot(data, col=factor(new.assign), pch=19, main=paste0("iteration ", nn))

  }
  dev.off()

  ##########
  # Plot results
  ##########
  png(filename = paste0("C://Users//User//Dropbox//2019.Spring//STAT556_1_Robust//05_Project2//image//",
                        simnum, "_", p,"_",q,"_",estim,"_",alpha,".png"), width = 900, height = 900)
  par(mfrow=c(1,3))
  plot(data, col=factor(old_assign), pch=19, main="Initial assignment")
  plot(data, col=factor(new.assign), pch=19, main="At 9th iteration")
  plot(data, col=factor(data2[,p+1]), pch=19, main="Truth")
  dev.off()

  #Misclassification rate
  new.assign2 = new.assign
  true = data2[,p+1]

  plot(data[new.assign==1,], pch=19, main="At 9th iteration", xlim=c(-100,100), ylim=c(-100,100))
  points(data[data2[,p+1]==1,],col="blue", pch=1, main="At 9th iteration", xlim=c(-100,100), ylim=c(-100,100))

  new.assign2[new.assign2==1] = "a"
  true[true==1] = "a"

  plot(data[new.assign==2,], pch=19, main="At 9th iteration", xlim=c(-100,100), ylim=c(-100,100))
  points(data[data2[,p+1]==3,],col="blue", pch=1, main="At 9th iteration", xlim=c(-100,100), ylim=c(-100,100))

  new.assign2[new.assign2==2] = "b"
  true[true==3] = "b"

  plot(data[new.assign==3,], pch=19, main="At 9th iteration", xlim=c(-100,100), ylim=c(-100,100))
  points(data[data2[,p+1]==2,],col="blue", pch=1, main="At 9th iteration", xlim=c(-100,100), ylim=c(-100,100))

  new.assign2[new.assign2==3] = "c"
  true[true==2] = "c"

  if(alpha!=0){
    par(mfrow=c(1,2))
    plot(data[-idx.trim,], col=factor(new.assign2[-idx.trim]), pch=19, main="At 9th iteration")
    plot(data, col=factor(true), pch=19, main="Truth")

    MR = sum(new.assign2[-idx.trim]!=(true[-idx.trim]))/length(true[-idx.trim])

  }else{
    par(mfrow=c(1,2))
    plot(data, col=factor(new.assign2), pch=19, main="At 9th iteration")
    plot(data, col=factor(true), pch=19, main="Truth")

    MR = sum(new.assign2!=(true))/length(true)
  }
  MR
  result = c()
  result = rbind(result,c(simnum, p, q, estim, MR))
  #result2= c()
  result2 = rbind(result2, result)
  result2
}
