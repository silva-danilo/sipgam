# packs
library(mgcv)

# setwd
setwd("/mnt/chromeos/removable/danvah/sipgam")

# data gen
data_gen <- function(n, R, seed=12345){
  # prep
  set.seed(seed)
  dat <- list()
  dat$Z1 <- matrix(runif(3*n), nrow=n)
  
  # prep f1
  alpha1 <- c(1, -1, 1/2)
  kk1 <- sqrt(sum(alpha1^2))
  alpha1 <- alpha1/kk1
  dat$u1 <- dat$Z1%*%alpha1
  t1 <- (dat$u1 + 0.41)/1.4
  dat$f1 <- 0.2*t1^11 * (10*(1-t1))^6 + 10*(10*t1)^3 * (1-t1)^10
  dat$f1 <- dat$f1/4
  # dat$f1 <- dat$f1 - mean(dat$f1)
  
  # response simu
  dat$mu <- exp(0.25 + dat$f1)
  y_list <- list()
  for(j in 1:R){
    yj <- rpois(nrow(dat$mu), lambda=dat$mu)
    y_list <- c(y_list, list(yj))
  }
  names(y_list) <- paste0("y", 1:R)
  
  # return
  dat <- c(dat, y_list)
  dat
}

# # prep
# dat <- data_gen(600, R=1)
# ids1 <- order(dat$u1)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$f1[ids1]~dat$u1[ids1], ylab=expression(tilde(f)[1]),
#      xlab=expression("u"[1]), type="l", lwd=2)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$mu~dat$u1, ylab=expression(mu), xlab=expression("u"[1]), lwd=2)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(y1~u1, ylab="y", xlab=expression("u"[1]), data=dat)

# si function
si <- function(alpha_til, y, Z, opt=T, qj=9, fx=F){
  # global iter count
  tot_ite <<- tot_ite + 1
  
  # alpha and u
  alpha <- c(1, alpha_til)
  kk <- sqrt(sum(alpha^2))
  alpha <- alpha/kk
  u <- Z%*%alpha
  
  # model
  b <- gam(y~1 + s(u, fx=fx, k=qj+1), family="poisson", method="ML")
  
  # return
  if(opt) b$gcv.ubre else{
    # alpha and J
    b$alpha <- alpha
    J <- outer(alpha, -alpha_til/kk^2)
    for(j in 1:length(alpha_til)) J[j+1, j] <- J[j+1, j] + 1/kk
    b$J <- J
    b
  }
}

# fit
gplsiam_2step <- function(Z, y){
  # tot_ite count
  tot_ite <<- 0
  
  # initial alpha_til
  alpha_til <- c(2, 1)
  
  # fit
  f0 <- optim(alpha_til, si, y=y, Z=Z, fx=T, qj=4)
  f1 <- optim(f0$par, si, y=y, Z=Z, hessian=T)
  b <- si(f1$par, y, Z=Z, opt=F)
  b
}
  
# model
n <- 600
dat <- data_gen(n, R=1)
Z <- dat$Z1
y <- dat$y1
b1 <- gplsiam_2step(Z, y)

# check alpha
alpha <- c(1, -1, 0.5)
kk <- sqrt(sum(alpha^2))
alpha <- alpha/kk
alpha
b1$alpha
