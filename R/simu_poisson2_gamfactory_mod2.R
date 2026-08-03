# packs
library(gamFactory)
library(viridis)

# setwd
# setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
# setwd("/mnt/chromeos/removable/danvah/sipgam")
setwd("D:/sipgam")

# load gasim
source("R/gplsiam_poisson2.R")

# fit
gplsiam_gamfactory <- function(X, Z, y, qj=9){
  # init_alpha
  b <- gam_nl(y ~ X + 
                s_nest(Z$Z1, trans=trans_linear(), k=qj, m=c(4,2)) +
                s_nest(Z$Z2, trans=trans_linear(), k=qj, m=c(4,2)) -
                1, family=fam_poisson(), sp=exp(rep(16, 2)))
  a1 <- b$smooth[[1]]$xt$si$alpha
  a2 <- b$smooth[[2]]$xt$si$alpha
  # a1 <- runif(ncol(Z$Z1),-1,1)
  # a2 <- runif(ncol(Z$Z2),-1,1)
  
  # model
  b <- gam_nl(y ~ X + 
                s_nest(Z$Z1, trans=trans_linear(alpha=a1), k=qj, m=c(4,2)) +
                s_nest(Z$Z2, trans=trans_linear(alpha=a2), k=qj, m=c(4,2)) -
                1, family=fam_poisson())
  
  # save
  fit <- list()
  fit$m <- length(Z)
  fit$mu <- predict(b, type="response")
  fit$lambda <- b$sp/sapply(b$smooth, "[[", "S.scale")
  fit$phi <- 1/b$sig2 
  fit$tot_ite <- b$outer.info$iter
  fit$ite <- fit$tot_ite
  fit$beta <- b$coefficients[1:ncol(X)]
  md <- 0
  for(j in 1:fit$m){
    fit$alpha[[j]] <- b$smooth[[j]]$xt$si$alpha
    fit$alpha[[j]] <- fit$alpha[[j]]/sqrt(sum(fit$alpha[[j]]^2))
    fit$alpha[[j]] <- fit$alpha[[j]]/sign(fit$alpha[[j]][1])
    fit$f[[j]] <- predict(b, type="terms")[,j+1]
    md_j <- mean(fit$f[[j]])
    md <- md + md_j
    fit$f[[j]] <- fit$f[[j]] - md_j
    fit$u[[j]] <- b$model[,j+2] %*% fit$alpha[[j]]
  }
  fit$beta[1] <- fit$beta[1] + md
  fit$edf <- b$edf
  
  # return
  term_name <- paste0("f", 1:fit$m)
  names(fit$alpha) <- term_name
  names(fit$f) <- term_name
  names(fit$u) <- term_name
  fit
}

# # model
# dat <- data_gen(600, R=1)
# X <- dat$X
# Z <- list(Z1=dat$Z1, Z2=dat$Z2)
# y <- dat$y1
# b3 <- gplsiam_gamfactory(X, Z, y)
# 
# # prep
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# 
# # plot
# plot(dat$u1~b3$u$f1, ylab="u1 true", xlab="u1 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# plot(dat$u2~b3$u$f2, ylab="u2 true", xlab="u2 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # prep
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# 
# # plot
# plot(b3$f$f1~b3$u$f1, ylab="f1 fitted", xlab="u1 fitted")
# plot(b3$f$f2~b3$u$f2, ylab="f2 fitted", xlab="u2 fitted")
# 
# # check alpha1
# alpha1 <- c(1, -1.4)
# kk1 <- sqrt(sum(alpha1^2))
# alpha1 <- alpha1/kk1
# alpha1
# b3$alpha$f1
# 
# # check alpha2
# alpha2 <- c(1, -1, 1/2)
# kk2 <- sqrt(sum(alpha2^2))
# alpha2 <- alpha2/kk2
# alpha2
# b3$alpha$f2
# 
# # check beta
# c(2, 0.7)
# b3$beta

# prep
R <- 500

# simu n1
n <- 200
dat_n1 <- data_gen(n, R)
X <- dat_n1$X
Z <- list(Z1=dat_n1$Z1, Z2=dat_n1$Z2)
time_n1 <- numeric(R)
list_n1 <- list()
for(j in 1:R){
  name <- paste0("y", j)
  y <- unlist(dat_n1[name])
  time_n1[j] <- system.time(b <- gplsiam_gamfactory(X, Z, y), gcFirst=T)[3]
  list_n1[[name]] <- b
  cat("\r", "Model actual:", j, strrep(" ", 20))
}

# simu n2
n <- 800
dat_n2 <- data_gen(n, R)
X <- dat_n2$X
Z <- list(Z1=dat_n2$Z1, Z2=dat_n2$Z2)
time_n2 <- numeric(R)
list_n2 <- list()
for(j in 1:R){
  name <- paste0("y", j)
  y <- unlist(dat_n2[name])
  time_n2[j] <- system.time(b <- gplsiam_gamfactory(X, Z, y), gcFirst=T)[3]
  list_n2[[name]] <- b
  cat("\r", "Model actual:", j, strrep(" ", 20))
}

# simu n3
n <- 3200
dat_n3 <- data_gen(n, R)
X <- dat_n3$X
Z <- list(Z1=dat_n3$Z1, Z2=dat_n3$Z2)
time_n3 <- numeric(R)
list_n3 <- list()
for(j in 1:R){
  name <- paste0("y", j)
  y <- unlist(dat_n3[name])
  time_n3[j] <- system.time(b <- gplsiam_gamfactory(X, Z, y), gcFirst=T)[3]
  list_n3[[name]] <- b
  cat("\r", "Model actual:", j, strrep(" ", 20))
}

# # save environment
# save.image(file="data/simu_poisson2_gamfactory_mod2.RData")
# rm(list = ls())

# # load environment
# load("data/simu_poisson2_gamfactory_mod2.RData")

# true coefs f1
alpha1 <- c(1, -1.4)
kk1 <- sqrt(sum(alpha1^2))
alpha1 <- alpha1/kk1

# true coefs f2
alpha2 <- c(1, -1, 1/2)
kk2 <- sqrt(sum(alpha2^2))
alpha2 <- alpha2/kk2

# true coefs fixed
beta <- c(2, 0.7)

# time all
sum(time_n1,time_n2,time_n3)/60
# quantile(c(time_n1,time_n2,time_n3), 0.9)

# prep
palet <- viridis(3, direction=-1)[-1]
name <- names(list_n3)
probs <- seq(0, 1, length.out=100)

# plot f1 with n1
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
rangey <- c(-2.4, 1.2)
rangex <- c(-2.5, 2.5)
ids_f1_n1 <- rep(F, R)
plot(dat_n1$f1~dat_n1$u1, ylab=expression(tilde("f")[1]), 
     xlab=expression("u"[1]), type="n", ylim=rangey, xlim=rangex)
for(j in 1:R){
  # prep
  f1_n1_all <- list_n1[[name[j]]]$f$f1
  u1_n1_all <- list_n1[[name[j]]]$u$f1
  ii <- order(u1_n1_all)
  f1_n1_all <- f1_n1_all[ii]
  u1_n1_all <- u1_n1_all[ii]
  
  # remove
  if(mean(abs(1 - list_n1[[name[j]]]$alpha$f1/alpha1)) > 0.5) ids_f1_n1[j] <- T
  
  # plot
  u1_n1 <- quantile(u1_n1_all, probs, type=1)
  f1_n1 <- f1_n1_all[u1_n1_all %in% u1_n1]
  if(!ids_f1_n1[j]) lines(f1_n1~u1_n1, col=adjustcolor(palet[1], alpha=0.2))
  if(ids_f1_n1[j]) lines(f1_n1~u1_n1, col=2)
}
ii <- order(dat_n1$u1)
lines(dat_n1$f1[ii]~dat_n1$u1[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f1 with n2
ids_f1_n2 <- rep(F, R)
plot(dat_n2$f1~dat_n2$u1, ylab=expression(tilde("f")[1]), 
     xlab=expression("u"[1]), type="n", ylim=rangey, xlim=rangex)
for(j in 1:R){
  # prep
  f1_n2_all <- list_n2[[name[j]]]$f$f1
  u1_n2_all <- list_n2[[name[j]]]$u$f1
  ii <- order(u1_n2_all)
  f1_n2_all <- f1_n2_all[ii]
  u1_n2_all <- u1_n2_all[ii]
  
  # remove
  if(mean(abs(1 - list_n2[[name[j]]]$alpha$f1/alpha1)) > 0.5) ids_f1_n2[j] <- T
  
  # plot
  u1_n2 <- quantile(u1_n2_all, probs, type=1)
  f1_n2 <- f1_n2_all[u1_n2_all %in% u1_n2]
  if(!ids_f1_n2[j]) lines(f1_n2~u1_n2, col=adjustcolor(palet[1], alpha=0.2))
  if(ids_f1_n2[j]) lines(f1_n2~u1_n2, col=2)
}
ii <- order(dat_n2$u1)
lines(dat_n2$f1[ii]~dat_n2$u1[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=800]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f1 with n3
ids_f1_n3 <- rep(F, R)
plot(dat_n3$f1~dat_n3$u1, ylab=expression(tilde("f")[1]), 
     xlab=expression("u"[1]), type="n", ylim=rangey, xlim=rangex)
for(j in 1:R){
  # prep
  f1_n3_all <- list_n3[[name[j]]]$f$f1
  u1_n3_all <- list_n3[[name[j]]]$u$f1
  ii <- order(u1_n3_all)
  f1_n3_all <- f1_n3_all[ii]
  u1_n3_all <- u1_n3_all[ii]
  
  # remove
  if(mean(abs(1 - list_n3[[name[j]]]$alpha$f1/alpha1)) > 0.5) ids_f1_n3[j] <- T
  
  # plot
  u1_n3 <- quantile(u1_n3_all, probs, type=1)
  f1_n3 <- f1_n3_all[u1_n3_all %in% u1_n3]
  if(!ids_f1_n3[j]) lines(f1_n3~u1_n3, col=adjustcolor(palet[1], alpha=0.2))
  if(ids_f1_n3[j]) lines(f1_n3~u1_n3, col=2)
}
ii <- order(dat_n3$u1)
lines(dat_n3$f1[ii]~dat_n3$u1[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f2 with n1
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
rangey <- c(-3.2, 1.5)
rangex <- c(-2.6, 2.7)
ids_f2_n1 <- rep(F, R)
plot(dat_n1$f2~dat_n1$u2, ylab=expression(tilde("f")[2]), 
     xlab=expression("u"[2]), type="n", ylim=rangey, xlim=rangex)
for(j in 1:R){
  # prep
  f2_n1_all <- list_n1[[name[j]]]$f$f2
  u2_n1_all <- list_n1[[name[j]]]$u$f2
  ii <- order(u2_n1_all)
  f2_n1_all <- f2_n1_all[ii]
  u2_n1_all <- u2_n1_all[ii]
  
  # remove
  if(mean(abs(1 - list_n1[[name[j]]]$alpha$f2/alpha2)) > 0.5) ids_f2_n1[j] <- T
  
  # plot
  u2_n1 <- quantile(u2_n1_all, probs, type=1)
  f2_n1 <- f2_n1_all[u2_n1_all %in% u2_n1]
  if(!ids_f2_n1[j]) lines(f2_n1~u2_n1, col=adjustcolor(palet[1], alpha=0.2))
  if(ids_f2_n1[j]) lines(f2_n1~u2_n1, col=2)
}
ii <- order(dat_n1$u2)
lines(dat_n1$f2[ii]~dat_n1$u2[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f2 with n2
ids_f2_n2 <- rep(F, R)
plot(dat_n2$f2~dat_n2$u2, ylab=expression(tilde("f")[2]), 
     xlab=expression("u"[2]), type="n", ylim=rangey, xlim=rangex)
for(j in 1:R){
  # prep
  f2_n2_all <- list_n2[[name[j]]]$f$f2
  u2_n2_all <- list_n2[[name[j]]]$u$f2
  ii <- order(u2_n2_all)
  f2_n2_all <- f2_n2_all[ii]
  u2_n2_all <- u2_n2_all[ii]
  
  # remove
  if(mean(abs(1 - list_n2[[name[j]]]$alpha$f2/alpha2)) > 0.5) ids_f2_n2[j] <- T
  
  # plot
  u2_n2 <- quantile(u2_n2_all, probs, type=1)
  f2_n2 <- f2_n2_all[u2_n2_all %in% u2_n2]
  if(!ids_f2_n2[j]) lines(f2_n2~u2_n2, col=adjustcolor(palet[1], alpha=0.2))
  if(ids_f2_n2[j]) lines(f2_n2~u2_n2, col=2)
}
ii <- order(dat_n2$u2)
lines(dat_n2$f2[ii]~dat_n2$u2[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=800]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f2 with n3
ids_f2_n3 <- rep(F, R)
plot(dat_n3$f2~dat_n3$u2, ylab=expression(tilde("f")[2]), 
     xlab=expression("u"[2]), type="n", ylim=rangey, xlim=rangex)
for(j in 1:R){
  # prep
  f2_n3_all <- list_n3[[name[j]]]$f$f2
  u2_n3_all <- list_n3[[name[j]]]$u$f2
  ii <- order(u2_n3_all)
  f2_n3_all <- f2_n3_all[ii]
  u2_n3_all <- u2_n3_all[ii]
  
  # remove
  if(mean(abs(1 - list_n3[[name[j]]]$alpha$f2/alpha2)) > 0.5) ids_f2_n3[j] <- T
  
  # plot
  u2_n3 <- quantile(u2_n3_all, probs, type=1)
  f2_n3 <- f2_n3_all[u2_n3_all %in% u2_n3]
  if(!ids_f2_n3[j]) lines(f2_n3~u2_n3, col=adjustcolor(palet[1], alpha=0.2))
  if(ids_f2_n3[j]) lines(f2_n3~u2_n3, col=2)
}
ii <- order(dat_n3$u2)
lines(dat_n3$f2[ii]~dat_n3$u2[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# remove
ids_n1 <- (ids_f1_n1 + ids_f2_n1) > 0
ids_n2 <- (ids_f1_n2 + ids_f2_n2) > 0
ids_n3 <- (ids_f1_n3 + ids_f2_n3) > 0
(1:R)[ids_n1]
(1:R)[ids_n2]
(1:R)[ids_n3]
