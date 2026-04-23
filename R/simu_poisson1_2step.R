
# setwd
# setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
# setwd("/mnt/chromeos/removable/danvah/sipgam")
# setwd("C:/Users/danilo/Desktop/sipgam")
setwd("D:/sipgam")

# load gplsiam
source("R/gplsiam_poisson1.R")

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
  time_n1[j] <- system.time(b <- gplsiam_2step(X, Z, y), gcFirst=T)[3]
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
  time_n2[j] <- system.time(b <- gplsiam_2step(X, Z, y), gcFirst=T)[3]
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
  time_n3[j] <- system.time(b <- gplsiam_2step(X, Z, y), gcFirst=T)[3]
  list_n3[[name]] <- b
  cat("\r", "Model actual:", j, strrep(" ", 20))
}

# # save environment
# save.image(file="data/simu_poisson1_2step.RData")
# rm(list = ls())

# load environment
load("data/simu_poisson1_2step.RData")

# true coefs f1
alpha1 <- c(1, -1.4)
kk1 <- sqrt(sum(alpha1^2))
alpha1 <- alpha1/kk1

# true coefs f2
alpha2 <- c(1, 1.7, -0.8)
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
rangey <- c(-0.9, 2.0)
rangex <- c(-2.4, 2.4)
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
rangey <- c(-3.1, 2.0)
rangex <- c(-2.6, 2.6)
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
