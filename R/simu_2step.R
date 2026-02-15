
# packs
library(viridis)

# setwd
#setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
#setwd("/mnt/chromeos/removable/danvah/sipgam")
setwd("D:/sipgam")

# load gplsiam
source("R/gplsiam_gamma.R")

# prep
R <- 200

# simu (n1=200)
n <- 200
dat_n1 <- data_gen(n, R)
X <- dat_n1$X
Z <- list(dat_n1$Z1, dat_n1$Z2, dat_n1$Z3)
time_n1 <- numeric(R)
list_n1 <- list()
for(j in 1:R){
  name <- paste0("y", j)
  y <- unlist(dat_n1[name])
  time_n1[j] <- system.time(b <- gplsiam_mgcv(X, Z, y), gcFirst=T)[3]
  list_n1[[name]] <- b
  cat("\r", "Model actual:", j, strrep(" ", 20))
}

# simu (n2=800)
n <- 800
dat_n2 <- data_gen(n, R)
X <- dat_n2$X
Z <- list(dat_n2$Z1, dat_n2$Z2, dat_n2$Z3)
time_n2 <- numeric(R)
list_n2 <- list()
for(j in 1:R){
  name <- paste0("y", j)
  y <- unlist(dat_n2[name])
  time_n2[j] <- system.time(b <- gplsiam_mgcv(X, Z, y), gcFirst=T)[3]
  list_n2[[name]] <- b
  cat("\r", "Model actual:", j, strrep(" ", 20))
}

# simu (n3=3200)
n <- 3200
dat_n3 <- data_gen(n, R)
X <- dat_n3$X
Z <- list(dat_n3$Z1, dat_n3$Z2, dat_n3$Z3)
time_n3 <- numeric(R)
list_n3 <- list()
for(j in 1:R){
  name <- paste0("y", j)
  y <- unlist(dat_n3[name])
  time_n3[j] <- system.time(b <- gplsiam_mgcv(X, Z, y), gcFirst=T)[3]
  list_n3[[name]] <- b
  cat("\r", "Model actual:", j, strrep(" ", 20))
}

# save environment
# save.image(file="simu_2step.RData")

# true coefs f1
alpha1 <- c(1, -1.4)
kk1 <- sqrt(sum(alpha1^2))
alpha1 <- alpha1/kk1

# true coefs f2
alpha2 <- c(1, 1.7, -0.8)
kk2 <- sqrt(sum(alpha2^2))
alpha2 <- alpha2/kk2

# true coefs f3
alpha3 <- c(1, 3.4, -0.5, -1.6)
kk3 <- sqrt(sum(alpha3^2))
alpha3 <- alpha3/kk3

# true coefs fixed
beta <- c(0.6, -1.8)
phi <- 9

# prep plot
name <- names(list_n3)
palet <- viridis(3, direction=-1)[-1]

# plot time
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
boxplot(time_n1, time_n2, time_n3, ylab="time total",
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# time
sum(time_n1,time_n2,time_n3)/60
quantile(c(time_n1,time_n2,time_n3), 0.9)

# plot ite
ite_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$ite)
ite_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$ite)
ite_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$ite)
boxplot(ite_n1, ite_n2, ite_n3, ylab="model iteration",
        col=adjustcolor(palet[1],alpha=0.5),
        names=c("n=200","n=800","n=3200"))

# plot edf
edf_n1 <- sapply(1:R, function(j) sum(list_n1[[name[j]]]$edf)) + 6
edf_n2 <- sapply(1:R, function(j) sum(list_n2[[name[j]]]$edf)) + 6
edf_n3 <- sapply(1:R, function(j) sum(list_n3[[name[j]]]$edf)) + 6
range_min <- min(edf_n1, edf_n2, edf_n3)
range_max <- max(edf_n1, edf_n2, edf_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(edf_n1, edf_n2, edf_n3,
        ylab=expression(edf ~ "fitted"), col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))

# remove
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
cut_n1 <- 20.8
cut_n2 <- 24.3 
cut_n3 <- 27 
plot(edf_n1, ylab="edf for n=200")
abline(h=cut_n1, lwd=2, lty=2)
plot(edf_n2, ylab="edf for n=800")
abline(h=cut_n2, lwd=2, lty=2)
plot(edf_n3, ylab="edf for n=3200")
abline(h=cut_n3, lwd=2, lty=2)
ids_n1 <- edf_n1 < cut_n1
ids_n2 <- edf_n2 < cut_n2
ids_n3 <- edf_n3 < cut_n3
(1:R)[ids_n1]
(1:R)[ids_n2]
(1:R)[ids_n3]

# plot phi
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
phi_n1 <- 1/sapply(1:R, function(j) list_n1[[name[j]]]$sig2) 
phi_n1 <- phi_n1[!ids_n1]
phi_n2 <- 1/sapply(1:R, function(j) list_n2[[name[j]]]$sig2) 
phi_n2 <- phi_n2[!ids_n2]
phi_n3 <- 1/sapply(1:R, function(j) list_n3[[name[j]]]$sig2)
phi_n3 <- phi_n3[!ids_n3]
range_min <- min(phi_n1, phi_n2, phi_n3)
range_max <- max(phi_n1, phi_n2, phi_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(phi_n1, phi_n2, phi_n3, ylab=expression(phi ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"), 
        ylim=c(range_min, range_max))
abline(h=phi, lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true param."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot beta1
beta1_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$coefficients[1]) 
beta1_n1 <- beta1_n1[!ids_n1]
beta1_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$coefficients[1]) 
beta1_n2 <- beta1_n2[!ids_n2]
beta1_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$coefficients[1])
beta1_n3 <- beta1_n3[!ids_n3]
range_min <- min(beta1_n1, beta1_n2, beta1_n3)
range_max <- max(beta1_n1, beta1_n2, beta1_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(beta1_n1, beta1_n2, beta1_n3,
        ylab=expression(beta[1] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=beta[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot beta2
beta2_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$coefficients[2])
beta2_n1 <- beta2_n1[!ids_n1]
beta2_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$coefficients[2])
beta2_n2 <- beta2_n2[!ids_n2]
beta2_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$coefficients[2]) 
beta2_n3 <- beta2_n3[!ids_n3]
range_min <- min(beta2_n1, beta2_n2, beta2_n3)
range_max <- max(beta2_n1, beta2_n2, beta2_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(beta2_n1, beta2_n2, beta2_n3, 
        ylab=expression(beta[2] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=beta[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha11
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
alpha11_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha1[1]) 
alpha11_n1 <- alpha11_n1[!ids_n1]
alpha11_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha1[1]) 
alpha11_n2 <- alpha11_n2[!ids_n2]
alpha11_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha1[1]) 
alpha11_n3 <- alpha11_n3[!ids_n3]
range_min <- min(alpha11_n1, alpha11_n2, alpha11_n3)
range_max <- max(alpha11_n1, alpha11_n2, alpha11_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha11_n1, alpha11_n2, alpha11_n3, 
        ylab=expression(alpha[1]^1 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha1[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha12
alpha12_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha1[2]) 
alpha12_n1 <- alpha12_n1[!ids_n1]
alpha12_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha1[2]) 
alpha12_n2 <- alpha12_n2[!ids_n2]
alpha12_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha1[2]) 
alpha12_n3 <- alpha12_n3[!ids_n3]
range_min <- min(alpha12_n1, alpha12_n2, alpha12_n3)
range_max <- max(alpha12_n1, alpha12_n2, alpha12_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha12_n1, alpha12_n2, alpha12_n3, 
        ylab=expression(alpha[2]^1 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha1[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha21
alpha21_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha2[1]) 
alpha21_n1 <- alpha21_n1[!ids_n1]
alpha21_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha2[1]) 
alpha21_n2 <- alpha21_n2[!ids_n2]
alpha21_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha2[1]) 
alpha21_n3 <- alpha21_n3[!ids_n3]
range_min <- min(alpha21_n1, alpha21_n2, alpha21_n3)
range_max <- max(alpha21_n1, alpha21_n2, alpha21_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha21_n1, alpha21_n2, alpha21_n3,
        ylab=expression(alpha[1]^2 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha2[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha22
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
alpha22_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha2[2]) 
alpha22_n1 <- alpha22_n1[!ids_n1]
alpha22_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha2[2]) 
alpha22_n2 <- alpha22_n2[!ids_n2]
alpha22_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha2[2]) 
alpha22_n3 <- alpha22_n3[!ids_n3]
range_min <- min(alpha22_n1, alpha22_n2, alpha22_n3)
range_max <- max(alpha22_n1, alpha22_n2, alpha22_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha22_n1, alpha22_n2, alpha22_n3, 
        ylab=expression(alpha[2]^2 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha2[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha23
alpha23_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha2[3]) 
alpha23_n1 <- alpha23_n1[!ids_n1]
alpha23_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha2[3]) 
alpha23_n2 <- alpha23_n2[!ids_n2]
alpha23_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha2[3]) 
alpha23_n3 <- alpha23_n3[!ids_n3]
range_min <- min(alpha23_n1, alpha23_n2, alpha23_n3)
range_max <- max(alpha23_n1, alpha23_n2, alpha23_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha23_n1, alpha23_n2, alpha23_n3, 
        ylab=expression(alpha[3]^2 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha2[3], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha31
alpha31_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha3[1]) 
alpha31_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha3[1]) 
alpha31_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha3[1]) 
range_min <- min(alpha31_n1, alpha31_n2, alpha31_n3)
range_max <- max(alpha31_n1, alpha31_n2, alpha31_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha31_n1, alpha31_n2, alpha31_n3, 
        ylab=expression(alpha[1]^3 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha3[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha32
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
alpha32_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha3[2]) 
alpha32_n1 <- alpha32_n1[!ids_n1]
alpha32_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha3[2]) 
alpha32_n2 <- alpha32_n2[!ids_n2]
alpha32_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha3[2]) 
alpha32_n3 <- alpha32_n3[!ids_n3]
range_min <- min(alpha32_n1, alpha32_n2, alpha32_n3)
range_max <- max(alpha32_n1, alpha32_n2, alpha32_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha32_n1, alpha32_n2, alpha32_n3, 
        ylab=expression(alpha[2]^3 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha3[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha33
alpha33_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha3[3]) 
alpha33_n1 <- alpha33_n1[!ids_n1]
alpha33_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha3[3]) 
alpha33_n2 <- alpha33_n2[!ids_n2]
alpha33_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha3[3]) 
alpha33_n3 <- alpha33_n3[!ids_n3]
range_min <- min(alpha33_n1, alpha33_n2, alpha33_n3)
range_max <- max(alpha33_n1, alpha33_n2, alpha33_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha33_n1, alpha33_n2, alpha33_n3, 
        ylab=expression(alpha[3]^3 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha3[3], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha34
alpha34_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$alpha3[4]) 
alpha34_n1 <- alpha34_n1[!ids_n1]
alpha34_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$alpha3[4]) 
alpha34_n2 <- alpha34_n2[!ids_n2]
alpha34_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$alpha3[4]) 
alpha34_n3 <- alpha34_n3[!ids_n3]
range_min <- min(alpha34_n1, alpha34_n2, alpha34_n3)
range_max <- max(alpha34_n1, alpha34_n2, alpha34_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha34_n1, alpha34_n2, alpha34_n3, 
        ylab=expression(alpha[4]^3 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha3[4], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot lambda1
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
lambda1_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$sp[[1]]) 
lambda1_n1 <- lambda1_n1[!ids_n1]
lambda1_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$sp[[1]]) 
lambda1_n2 <- lambda1_n2[!ids_n2]
lambda1_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$sp[[1]]) 
lambda1_n3 <- lambda1_n3[!ids_n3]
boxplot(lambda1_n1, lambda1_n2, lambda1_n3, 
        ylab=expression(lambda[1] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# plot lambda2
lambda2_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$sp[[2]]) 
lambda2_n1 <- lambda2_n1[!ids_n1]
lambda2_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$sp[[2]]) 
lambda2_n2 <- lambda2_n2[!ids_n2]
lambda2_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$sp[[2]]) 
lambda2_n3 <- lambda2_n3[!ids_n3]
boxplot(lambda2_n1, lambda2_n2, lambda2_n3, 
        ylab=expression(lambda[2] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# plot lambda3
lambda3_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$sp[[3]]) 
lambda3_n1 <- lambda3_n1[!ids_n1]
lambda3_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$sp[[3]]) 
lambda3_n2 <- lambda3_n2[!ids_n2]
lambda3_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$sp[[3]]) 
lambda3_n3 <- lambda3_n3[!ids_n3]
boxplot(lambda3_n1, lambda3_n2, lambda3_n3, 
        ylab=expression(lambda[3] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# prep
palet <- viridis(3, direction=-1)[-1]
probs <- seq(0, 1, length.out=100)

# plot f1 with n1
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
range <- c(-2.4, 1)
plot(dat_n1$f1~dat_n1$u1, ylab=expression(tilde("f")[1]), 
     xlab=expression("u"[1]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f1_n1_all <- predict(list_n1[[name[j]]], type="terms")[,2]
  u1_n1_all <- list_n1[[name[j]]]$model$u1
  ii <- order(u1_n1_all)
  f1_n1_all <- f1_n1_all[ii]
  u1_n1_all <- u1_n1_all[ii]
  
  # plot
  u1_n1 <- quantile(u1_n1_all, probs, type=1)
  f1_n1 <- f1_n1_all[u1_n1_all %in% u1_n1]
  if(!ids_n1[j]) lines(f1_n1~u1_n1, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n1$u1)
lines(dat_n1$f1[ii]~dat_n1$u1[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f1 with n2
plot(dat_n2$f1~dat_n2$u1, ylab=expression(tilde("f")[1]), 
     xlab=expression("u"[1]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f1_n2_all <- predict(list_n2[[name[j]]], type="terms")[,2]
  u1_n2_all <- list_n2[[name[j]]]$model$u1
  ii <- order(u1_n2_all)
  f1_n2_all <- f1_n2_all[ii]
  u1_n2_all <- u1_n2_all[ii]
  
  # plot
  u1_n2 <- quantile(u1_n2_all, probs, type=1)
  f1_n2 <- f1_n2_all[u1_n2_all %in% u1_n2]
  if(!ids_n2[j]) lines(f1_n2~u1_n2, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n2$u1)
lines(dat_n2$f1[ii]~dat_n2$u1[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=800]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f1 with n3
plot(dat_n3$f1~dat_n3$u1, ylab=expression(tilde("f")[1]), 
     xlab=expression("u"[1]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f1_n3_all <- predict(list_n3[[name[j]]], type="terms")[,2]
  u1_n3_all <- list_n3[[name[j]]]$model$u1
  ii <- order(u1_n3_all)
  f1_n3_all <- f1_n3_all[ii]
  u1_n3_all <- u1_n3_all[ii]
  
  # plot
  u1_n3 <- quantile(u1_n3_all, probs, type=1)
  f1_n3 <- f1_n3_all[u1_n3_all %in% u1_n3]
  if(!ids_n3[j]) lines(f1_n3~u1_n3, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n3$u1)
lines(dat_n3$f1[ii]~dat_n3$u1[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f2 with n1
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
range <- c(-3, 1)
plot(dat_n1$f2~dat_n1$u2, ylab=expression(tilde("f")[2]), 
     xlab=expression("u"[2]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f2_n1_all <- predict(list_n1[[name[j]]], type="terms")[,3]
  u2_n1_all <- list_n1[[name[j]]]$model$u2
  ii <- order(u2_n1_all)
  f2_n1_all <- f2_n1_all[ii]
  u2_n1_all <- u2_n1_all[ii]
  
  # plot
  u2_n1 <- quantile(u2_n1_all, probs, type=1)
  f2_n1 <- f2_n1_all[u2_n1_all %in% u2_n1]
  if(!ids_n1[j]) lines(f2_n1~u2_n1, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n1$u2)
lines(dat_n1$f2[ii]~dat_n1$u2[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f2 with n2
plot(dat_n2$f2~dat_n2$u2, ylab=expression(tilde("f")[2]), 
     xlab=expression("u"[2]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f2_n2_all <- predict(list_n2[[name[j]]], type="terms")[,3]
  u2_n2_all <- list_n2[[name[j]]]$model$u2
  ii <- order(u2_n2_all)
  f2_n2_all <- f2_n2_all[ii]
  u2_n2_all <- u2_n2_all[ii]
  
  # plot
  u2_n2 <- quantile(u2_n2_all, probs, type=1)
  f2_n2 <- f2_n2_all[u2_n2_all %in% u2_n2]
  if(!ids_n2[j]) lines(f2_n2~u2_n2, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n2$u2)
lines(dat_n2$f2[ii]~dat_n2$u2[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=800]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f2 with n3
plot(dat_n3$f2~dat_n3$u2, ylab=expression(tilde("f")[2]), 
     xlab=expression("u"[2]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f2_n3_all <- predict(list_n3[[name[j]]], type="terms")[,3]
  u2_n3_all <- list_n3[[name[j]]]$model$u2
  ii <- order(u2_n3_all)
  f2_n3_all <- f2_n3_all[ii]
  u2_n3_all <- u2_n3_all[ii]
  
  # plot
  u2_n3 <- quantile(u2_n3_all, probs, type=1)
  f2_n3 <- f2_n3_all[u2_n3_all %in% u2_n3]
  if(!ids_n3[j]) lines(f2_n3~u2_n3, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n3$u2)
lines(dat_n3$f2[ii]~dat_n3$u2[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f3 with n1
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
range <- c(-0.8, 2)
plot(dat_n1$f3~dat_n1$u3, ylab=expression(tilde("f")[3]), 
     xlab=expression("u"[3]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f3_n1_all <- predict(list_n1[[name[j]]], type="terms")[,4]
  u3_n1_all <- list_n1[[name[j]]]$model$u3
  ii <- order(u3_n1_all)
  f3_n1_all <- f3_n1_all[ii]
  u3_n1_all <- u3_n1_all[ii]
  
  # plot
  u3_n1 <- quantile(u3_n1_all, probs, type=1)
  f3_n1 <- f3_n1_all[u3_n1_all %in% u3_n1]
  if(!ids_n1[j]) lines(f3_n1~u3_n1, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n1$u3)
lines(dat_n1$f3[ii]~dat_n1$u3[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f3 with n2
plot(dat_n2$f3~dat_n2$u3, ylab=expression(tilde("f")[3]), 
     xlab=expression("u"[3]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f3_n2_all <- predict(list_n2[[name[j]]], type="terms")[,4]
  u3_n2_all <- list_n2[[name[j]]]$model$u3
  ii <- order(u3_n2_all)
  f3_n2_all <- f3_n2_all[ii]
  u3_n2_all <- u3_n2_all[ii]
  
  # plot
  u3_n2 <- quantile(u3_n2_all, probs, type=1)
  f3_n2 <- f3_n2_all[u3_n2_all %in% u3_n2]
  if(!ids_n2[j]) lines(f3_n2~u3_n2, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n2$u3)
lines(dat_n2$f3[ii]~dat_n2$u3[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=800]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f3 with n3
plot(dat_n3$f3~dat_n3$u3, ylab=expression(tilde("f")[3]), 
     xlab=expression("u"[3]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f3_n3_all <- predict(list_n3[[name[j]]], type="terms")[,4]
  u3_n3_all <- list_n3[[name[j]]]$model$u3
  ii <- order(u3_n3_all)
  f3_n3_all <- f3_n3_all[ii]
  u3_n3_all <- u3_n3_all[ii]
  
  # plot
  u3_n3 <- quantile(u3_n3_all, probs, type=1)
  f3_n3 <- f3_n3_all[u3_n3_all %in% u3_n3]
  if(!ids_n3[j]) lines(f3_n3~u3_n3, col=adjustcolor(palet[1], alpha=0.3))
}
ii <- order(dat_n3$u3)
lines(dat_n3$f3[ii]~dat_n3$u3[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)
