
# packs
library(viridis)

# setwd
#setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
#setwd("/mnt/chromeos/removable/danvah/sipgam")
setwd("D:/sipgam")

# load gasim
source("R/gplsiam_gamma.R")

# prep
R <- 100

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
  time_n1[j] <- system.time(b <- gam(y~X+s(u1)+s(u2)+s(u3)-1,
                                     family=Gamma(link="log"), data=dat_n1), 
                            gcFirst=T)[3]
  list_n1[[name]] <- b
  cat("\r", j, strrep(" ", 20))
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
  time_n2[j] <- system.time(b <- gam(y~X+s(u1)+s(u2)+s(u3)-1, 
                                     family=Gamma(link="log"), data=dat_n2),
                            gcFirst=T)[3]
  list_n2[[name]] <- b
  cat("\r", j, strrep(" ", 20))
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
  time_n3[j] <- system.time(b <- gam(y~X+s(u1)+s(u2)+s(u3)-1,
                                     family=Gamma(link="log"), data=dat_n3),
                            gcFirst=T)[3]
  list_n3[[name]] <- b
  cat("\r", j, strrep(" ", 20))
}

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

# time quantile
quantile(c(time_n1,time_n2,time_n3), 0.9)

# plot edf
edf_n1 <- sapply(1:R, function(j) sum(list_n1[[name[j]]]$edf)) 
edf_n2 <- sapply(1:R, function(j) sum(list_n2[[name[j]]]$edf))
edf_n3 <- sapply(1:R, function(j) sum(list_n3[[name[j]]]$edf))
range_min <- min(edf_n1, edf_n2, edf_n3)
range_max <- max(edf_n1, edf_n2, edf_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(edf_n1, edf_n2, edf_n3,
        ylab=expression(edf ~ "fitted"), col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))

# plot phi
phi_n1 <- 1/sapply(1:R, function(j) list_n1[[name[j]]]$sig2) 
phi_n2 <- 1/sapply(1:R, function(j) list_n2[[name[j]]]$sig2) 
phi_n3 <- 1/sapply(1:R, function(j) list_n3[[name[j]]]$sig2) 
range_min <- min(phi_n1, phi_n2, phi_n3)
range_max <- max(phi_n1, phi_n2, phi_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(phi_n1, phi_n2, phi_n3, ylab=expression(phi ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"), 
        ylim=c(range_min, range_max))
abline(h=phi, lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true param."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot lambda1
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
lambda1_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$sp[[1]]) 
lambda1_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$sp[[1]]) 
lambda1_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$sp[[1]]) 
boxplot(lambda1_n1, lambda1_n2, lambda1_n3, 
        ylab=expression(lambda[1] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# plot lambda2
lambda2_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$sp[[2]]) 
lambda2_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$sp[[2]]) 
lambda2_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$sp[[2]]) 
boxplot(lambda2_n1, lambda2_n2, lambda2_n3, 
        ylab=expression(lambda[2] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# plot lambda3
lambda3_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$sp[[3]]) 
lambda3_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$sp[[3]]) 
lambda3_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$sp[[3]]) 
boxplot(lambda3_n1, lambda3_n2, lambda3_n3, 
        ylab=expression(lambda[3] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# plot beta1
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)
beta1_n1 <- sapply(1:R, function(j) list_n1[[name[j]]]$coefficients[1]) 
beta1_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$coefficients[1]) 
beta1_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$coefficients[1]) 
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
beta2_n2 <- sapply(1:R, function(j) list_n2[[name[j]]]$coefficients[2]) 
beta2_n3 <- sapply(1:R, function(j) list_n3[[name[j]]]$coefficients[2]) 
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
