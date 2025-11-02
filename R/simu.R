
# packs
library(viridis)

# setwd
#setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
#setwd("/mnt/chromeos/removable/danvah/sipgam")
setwd("D:/sipgam")

# load gasim
source("R/gasim_gamma.R")

# prep
R <- 500

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
  time_n1[j] <- system.time(b <- gasim(X, Z, y), gcFirst=T)[3]
  list_n1[[name]] <- b
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
  time_n2[j] <- system.time(b <- gasim(X, Z, y), gcFirst=T)[3]
  list_n2[[name]] <- b
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
  time_n3[j] <- system.time(b <- gasim(X, Z, y), gcFirst=T)[3]
  list_n3[[name]] <- b
}

# true-coefs f1
alpha_1 <- c(1, -1.4)
kk_1 <- sqrt(sum(alpha_1^2))
alpha_1 <- alpha_1/kk_1

# true-coefs f2
alpha_2 <- c(1, 1.7, -0.8)
kk_2 <- sqrt(sum(alpha_2^2))
alpha_2 <- alpha_2/kk_2

# true-coefs f3
alpha_3 <- c(1, 3.4, -0.5, -1.6)
kk_3 <- sqrt(sum(alpha_3^2))
alpha_3 <- alpha_3/kk_3

# true-coefs fixed
beta <- c(0.6, -1.8)
phi <- 9

# prep plot
name <- names(list_n3)
palet <- viridis(3, direction=-1)[-1]
par(mar=c(2.6,5.6,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7)

# plot time
boxplot(time_n1, time_n2, time_n3, ylab="time total", 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# time quantile
quantile(c(time_n1,time_n2,time_n3), 0.9)

# plot time/tot_ite
tot_ite_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$tot_ite)
tot_ite_n1 <- unlist(tot_ite_n1)
tot_ite_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$tot_ite)
tot_ite_n2 <- unlist(tot_ite_n2)
tot_ite_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$tot_ite)
tot_ite_n3 <- unlist(tot_ite_n3)
boxplot(time_n1/tot_ite_n1, time_n2/tot_ite_n2, time_n3/tot_ite_n3, 
        ylab="time per iteration", col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"))

# plot edf
edf_n1 <- lapply(1:R, function(j) sum(list_n1[[name[j]]]$edf)) 
edf_n1 <- unlist(edf_n1)
edf_n2 <- lapply(1:R, function(j) sum(list_n2[[name[j]]]$edf))
edf_n2 <- unlist(edf_n2)
edf_n3 <- lapply(1:R, function(j) sum(list_n3[[name[j]]]$edf))
edf_n3 <- unlist(edf_n3)
range_min <- min(edf_n1, edf_n2, edf_n3)
range_max <- max(edf_n1, edf_n2, edf_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(edf_n1, edf_n2, edf_n3,
        ylab=expression(edf ~ "fitted"), col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))

# plot phi
phi_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$phi) 
phi_n1 <- unlist(phi_n1)
phi_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$phi) 
phi_n2 <- unlist(phi_n2)
phi_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$phi) 
phi_n3 <- unlist(phi_n3)
range_min <- min(phi_n1, phi_n2, phi_n3)
range_max <- max(phi_n1, phi_n2, phi_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(phi_n1, phi_n2, phi_n3, ylab=expression(phi ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"), 
        ylim=c(range_min, range_max))
abline(h=phi, lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true param."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])
ids_n1 <- phi_n1 < 5 
ids_n2 <- phi_n2 < 5 
ids_n3 <- phi_n3 < 5
(1:R)[ids_n1]
(1:R)[ids_n2]
(1:R)[ids_n3]
# points(phi_n1[ids_n1]~rep(1, R)[ids_n1], pch=19, col=2)
# points(phi_n2[ids_n2]~rep(2, R)[ids_n2], pch=19, col=2)
# points(phi_n3[ids_n3]~rep(3, R)[ids_n3], pch=19, col=2)
# identify(x=rep(1, R), y=phi_n1, n=1, labels=1:R, cex=0.8)
# identify(x=rep(2, R), y=phi_n2, n=1, labels=1:R, cex=0.8)
# identify(x=rep(3, R), y=phi_n3, n=1, labels=1:R, cex=0.8)

# # plot ite
# ite_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$ite)
# ite_n1 <- unlist(ite_n1)
# ite_n1 <- ite_n1[!ids_n1]
# ite_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$ite)
# ite_n2 <- unlist(ite_n2)
# ite_n2 <- ite_n2[!ids_n2]
# ite_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$ite)
# ite_n3 <- unlist(ite_n3)
# ite_n3 <- ite_n3[!ids_n3]
# boxplot(ite_n1, ite_n2, ite_n3, ylab="model iteration",
#         col=adjustcolor(palet[1],alpha=0.5),
#         names=c("n=200","n=800","n=3200"))
# 
# # plot tot_ite
# tot_ite_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$tot_ite)
# tot_ite_n1 <- unlist(tot_ite_n1)
# tot_ite_n1 <- tot_ite_n1[!ids_n1]
# tot_ite_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$tot_ite)
# tot_ite_n2 <- unlist(tot_ite_n2)
# tot_ite_n2 <- tot_ite_n2[!ids_n2]
# tot_ite_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$tot_ite)
# tot_ite_n3 <- unlist(tot_ite_n3)
# tot_ite_n3 <- tot_ite_n3[!ids_n3]
# boxplot(tot_ite_n1, tot_ite_n2, tot_ite_n3, ylab="model total iteration",
#         col=adjustcolor(palet[1],alpha=0.5),
#         names=c("n=200","n=800","n=3200"))

# plot beta_1
beta_1_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$beta[1]) 
beta_1_n1 <- unlist(beta_1_n1)
beta_1_n1 <- beta_1_n1[!ids_n1]
beta_1_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$beta[1]) 
beta_1_n2 <- unlist(beta_1_n2)
beta_1_n2 <- beta_1_n2[!ids_n2]
beta_1_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$beta[1]) 
beta_1_n3 <- unlist(beta_1_n3)
beta_1_n3 <- beta_1_n3[!ids_n3]
range_min <- min(beta_1_n1, beta_1_n2, beta_1_n3)
range_max <- max(beta_1_n1, beta_1_n2, beta_1_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(beta_1_n1, beta_1_n2, beta_1_n3,
        ylab=expression(beta[1] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=beta[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot beta_2
beta_2_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$beta[2]) 
beta_2_n1 <- unlist(beta_2_n1)
beta_2_n1 <- beta_2_n1[!ids_n1]
beta_2_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$beta[2]) 
beta_2_n2 <- unlist(beta_2_n2)
beta_2_n2 <- beta_2_n2[!ids_n2]
beta_2_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$beta[2]) 
beta_2_n3 <- unlist(beta_2_n3)
beta_2_n3 <- beta_2_n3[!ids_n3]
range_min <- min(beta_2_n1, beta_2_n2, beta_2_n3)
range_max <- max(beta_2_n1, beta_2_n2, beta_2_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(beta_2_n1, beta_2_n2, beta_2_n3, 
        ylab=expression(beta[2] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=beta[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_11
alpha_11_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f1[1]) 
alpha_11_n1 <- unlist(alpha_11_n1)
alpha_11_n1 <- alpha_11_n1[!ids_n1]
alpha_11_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f1[1]) 
alpha_11_n2 <- unlist(alpha_11_n2)
alpha_11_n2 <- alpha_11_n2[!ids_n2]
alpha_11_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f1[1]) 
alpha_11_n3 <- unlist(alpha_11_n3)
alpha_11_n3 <- alpha_11_n3[!ids_n3]
range_min <- min(alpha_11_n1, alpha_11_n2, alpha_11_n3)
range_max <- max(alpha_11_n1, alpha_11_n2, alpha_11_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_11_n1, alpha_11_n2, alpha_11_n3, 
        ylab=expression(alpha[1]^1 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_1[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_12
alpha_12_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f1[2]) 
alpha_12_n1 <- unlist(alpha_12_n1)
alpha_12_n1 <- alpha_12_n1[!ids_n1]
alpha_12_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f1[2]) 
alpha_12_n2 <- unlist(alpha_12_n2)
alpha_12_n2 <- alpha_12_n2[!ids_n2]
alpha_12_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f1[2]) 
alpha_12_n3 <- unlist(alpha_12_n3)
alpha_12_n3 <- alpha_12_n3[!ids_n3]
range_min <- min(alpha_12_n1, alpha_12_n2, alpha_12_n3)
range_max <- max(alpha_12_n1, alpha_12_n2, alpha_12_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_12_n1, alpha_12_n2, alpha_12_n3, 
        ylab=expression(alpha[2]^1 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_1[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_21
alpha_21_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f2[1]) 
alpha_21_n1 <- unlist(alpha_21_n1)
alpha_21_n1 <- alpha_21_n1[!ids_n1]
alpha_21_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f2[1]) 
alpha_21_n2 <- unlist(alpha_21_n2)
alpha_21_n2 <- alpha_21_n2[!ids_n2]
alpha_21_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f2[1]) 
alpha_21_n3 <- unlist(alpha_21_n3)
alpha_21_n3 <- alpha_21_n3[!ids_n3]
range_min <- min(alpha_21_n1, alpha_21_n2, alpha_21_n3)
range_max <- max(alpha_21_n1, alpha_21_n2, alpha_21_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_21_n1, alpha_21_n2, alpha_21_n3,
        ylab=expression(alpha[1]^2 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_2[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_22
alpha_22_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f2[2]) 
alpha_22_n1 <- unlist(alpha_22_n1)
alpha_22_n1 <- alpha_22_n1[!ids_n1]
alpha_22_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f2[2]) 
alpha_22_n2 <- unlist(alpha_22_n2)
alpha_22_n2 <- alpha_22_n2[!ids_n2]
alpha_22_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f2[2]) 
alpha_22_n3 <- unlist(alpha_22_n3)
alpha_22_n3 <- alpha_22_n3[!ids_n3]
range_min <- min(alpha_22_n1, alpha_22_n2, alpha_22_n3)
range_max <- max(alpha_22_n1, alpha_22_n2, alpha_22_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_22_n1, alpha_22_n2, alpha_22_n3, 
        ylab=expression(alpha[2]^2 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_2[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_23
alpha_23_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f2[3]) 
alpha_23_n1 <- unlist(alpha_23_n1)
alpha_23_n1 <- alpha_23_n1[!ids_n1]
alpha_23_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f2[3]) 
alpha_23_n2 <- unlist(alpha_23_n2)
alpha_23_n2 <- alpha_23_n2[!ids_n2]
alpha_23_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f2[3]) 
alpha_23_n3 <- unlist(alpha_23_n3)
alpha_23_n3 <- alpha_23_n3[!ids_n3]
range_min <- min(alpha_23_n1, alpha_23_n2, alpha_23_n3)
range_max <- max(alpha_23_n1, alpha_23_n2, alpha_23_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_23_n1, alpha_23_n2, alpha_23_n3, 
        ylab=expression(alpha[3]^2 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_2[3], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_31
alpha_31_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f3[1]) 
alpha_31_n1 <- unlist(alpha_31_n1)
alpha_31_n1 <- alpha_31_n1[!ids_n1]
alpha_31_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f3[1]) 
alpha_31_n2 <- unlist(alpha_31_n2)
alpha_31_n2 <- alpha_31_n2[!ids_n2]
alpha_31_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f3[1]) 
alpha_31_n3 <- unlist(alpha_31_n3)
alpha_31_n3 <- alpha_31_n3[!ids_n3]
range_min <- min(alpha_31_n1, alpha_31_n2, alpha_31_n3)
range_max <- max(alpha_31_n1, alpha_31_n2, alpha_31_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_31_n1, alpha_31_n2, alpha_31_n3, 
        ylab=expression(alpha[1]^3 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_3[1], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_32
alpha_32_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f3[2]) 
alpha_32_n1 <- unlist(alpha_32_n1)
alpha_32_n1 <- alpha_32_n1[!ids_n1]
alpha_32_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f3[2]) 
alpha_32_n2 <- unlist(alpha_32_n2)
alpha_32_n2 <- alpha_32_n2[!ids_n2]
alpha_32_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f3[2]) 
alpha_32_n3 <- unlist(alpha_32_n3)
alpha_32_n3 <- alpha_32_n3[!ids_n3]
range_min <- min(alpha_32_n1, alpha_32_n2, alpha_32_n3)
range_max <- max(alpha_32_n1, alpha_32_n2, alpha_32_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_32_n1, alpha_32_n2, alpha_32_n3, 
        ylab=expression(alpha[2]^3 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_3[2], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_33
alpha_33_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f3[3]) 
alpha_33_n1 <- unlist(alpha_33_n1)
alpha_33_n1 <- alpha_33_n1[!ids_n1]
alpha_33_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f3[3]) 
alpha_33_n2 <- unlist(alpha_33_n2)
alpha_33_n2 <- alpha_33_n2[!ids_n2]
alpha_33_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f3[3]) 
alpha_33_n3 <- unlist(alpha_33_n3)
alpha_33_n3 <- alpha_33_n3[!ids_n3]
range_min <- min(alpha_33_n1, alpha_33_n2, alpha_33_n3)
range_max <- max(alpha_33_n1, alpha_33_n2, alpha_33_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_33_n1, alpha_33_n2, alpha_33_n3, 
        ylab=expression(alpha[3]^3 ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_3[3], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot alpha_34
alpha_34_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$alpha$f3[4]) 
alpha_34_n1 <- unlist(alpha_34_n1)
alpha_34_n1 <- alpha_34_n1[!ids_n1]
alpha_34_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$alpha$f3[4]) 
alpha_34_n2 <- unlist(alpha_34_n2)
alpha_34_n2 <- alpha_34_n2[!ids_n2]
alpha_34_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$alpha$f3[4]) 
alpha_34_n3 <- unlist(alpha_34_n3)
alpha_34_n3 <- alpha_34_n3[!ids_n3]
range_min <- min(alpha_34_n1, alpha_34_n2, alpha_34_n3)
range_max <- max(alpha_34_n1, alpha_34_n2, alpha_34_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(alpha_34_n1, alpha_34_n2, alpha_34_n3, 
        ylab=expression(alpha[4]^3 ~ "fitted"),
        col=adjustcolor(palet[1],alpha=0.5), 
        names=c("n=200","n=800","n=3200"), ylim=c(range_min, range_max))
abline(h=alpha_3[4], lwd=3, lty=1, col=palet[2])
legend("top", legend=c("true coef."), cex=1.7,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot lambda_1
lambda_1_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$lambda[1]) 
lambda_1_n1 <- unlist(lambda_1_n1)
lambda_1_n1 <- lambda_1_n1[!ids_n1]
lambda_1_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$lambda[1]) 
lambda_1_n2 <- unlist(lambda_1_n2)
lambda_1_n2 <- lambda_1_n2[!ids_n2]
lambda_1_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$lambda[1]) 
lambda_1_n3 <- unlist(lambda_1_n3)
lambda_1_n3 <- lambda_1_n3[!ids_n3]
boxplot(lambda_1_n1, lambda_1_n2, lambda_1_n3, 
        ylab=expression(lambda[1] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# plot lambda_2
lambda_2_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$lambda[2]) 
lambda_2_n1 <- unlist(lambda_2_n1)
lambda_2_n1 <- lambda_2_n1[!ids_n1]
lambda_2_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$lambda[2]) 
lambda_2_n2 <- unlist(lambda_2_n2)
lambda_2_n2 <- lambda_2_n2[!ids_n2]
lambda_2_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$lambda[2]) 
lambda_2_n3 <- unlist(lambda_2_n3)
lambda_2_n3 <- lambda_2_n3[!ids_n3]
boxplot(lambda_2_n1, lambda_2_n2, lambda_2_n3, 
        ylab=expression(lambda[2] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# plot lambda_3
lambda_3_n1 <- lapply(1:R, function(j) list_n1[[name[j]]]$lambda[3]) 
lambda_3_n1 <- unlist(lambda_3_n1)
lambda_3_n1 <- lambda_3_n1[!ids_n1]
lambda_3_n2 <- lapply(1:R, function(j) list_n2[[name[j]]]$lambda[3]) 
lambda_3_n2 <- unlist(lambda_3_n2)
lambda_3_n2 <- lambda_3_n2[!ids_n2]
lambda_3_n3 <- lapply(1:R, function(j) list_n3[[name[j]]]$lambda[3]) 
lambda_3_n3 <- unlist(lambda_3_n3)
lambda_3_n3 <- lambda_3_n3[!ids_n3]
boxplot(lambda_3_n1, lambda_3_n2, lambda_3_n3, 
        ylab=expression(lambda[3] ~ "fitted"), 
        col=adjustcolor(palet[1],alpha=0.5), names=c("n=200","n=800","n=3200"))

# prep
palet <- viridis(3, direction=-1)[-1]
probs <- seq(0, 1, length.out=100)
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)

# plot f1 with n1
range <- c(-2.4, 1)
plot(dat_n1$f1~dat_n1$u1, ylab=expression(tilde("f")[1]), 
     xlab=expression("u"[1]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f_1_n1_all <- list_n1[[name[j]]]$f$f1
  u_1_n1_all <- list_n1[[name[j]]]$u$f1
  ii <- order(u_1_n1_all)
  f_1_n1_all <- f_1_n1_all[ii]
  u_1_n1_all <- u_1_n1_all[ii]
  
  # plot
  u_1_n1 <- quantile(u_1_n1_all, probs, type=1)
  f_1_n1 <- f_1_n1_all[u_1_n1_all %in% u_1_n1]
  if(!ids_n1[j]) lines(f_1_n1~u_1_n1, col=adjustcolor(palet[1], alpha=0.2))
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
  f_1_n2_all <- list_n2[[name[j]]]$f$f1
  u_1_n2_all <- list_n2[[name[j]]]$u$f1
  ii <- order(u_1_n2_all)
  f_1_n2_all <- f_1_n2_all[ii]
  u_1_n2_all <- u_1_n2_all[ii]
  
  # plot
  u_1_n2 <- quantile(u_1_n2_all, probs, type=1)
  f_1_n2 <- f_1_n2_all[u_1_n2_all %in% u_1_n2]
  if(!ids_n2[j]) lines(f_1_n2~u_1_n2, col=adjustcolor(palet[1], alpha=0.2))
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
  f_1_n3_all <- list_n3[[name[j]]]$f$f1
  u_1_n3_all <- list_n3[[name[j]]]$u$f1
  ii <- order(u_1_n3_all)
  f_1_n3_all <- f_1_n3_all[ii]
  u_1_n3_all <- u_1_n3_all[ii]
  
  # plot
  u_1_n3 <- quantile(u_1_n3_all, probs, type=1)
  f_1_n3 <- f_1_n3_all[u_1_n3_all %in% u_1_n3]
  if(!ids_n3[j]) lines(f_1_n3~u_1_n3, col=adjustcolor(palet[1], alpha=0.2))
}
ii <- order(dat_n3$u1)
lines(dat_n3$f1[ii]~dat_n3$u1[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f2 with n1
range <- c(-3, 1)
plot(dat_n1$f2~dat_n1$u2, ylab=expression(tilde("f")[2]), 
     xlab=expression("u"[2]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f_2_n1_all <- list_n1[[name[j]]]$f$f2
  u_2_n1_all <- list_n1[[name[j]]]$u$f2
  ii <- order(u_2_n1_all)
  f_2_n1_all <- f_2_n1_all[ii]
  u_2_n1_all <- u_2_n1_all[ii]
  
  # plot
  u_2_n1 <- quantile(u_2_n1_all, probs, type=1)
  f_2_n1 <- f_2_n1_all[u_2_n1_all %in% u_2_n1]
  if(!ids_n1[j]) lines(f_2_n1~u_2_n1, col=adjustcolor(palet[1], alpha=0.2))
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
  f_2_n2_all <- list_n2[[name[j]]]$f$f2
  u_2_n2_all <- list_n2[[name[j]]]$u$f2
  ii <- order(u_2_n2_all)
  f_2_n2_all <- f_2_n2_all[ii]
  u_2_n2_all <- u_2_n2_all[ii]
  
  # plot
  u_2_n2 <- quantile(u_2_n2_all, probs, type=1)
  f_2_n2 <- f_2_n2_all[u_2_n2_all %in% u_2_n2]
  if(!ids_n2[j]) lines(f_2_n2~u_2_n2, col=adjustcolor(palet[1], alpha=0.2))
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
  f_2_n3_all <- list_n3[[name[j]]]$f$f2
  u_2_n3_all <- list_n3[[name[j]]]$u$f2
  ii <- order(u_2_n3_all)
  f_2_n3_all <- f_2_n3_all[ii]
  u_2_n3_all <- u_2_n3_all[ii]
  
  # plot
  u_2_n3 <- quantile(u_2_n3_all, probs, type=1)
  f_2_n3 <- f_2_n3_all[u_2_n3_all %in% u_2_n3]
  if(!ids_n3[j]) lines(f_2_n3~u_2_n3, col=adjustcolor(palet[1], alpha=0.2))
}
ii <- order(dat_n3$u2)
lines(dat_n3$f2[ii]~dat_n3$u2[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)

# plot f3 with n1
range <- c(-0.8, 2)
plot(dat_n1$f3~dat_n1$u3, ylab=expression(tilde("f")[3]), 
     xlab=expression("u"[3]), type="n", ylim=range)
for(j in 1:R){
  # prep
  f_3_n1_all <- list_n1[[name[j]]]$f$f3
  u_3_n1_all <- list_n1[[name[j]]]$u$f3
  ii <- order(u_3_n1_all)
  f_3_n1_all <- f_3_n1_all[ii]
  u_3_n1_all <- u_3_n1_all[ii]
  
  # plot
  u_3_n1 <- quantile(u_3_n1_all, probs, type=1)
  f_3_n1 <- f_3_n1_all[u_3_n1_all %in% u_3_n1]
  if(!ids_n1[j]) lines(f_3_n1~u_3_n1, col=adjustcolor(palet[1], alpha=0.2))
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
  f_3_n2_all <- list_n2[[name[j]]]$f$f3
  u_3_n2_all <- list_n2[[name[j]]]$u$f3
  ii <- order(u_3_n2_all)
  f_3_n2_all <- f_3_n2_all[ii]
  u_3_n2_all <- u_3_n2_all[ii]
  
  # plot
  u_3_n2 <- quantile(u_3_n2_all, probs, type=1)
  f_3_n2 <- f_3_n2_all[u_3_n2_all %in% u_3_n2]
  if(!ids_n2[j]) lines(f_3_n2~u_3_n2, col=adjustcolor(palet[1], alpha=0.2))
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
  f_3_n3_all <- list_n3[[name[j]]]$f$f3
  u_3_n3_all <- list_n3[[name[j]]]$u$f3
  ii <- order(u_3_n3_all)
  f_3_n3_all <- f_3_n3_all[ii]
  u_3_n3_all <- u_3_n3_all[ii]
  
  # plot
  u_3_n3 <- quantile(u_3_n3_all, probs, type=1)
  f_3_n3 <- f_3_n3_all[u_3_n3_all %in% u_3_n3]
  if(!ids_n3[j]) lines(f_3_n3~u_3_n3, col=adjustcolor(palet[1], alpha=0.2))
}
ii <- order(dat_n3$u3)
lines(dat_n3$f3[ii]~dat_n3$u3[ii], lwd=3, col=palet[2])
legend("top", c("fitted [n=3200]", "true"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="F", bty="n", inset=-0.07)
