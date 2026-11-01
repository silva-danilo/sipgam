# packs
library(readr)
library(pROC)

# setwd
# setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
# setwd("/mnt/chromeos/removable/danvah/sipgam")
setwd("D:/sipgam")

# load gplsiam
source("R/gplsiam_berno1.R")
          
# load data
dat <- readRDS("data/bike_hour_casa.rds")
by <- as.numeric(dat$yr)

# model
X <- model.matrix(~holiday+weekday+yr, data=dat) 
U <- list(dat$yday, dat$hr)
Z1 <- model.matrix(~hum+windspeed-1, data=dat)
Z <- list(Z1, Z1)
y <- dat$hdemand
time <- system.time(b <- gplsiam(X, U, Z, y, by), gcFirst=T)[3]
time

# summary beta
ini <- b$psi_pos$ini[1]
fin <- b$psi_pos$fin[1]
beta <- b$beta 
round(beta, 2)
sd <- sqrt(b$psi_var[ini:fin])
round(sd, 2)
w <- abs(beta/sd)
round(1 - pnorm(w), 3)

# summary alpha_til_1
ini <- b$psi_pos$ini[5]
fin <- b$psi_pos$fin[5]
alpha_1 <- b$alpha$f3
round(alpha_1, 2)
alpha_til_1 <- alpha_1[-1]/alpha_1[1]
round(alpha_til_1, 2)
sd <- sqrt(b$psi_var[ini:fin])
round(sd, 2)
w <- abs(alpha_til_1/sd)
round(1 - pnorm(w), 3)

# summary alpha_til_2
ini <- b$psi_pos$ini[7]
fin <- b$psi_pos$fin[7]
alpha_2 <- b$alpha$f4
round(alpha_2, 2)
alpha_til_2 <- alpha_2[-1]/alpha_2[1]
round(alpha_til_2, 2)
sd <- sqrt(b$psi_var[ini:fin])
round(sd, 2)
w <- abs(alpha_til_2/sd)
round(1 - pnorm(w), 3)

# summary lambda
lambda <- b$lambda
round(lambda, 3)

# summary edf
edf <- sum(b$edf)
round(edf, 3)
edf_1 <- sum(b$edf[b$psi_pos$ini[2]:b$psi_pos$fin[2]])
round(edf_1, 3)
edf_2 <- sum(b$edf[b$psi_pos$ini[3]:b$psi_pos$fin[3]])
round(edf_2, 3)
edf_3 <- sum(b$edf[b$psi_pos$ini[4]:b$psi_pos$fin[4]])
round(edf_3, 3)
edf_4 <- sum(b$edf[b$psi_pos$ini[6]:b$psi_pos$fin[6]])
round(edf_4, 3)

# prep
palet <- viridis(2, direction=-1)[-1]
par(mar=c(4.5,5,1,1), mfrow=c(1,2), cex.lab=1.3, cex.axis=1.1, pch=19)

# plot f1
ii <- order(b$u$f1)
plot(b$f$f1[ii]~b$u$f1[ii], 
     ylab=expression(tilde("f")[1]~"fitted"),
     xlab="day of the year", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f1, b$u$f1, lambda=1e-4)
low <- smooth.spline(y=b$f_low$f1, b$u$f1, lambda=1e-4)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot f2
ii <- order(b$u$f2)
plot(b$f$f2[ii]~b$u$f2[ii],
     ylab=expression(tilde("f")[2]~"fitted"),
     xlab="hour of the day", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f2, b$u$f2, lambda=1e-4)
low <- smooth.spline(y=b$f_low$f2, b$u$f2, lambda=1e-4)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot f3
ii <- order(b$u$f3[by==1])
plot(b$f$f3[by==1][ii]~b$u$f3[by==1][ii],
     ylab=expression(tilde("f")[3]~"fitted"), 
     xlab="environmental 2011 index fitted", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f3[by==1], b$u$f3[by==1], lambda=1e-4)
low <- smooth.spline(y=b$f_low$f3[by==1], b$u$f3[by==1], lambda=1e-4)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot f4
ii <- order(b$u$f4[by==2])
plot(b$f$f4[by==2][ii]~b$u$f4[by==2][ii],
     ylab=expression(tilde("f")[4]~"fitted"), 
     xlab="environmental index 2012 fitted", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f4[by==2], b$u$f4[by==2], lambda=1e-4)
low <- smooth.spline(y=b$f_low$f4[by==2], b$u$f4[by==2], lambda=1e-4)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot diag
b$y <- dat$hdemand
res_gplsiam(b, R=40)

# summary auc
roc <- roc(dat$hdemand, b$mu, levels=0:1, direction="<")
roc$auc
coords(roc, "best")
