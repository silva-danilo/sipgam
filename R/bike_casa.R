
# packs
library(readr)
library(viridis)
require(pROC)

# setwd
setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
#setwd("/mnt/chromeos/removable/danvah/sipgam")
#setwd("D:/sipgam")

# load gasim
source("R/gasim_berno.R")

# load data
dat <- readRDS("data/bike_hour_casa.rds")
by <- as.numeric(dat$yr)

# model
X <- model.matrix(~holiday+weekday+yr, data=dat) 
U <- list(dat$yday, dat$hr)
Z1 <- model.matrix(~windspeed+hum+temp-1, data=dat)
Z <- list(Z1, Z1)
y <- dat$hdemand
time <- system.time(b <- gasim(X, U, Z, y, by), gcFirst=T)[3]
round(time, 2)

# summary beta
ini <- b$psi_pos$ini[1]
fin <- b$psi_pos$fin[1]
beta <- b$beta 
round(beta, 2)
sd <- sqrt(diag(b$vcov)[ini:fin])
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
sd <- sqrt(diag(b$vcov)[ini:fin])
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
sd <- sqrt(diag(b$vcov)[ini:fin])
round(sd, 2)
w <- abs(alpha_til_2/sd)
round(1 - pnorm(w), 3)

# summary lambda + edf
lambda <- b$lambda
round(lambda, 3)
edf <- sum(b$edf)
round(edf, 3)

# prep
palet <- viridis(2, alpha=1, direction=-1)[-1]
par(mar=c(4.5,5,1,1), mfrow=c(1,2), cex.lab=1.4, cex.axis=1.1, pch=19)

# plot f1
ii <- order(b$u$f1)
plot(b$f$f1[ii]~b$u$f1[ii], 
     ylab=expression(tilde("f")[1]~"fitted"),
     xlab="day of the year", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f1, b$u$f1, df=100)
low <- smooth.spline(y=b$f_low$f1, b$u$f1, df=100)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot f2
ii <- order(b$u$f2)
plot(b$f$f2[ii]~b$u$f2[ii],
     ylab=expression(tilde("f")[2]~"fitted"),
     xlab="hour of the day", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f2, b$u$f2, df=20)
low <- smooth.spline(y=b$f_low$f2, b$u$f2, df=20)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot f3
ii <- order(b$u$f3[by==1])
plot(b$f$f3[by==1][ii]~b$u$f3[by==1][ii],
     ylab=expression(tilde("f")[3]~"fitted"), 
     xlab="environmental index 2011 fitted", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f3[by==1], b$u$f3[by==1], df=100)
low <- smooth.spline(y=b$f_low$f3[by==1], b$u$f3[by==1], df=100)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot f4
ii <- order(b$u$f4[by==2])
plot(b$f$f4[by==2][ii]~b$u$f4[by==2][ii],
     ylab=expression(tilde("f")[4]~"fitted"), 
     xlab="environmental index 2012 fitted", type="l", lwd=2)
upp <- smooth.spline(y=b$f_upp$f4[by==2], b$u$f4[by==2], df=100)
low <- smooth.spline(y=b$f_low$f4[by==2], b$u$f4[by==2], df=100)
polygon(c(upp$x, rev(low$x)), c(upp$y, rev(low$y)),
        col=adjustcolor(palet[1], alpha=0.5), border=NA)

# plot
res_gasim(b, y)

# plot
par(mar=c(4.5,5,1,1), mfrow=c(1,2), cex.lab=1.4, cex.axis=1.1, pch=19)
roc(dat$hdemand, b$mu, xlab="specificity", ylab="sensitivity",
    print.auc=T, plot=T, levels=0:1, direction="<")
optimal_coords <- roc(dat$hdemand, b$mu, levels=0:1, direction="<")
optimal_coords <- coords(optimal_coords, "best", 
                         ret=c("specificity", "sensitivity", "threshold"))
round(optimal_coords, 2)
