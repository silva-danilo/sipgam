
# packs
library(mgcv)
library(readr)
library(robustbase)
library(lubridate)
library(viridis)

# setwd
setwd("/home/posmae/danilo.silva/Rede IME/sipgam")
#setwd("/mnt/chromeos/removable/danvah/sipgam")
#setwd("D:/sipgam")

# load data
dat <- read_csv("data/bike_hour.csv")
sum(is.na(dat))

# add covs
dat$hdemand <- (dat$cnt > 150)*1
dat$yday <- yday(dat$dteday)

# prep factors
dat$yr <- factor(dat$yr)
dat$mnth <- factor(dat$mnth)
dat$holiday <- factor(dat$holiday)
dat$weekday <- factor(dat$weekday)

# prep covs
ids <- dat$atemp > 100
dat$atemp[ids] <- dat$atemp[ids]/1000
ids <- dat$hum > 100
dat$hum[ids] <- dat$hum[ids]/1000
ids <- dat$windspeed > 100
dat$windspeed[ids] <- dat$windspeed[ids]/1000

# save 
write_rds(dat, "data/bike_hour_casa.rds")

# prep plot
par(mar=c(4.5,4.5,1,1), mfrow=c(1,4))
cnt_max <- max(dat$cnt)
cnt_max <- 1.1*cnt_max
dat0 <- dat[dat$yr==0,]
dat1 <- dat[dat$yr==1,]
palet <- viridis(3, direction=-1)[-1]
  
# # plot season
# adjbox(cnt~season, data=dat0, col=adjustcolor(palet[1], alpha=0.5), 
#        names=c("sp", "su", "fa", "wi"),
#        ylab="count rental bikes", xlab="season",
#        cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
# adjbox(cnt~season, data=dat1, col=adjustcolor(palet[1], alpha=0.5), 
#        names=c("sp", "su", "fa", "wi"),
#        ylab="count rental bikes", xlab="season",
#        cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))

# plot mnth
adjbox(cnt~mnth, data=dat0, col=adjustcolor(palet[1], alpha=0.5), 
       names=1:12,
       ylab="count rental bikes", xlab="month",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2011", side=3, line=-2, cex=0.9)
adjbox(cnt~mnth, data=dat1, col=adjustcolor(palet[1], alpha=0.5), 
       names=1:12,
       ylab="count rental bikes", xlab="month",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2012", side=3, line=-2, cex=0.9)

# plot hr
adjbox(cnt~hr, data=dat0, col=adjustcolor(palet[1], alpha=0.5), 
       names=0:23,
       ylab="count rental bikes", xlab="hour",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2011", side=3, line=-2, cex=0.9)
adjbox(cnt~hr, data=dat1, col=adjustcolor(palet[1], alpha=0.5), 
       names=0:23,
       ylab="count rental bikes", xlab="hour",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2012", side=3, line=-2, cex=0.9)

# plot holiday
adjbox(cnt~holiday, data=dat0, col=adjustcolor(palet[1], alpha=0.5), 
       names=c("no", "yes"),
       ylab="count rental bikes", xlab="holiday",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2011", side=3, line=-2, cex=0.9)
adjbox(cnt~holiday, data=dat1, col=adjustcolor(palet[1], alpha=0.5), 
       names=c("no", "yes"),
       ylab="count rental bikes", xlab="holiday",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2012", side=3, line=-2, cex=0.9)

# plot weekday
adjbox(cnt~weekday, data=dat0, col=adjustcolor(palet[1], alpha=0.5), 
       names=c("sun", "mon", "tue", "wed", "thu", "fri", "sat"), 
       ylab="count rental bikes", xlab="weekday",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2011", side=3, line=-2, cex=0.9)
adjbox(cnt~weekday, data=dat1, col=adjustcolor(palet[1], alpha=0.5), 
       names=c("sun", "mon", "tue", "wed", "thu", "fri", "sat"), 
       ylab="count rental bikes", xlab="weekday",
       cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
mtext("year = 2012", side=3, line=-2, cex=0.9)

# # plot workingday
# adjbox(cnt~workingday, data=dat0, col=adjustcolor(palet[1], alpha=0.5), 
#        names=c("no", "yes"), 
#        ylab="count rental bikes", xlab="workingday",
#        cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
# adjbox(cnt~workingday, data=dat1, col=adjustcolor(palet[1], alpha=0.5), 
#        names=c("no", "yes"), 
#        ylab="count rental bikes", xlab="workingday",
#        cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))

# # plot weathersit
# adjbox(cnt~weathersit, data=dat0, col=adjustcolor(palet[1], alpha=0.5), 
#        names=1:4, 
#        ylab="count rental bikes", xlab="weathersit",
#        cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
# adjbox(cnt~weathersit, data=dat1, col=adjustcolor(palet[1], alpha=0.5), 
#        names=1:4, 
#        ylab="count rental bikes", xlab="weathersit",
#        cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))

# plot yday
plot(cnt~yday, data=dat0, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="yday",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat0$yday, dat0$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2011]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])
plot(cnt~yday, data=dat1, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="yday",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat1$yday, dat1$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2012]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])

# plot temp
plot(cnt~temp, data=dat0, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="temperature",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat0$temp, dat0$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2011]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])
plot(cnt~temp, data=dat1, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="temperature",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat1$temp, dat1$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2012]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])

# # plot atemp
# plot(cnt~atemp, data=dat0, col=adjustcolor(palet[1], alpha=0.5),
#      ylab="count rental bikes", xlab="feeling temperature",
#      cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
# lines(lowess(dat0$atemp, dat0$cnt), lwd=2, col=palet[2])
# plot(cnt~atemp, data=dat1, col=adjustcolor(palet[1], alpha=0.5),
#      ylab="count rental bikes", xlab="feeling temperature",
#      cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
# lines(lowess(dat1$atemp, dat1$cnt), lwd=2, col=palet[2])

# plot hum
plot(cnt~hum, data=dat0, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="humidity",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat0$hum, dat0$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2011]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])
plot(cnt~hum, data=dat1, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="humidity",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat1$hum, dat1$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2012]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])

# plot windspeed
plot(cnt~windspeed, data=dat0, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="wind speed",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat0$windspeed, dat0$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2011]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])
plot(cnt~windspeed, data=dat1, col=adjustcolor(palet[1], alpha=0.5),
     ylab="count rental bikes", xlab="wind speed",
     cex.lab=1.7, cex.axis=1.3, pch=19, ylim=c(0,cnt_max))
lines(lowess(dat1$windspeed, dat1$cnt), lwd=2, col=palet[2])
legend("top", legend=c("loess [year = 2012]"), cex=1.3,
       bty="n", horiz=T, y.intersp=0.4, lwd=2, col=palet[2])

# # plot casual+registered
# par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
# plot(cnt~I(casual+registered), data=dat,
#      ylab="count rental bikes", xlab="I(casual+registered)",
#      cex.lab=1.7, cex.axis=1.2, pch=19)

# model
#ids <- dat$instant %in% c(15993, 13341, 13677)
b <- gam(hdemand~s(temp,by=yr)+s(hum,by=yr)+s(windspeed,by=yr)+s(hr)+s(yday)+
         yr+holiday+weekday, family=binomial, data=dat)
summary(b)

# diag
par(mar=c(4.5,5,1,1), mfrow=c(2,2))
gam.check(b, rep=200, cex=0.5, level=0.95,
          cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2)

# terms
par(mar=c(4.5,5,1,1), mfrow=c(1,4))
plot(b, select=1, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-4,2))
plot(b, select=2, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-4,2))
plot(b, select=3, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-3,2))
plot(b, select=4, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-3,2))
plot(b, select=5, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-1.5,0.5))
plot(b, select=6, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-1.5,0.5))
plot(b, select=7, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-21,6))
plot(b, select=8, cex.lab=1.7, cex.axis=1.2, pch=19, lwd=2, ylim=c(-1.5,1))
