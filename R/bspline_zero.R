
# packs
library(mgcv)
library(splines)
library(splines2)

# prep
n <- 500
set.seed(777)
x1 <- runif(n, 1, 2)
u1 <- runif(n, -5, 5)
f1 <- sin(u1) + (u1/4)^3
f1 <- f1 - mean(f1)
u2 <- runif(n,-1,2)
f2 <- -u2^3 + exp(u2)/2
f2 <- f2 - mean(f2)

# simu
mu <- 8 - 2*x1 + f1 + f2
set.seed(777)
y <- rnorm(n, mu)
dat <- data.frame(y, x1, u1, f1, u2, f2, mu)
rm(x1, u1, f1, u2, f2, mu, n, y)

# plot
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1~u1, data=dat)
plot(f2~u2, data=dat)

# plot
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(y~u1, data=dat)
plot(y~u2, data=dat)

# lm
b1 <- lm(y ~ x1 + u1 + u2, data=dat)
summary(b1)
coef <- coef(b1)
coef <- coef[-c(1:2)]
f1_pred <- dat$u1*coef[1]
f2_pred <- dat$u2*coef[2]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
plot(f2_pred~u2, data=dat)
plot(f2~u2, data=dat)
rm(coef, f1_pred, f2_pred)

# mgcv
b2 <- gam(y ~ x1 + s(u1) + s(u2), data=dat)
summary(b2)
N <- model.matrix(b2)
coef <- coef(b2)
f1_pred <- N[,3:11]%*%coef[3:11]
f2_pred <- N[,12:20]%*%coef[12:20]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
plot(f2_pred~u2, data=dat)
plot(f2~u2, data=dat)
rm(coef, f1_pred, f2_pred, N)

# lm c/ bspline (splines)
t1 <- seq(min(dat$u1), max(dat$u1), length.out=9+2)
t1 <- t1[-c(1, 9+2)]
t2 <- seq(min(dat$u2), max(dat$u2), length.out=9+2)
t2 <- t2[-c(1, 9+2)]
N1 <- splineDesign(dat$u1, knots=t1, outer.ok=T)
N2 <- splineDesign(dat$u2, knots=t2, outer.ok=T)
b3 <- lm(y ~ x1 + N1 + N2, data=dat)
summary(b3)
coef <- coef(b3)
f1_pred <- N1%*%coef[3:7]
f2_pred <- N2%*%coef[8:12]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
plot(f2_pred~u2, data=dat)
plot(f2~u2, data=dat)
rm(coef, f1_pred, f2_pred, N1, N2, t1, t2)

# lm c/ bspline (splines2)
t1 <- seq(min(dat$u1), max(dat$u1), length.out=9+2)
t1 <- t1[-c(1, 9+2)]
t2 <- seq(min(dat$u2), max(dat$u2), length.out=9+2)
t2 <- t2[-c(1, 9+2)]
N1 <- bSpline(dat$u1, knots=t1, degree=3, intercept=T)
N2 <- bSpline(dat$u2, knots=t2, degree=3, intercept=T)
b3 <- lm(y ~ x1 + N1 + N2, data=dat)
summary(b3)
coef <- coef(b3)
coef[is.na(coef)] <- 0
f1_pred <- N1%*%coef[3:15]
f2_pred <- N2%*%coef[16:28]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
plot(f2_pred~u2, data=dat)
plot(f2~u2, data=dat)
rm(coef, f1_pred, f2_pred, N1, N2, t1, t2)

# lm c/ bspline (splines2)
t1 <- seq(min(dat$u1), max(dat$u1), length.out=9+2)
t1 <- t1[-c(1, 9+2)]
t2 <- seq(min(dat$u2), max(dat$u2), length.out=9+2)
t2 <- t2[-c(1, 9+2)]
N1 <- bSpline(dat$u1, knots=t1, degree=3, intercept=F)
N2 <- bSpline(dat$u2, knots=t2, degree=3, intercept=F)
b4 <- lm(y ~ x1 + N1 + N2, data=dat)
summary(b4)
coef <- coef(b4)
f1_pred <- N1%*%coef[3:14]
f2_pred <- N2%*%coef[15:26]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
plot(f2_pred~u2, data=dat)
plot(f2~u2, data=dat)
rm(coef, f1_pred, f2_pred, N1, N2, t1, t2)

# wood rep 
t1 <- seq(min(dat$u1), max(dat$u1), length.out=9+2)
t1 <- t1[-c(1, 9+2)]
t2 <- seq(min(dat$u2), max(dat$u2), length.out=9+2)
t2 <- t2[-c(1, 9+2)]
N1 <- bSpline(dat$u1, knots=t1, degree=3, intercept=T)
N2 <- bSpline(dat$u2, knots=t2, degree=3, intercept=T)
n <- nrow(dat)
N1 <- N1 - rep(1, n)%*%t(rep(1, n))%*%N1/n
N2 <- N2 - rep(1, n)%*%t(rep(1, n))%*%N2/n
N1 <- N1[,-13]
N2 <- N2[,-13]
b5 <- lm(y ~ x1 + N1 + N2, data=dat)
summary(b5)
coef <- coef(b5)
f1_pred <- N1%*%coef[3:14]
f2_pred <- N2%*%coef[15:26]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
plot(f2_pred~u2, data=dat)
plot(f2~u2, data=dat)
rm(coef, f1_pred, f2_pred, N1, N2, t1, t2, n)
