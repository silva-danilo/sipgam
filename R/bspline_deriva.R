
# packages
library(mgcv)
library(splines)

# prep
n <- 500
set.seed(777)
x1 <- runif(n, 1, 2)
u1 <- runif(n, -5, 5)
f1 <- sin(u1) + (u1/4)^3
f1 <- f1 - mean(f1)
df1 <- cos(u1) + (3/4)*(u1/4)^2
df1 <- df1 - mean(df1)

# simu
mu <- 8 - 2*x1 + f1
set.seed(777)
y <- rnorm(n, mu)
dat <- data.frame(y, x1, u1, f1, df1, mu)
rm(x1, u1, f1, df1, mu, n, y)

# plot
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1~u1, data=dat)
plot(df1~u1, data=dat)

# f mgcv
b1 <- gam(y ~ x1 + s(u1, bs="ps", m=c(2,2)), data=dat)
summary(b1)
ini <- b1$smooth[[1]]$first.para
fin <- b1$smooth[[1]]$last.para
N <- model.matrix(b1)[,ini:fin]
coef <- coef(b1)[ini:fin]
f1_pred <- N%*%coef
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
rm(coef, f1_pred, N, ini, fin)

# df mgcv
b2 <- b1
b2$smooth[[1]]$deriv <- 1
ini <- b1$smooth[[1]]$first.para
fin <- b1$smooth[[1]]$last.para
dN <- model.matrix(b2)[,ini:fin]
coef <- coef(b1)[ini:fin]
df1_pred <- dN%*%coef
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(df1_pred~u1, data=dat)
plot(df1~u1, data=dat)
rm(coef, df1_pred, dN, ini, fin)

# fu lm
t <- seq(min(dat$u1), max(dat$u1), length.out=5+2)
t <- t[-c(1, 5+2)]
t <- c(rep(min(dat$u1), 4), t, rep(max(dat$u1), 4))
m <- length(t)
N <- splineDesign(dat$u1, knots=t, outer.ok=T)
n <- nrow(dat)
N <- N - rep(1, n)%*%t(rep(1, n))%*%N/n
N <- N[,-(m-4)]
b3 <- lm(y ~ x1 + N, data=dat)
summary(b3)
coef <- coef(b3)
f1_pred <- N%*%coef[-c(1:2)]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1_pred~u1, data=dat)
plot(f1~u1, data=dat)
rm(coef, f1_pred, N, m, n, t)

# dfu lm
t <- seq(min(dat$u1), max(dat$u1), length.out=5+2)
t <- t[-c(1, 5+2)]
t <- c(rep(min(dat$u1), 4), t, rep(max(dat$u1), 4))
m <- length(t)
dN <- splineDesign(dat$u1, knots=t, outer.ok=T, derivs=1)
n <- nrow(dat)
dN <- dN - rep(1, n)%*%t(rep(1, n))%*%dN/n
dN <- dN[,-(m-4)]
coef <- coef(b3)
df1_pred <- dN%*%coef[-c(1:2)]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(df1_pred~u1, data=dat)
plot(df1~u1, data=dat)
rm(coef, df1_pred, dN, m, n, t)
