
# packs
library(splines)

# prep
n <- 500
set.seed(777)
x1 <- runif(n, 1, 2)
u1 <- runif(n, -5, 5)
f1 <- sin(u1) + (u1/4)^3
f1 <- f1 - mean(f1)

# simu
mu <- 8 - 2*x1 + f1
set.seed(777)
y <- rnorm(n, mu)
dat <- data.frame(y, x1, u1, f1, mu)
rm(x1, u1, f1, mu, n, y)

# plot
par(mar=c(4.5,4.5,1,1), mfrow=c(1,2))
plot(f1~u1, data=dat)
plot(y~u1, data=dat)

# prep
bspline <- function(u, t, i, k=3){ 
  # base
  if(k==0){
    res <- as.numeric(u<t[i+1] & u>=t[i])
  } else{
    # recursion
    d1 <- (t[i+k]-t[i])
    d2 <- (t[i+k+1]-t[i+1])
    w1 <- (u-t[i])/d1
    w2 <- (t[i+k+1]-u)/d2
    ids1 <- d1==0
    ids2 <- d2==0
    w1[ids1] <- 0
    w2[ids2] <- 0
    res <- w1*bspline(u, t, i, k-1) + w2*bspline(u, t, i+1, k-1)
  }
  res
}

# prep
t <- seq(min(dat$u1), max(dat$u1), length.out=10+2)
t <- t[-c(1, 10+2)]
t <- c(rep(min(dat$u1), 4), t, rep(max(dat$u1), 4))
m <- length(t)

# test
bspline(1.4, t, 1:(m-4), 3)
splineDesign(1.4, knots=t, ord=4, outer.ok=T)

# matrix
N1 <- matrix(NA, nrow(dat), m-4)
for(i in 1:(m-4)) N1[,i] <- bspline(dat$u1, t, i, 3)
rm(i)

# matrix
N2 <- splineDesign(dat$u1, knots=t, ord=4, outer.ok=T)

# test
round(N1-N2, 2)
round(colSums(N1-N2), 2)
