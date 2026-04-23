
# packs
library(gamFactory)

# dat
set.seed(13)
n <- 800
p <- 3
alpha <- 1:p/sqrt(sum((1:p)^2))
Z <- matrix(runif(p*n), ncol=p)
Z <- scale(Z)
x1 <- rnorm(n)
mu <- exp(2*sin(Z%*%alpha))
y <- rpois(n, mu)
dat <- data.frame(y=y, x1=x1)
dat$Z <- Z 

# model
fit <- gam_nl(y~s_nest(Z, trans=trans_linear()) + s(x1), data=dat, 
              family=fam_poisson())

# check
fit$smooth[[1]]$xt$si$alpha
alpha