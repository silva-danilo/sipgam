
# packs
library(mgcv)

# data
dat <- mtcars

# model
b <- gam(mpg~s(disp, bs="ps"), data=dat)

# wood knots
b$smooth[[1]]$knots

# copy knots
qj <- 9
mj <- (qj + 1) + 4 - 8
minj <- min(dat$disp)
maxj <- max(dat$disp)
delta <- maxj-minj
minj <- minj - delta*0.001
maxj <- maxj + delta*0.001
h <- (maxj - minj)/(mj+1)
tj <- seq(minj-3*h, maxj+3*h, length.out=mj+8)
tj

# check
tj-b$smooth[[1]]$knots
