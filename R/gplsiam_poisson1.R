
# packs
library(splines)
library(Matrix)
library(mgcv)
library(gamFactory)
library(viridis)

# data gen
data_gen <- function(n, R, seed=13){
  # prep
  set.seed(seed)
  dat <- list()
  dat$X <- cbind(1, matrix(runif(n), nrow=n))
  dat$Z1 <- matrix(runif(2*n), nrow=n)
  dat$Z2 <- matrix(runif(3*n), nrow=n)
  
  # scale
  dat$Z1 <- scale(dat$Z1)
  dat$Z2 <- scale(dat$Z2)
  
  # prep f1
  alpha1 <- c(1, -1.4)
  kk1 <- sqrt(sum(alpha1^2))
  alpha1 <- alpha1/kk1
  dat$u1 <- dat$Z1%*%alpha1
  t1 <- dat$u1/sqrt(12) + sum(0.5*alpha1)
  dat$f1 <- sin(4*t1)
  # dat$f1 <- sin(4*dat$u1)
  dat$f1 <- dat$f1 - mean(dat$f1)
  
  # prep f2
  alpha2 <- c(1, 1.7, -0.8)
  kk2 <- sqrt(sum(alpha2^2))
  alpha2 <- alpha2/kk2
  dat$u2 <- dat$Z2%*%alpha2
  t2 <- dat$u2/sqrt(12) + sum(0.5*alpha2)
  dat$f2 <- sin(4*t2) - cos(4*t2)
  # dat$f2 <- sin(4*dat$u2) - cos(4*dat$u2)
  dat$f2 <- dat$f2 - mean(dat$f2)
  
  # response simu
  beta <- c(2, 0.7)
  dat$mu <- exp(dat$X%*%beta + dat$f1 + dat$f2)
  y_list <- list()
  for(j in 1:R){
    yj <- rpois(nrow(dat$mu), lambda=dat$mu)
    y_list <- c(y_list, list(yj))
  }
  names(y_list) <- paste0("y", 1:R)
  
  # return
  dat <- c(dat, y_list)
  dat
}

# # prep
# dat <- data_gen(200, R=1)
# ids1 <- order(dat$u1)
# ids2 <- order(dat$u2)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$f1[ids1]~dat$u1[ids1], ylab=expression(tilde(f)[1]),
#      xlab=expression("u"[1]), type="l", lwd=2)
# plot(dat$f2[ids2]~dat$u2[ids2], ylab=expression(tilde(f)[2]),
#      xlab=expression("u"[2]), type="l", lwd=2)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$mu~dat$u1, ylab=expression(mu), xlab=expression("u"[1]), lwd=2)
# plot(dat$mu~dat$u2, ylab=expression(mu), xlab=expression("u"[2]), lwd=2)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(y1~u1, ylab="y", xlab=expression("u"[1]), data=dat)
# plot(y1~u2, ylab="y", xlab=expression("u"[2]), data=dat)

# link
gmu <- function(mu) log(mu)

# inverse link
inv_gmu <- function(eta){ 
  # return
  thresh <- -log(.Machine$double.eps)
  eta <- pmin(thresh, pmax(eta, -thresh))
  exp(eta)
}

# variance function
vmu <- function(mu) mu

# weight
wmu <- function(mu){
  # return
  dgmu_dmu <- 1/mu
  dgmu_dmu^(-2)/vmu(mu)
}

# ploglik
ploglik <- function(fit){
  # return
  tmu <- log(fit$mu)
  bmu <- fit$mu
  cy <- sapply(y, function(j) -sum(log(1:j)))
  cy[y==0] <- 0
  ploglik <- sum(fit$phi*(y*tmu - bmu) + cy)
  ploglik <- as.numeric(ploglik - t(fit$psi)%*%fit$P%*%fit$psi)
  ploglik
}

# ralpha_til
ralpha_til <- function(s){
  # prep
  bad <- T
  while(bad==T){
    # simu
    temp_til <- runif(s, -1, 1)
    temp <- c(1, temp_til)
    kk <- sqrt(sum(temp^2))
    temp <- temp/kk

    # check
    max_temp <- max(abs(temp))
    max_temp <- max_temp < 0.8
    pri_temp <- head(temp, 1)
    pri_temp <- pri_temp > 0.2
    if(max_temp & pri_temp) bad <- F
  }

  # return
  alpha_til <- temp_til
  alpha_til
}

# fit start
start_fit <- function(){
  # prep 
  n <- nrow(X)
  p <- ncol(X)
  m <- length(Z)
  qj_vec <- rep(9, m) 
  mj_vec <- qj_vec + 1 + 4 - 8
  fit <- list()
  P_big <- list()
  
  # beta
  beta <- coef(gam(y~X-1, family=poisson("log")))
  M <- X
  psi <- beta
  psi_ini <- 1
  psi_fin <- p
  eta <- X%*%beta
  
  # m-loop
  for(j in 1:m){
    # prep
    Zj <- Z[[j]]
    sj <- ncol(Zj) - 1
    
    # alpha_tilj
    alpha_tilj <- ralpha_til(sj)
    
    # alphaj
    alphaj <- c(1, alpha_tilj)
    kkj <- sqrt(sum(alphaj^2))
    alphaj <- alphaj/kkj
    
    # uj
    uj <- Zj %*% alphaj
    
    # N_tilj
    minj <- min(uj)
    maxj <- max(uj)
    delta <- maxj-minj
    minj <- minj - delta*0.001
    maxj <- maxj + delta*0.001
    h <- (maxj - minj)/(mj_vec[j]+1)
    tj <- seq(minj-3*h, maxj+3*h, length.out=mj_vec[j]+8)
    # minj <- min(uj)
    # maxj <- max(uj)
    # tj <- seq(minj, maxj, length.out=mj_vec[j]+2)
    # tj <- tj[-c(1, mj_vec[j] + 2)]
    # tj <- c(rep(minj, 4), tj, rep(maxj, 4))
    N_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T)
    N_tilj <- scale(N_tilj, scale=F)
    N_tilj <- N_tilj[,-ncol(N_tilj)]
    
    # gamma_tilj
    qj <- ncol(N_tilj)
    gamma_tilj <- coef(gam(y~N_tilj, family=poisson("log")))[-1]
    
    # add N_tilj
    M <- cbind(M, N_tilj)
    psi <- c(psi, gamma_tilj)
    psi_ini <- c(psi_ini, tail(psi_fin,1) + 1)
    psi_fin <- c(psi_fin, tail(psi_fin,1) + qj)
    
    # dN_tilj
    dN_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T, derivs=1)
    dN_tilj <- scale(dN_tilj, scale=F)
    dN_tilj <- dN_tilj[,-ncol(dN_tilj)]
    
    # df_tilj
    df_tilj <- dN_tilj%*%gamma_tilj
    
    # Jj
    Jj <- outer(c(1,alpha_tilj), -alphaj[-1]/kkj^2)
    for (i in 1:sj) Jj[i+1,i] <- Jj[i+1,i] + 1/kkj
    
    # T_tilj
    T_tilj <- as.vector(df_tilj)*Zj%*%Jj
    
    # add T_tilj
    M <- cbind(M, T_tilj)
    psi <- c(psi, alpha_tilj)
    psi_ini <- c(psi_ini, tail(psi_fin,1) + 1)
    psi_fin <- c(psi_fin, tail(psi_fin,1) + sj)
    
    # f_tilj
    f_tilj <- N_tilj%*%gamma_tilj
    
    # eta
    eta <- eta + f_tilj
    
    # P_tilj
    D_tilj <- diff(diag(qj+1), differences=2)
    D_tilj <- D_tilj[,-(qj+1)]
    P_tilj <- Matrix(crossprod(D_tilj))
    P_big <- c(P_big, list(bdiag(P_tilj, diag(sj)*0)))
  }
  
  # save 
  fit$n <- n
  fit$m <- m
  fit$mj_vec <- mj_vec
  fit$psi <- as.matrix(psi)
  fit$psi_pos <- list(ini=psi_ini, fin=psi_fin)
  fit$mu <- inv_gmu(eta)
  fit$M_til <- sqrt(wmu(fit$mu))*M
  
  # lambda + phi
  lambda <- runif(m, 1, 1000)
  phi <- 1

  # save 
  fit$lambda <- lambda
  fit$phi <- phi
  
  # penalization
  P <- 0
  P_big2 <- P_big
  for(j in 1:m){
    # bdiag
    indic <- numeric(m)
    indic[j] <- 1
    P_big2[[j]] <- diag(p)*0
    for(i in 1:m) P_big2[[j]] <- bdiag(P_big2[[j]], P_big[[i]]*indic[i])
    P <- P + P_big2[[j]]*lambda[j]
  }
  
  # save
  fit$P_big2 <- P_big2
  fit$P <- P
  fit$metric <- 10
  fit
}

# fit update
update_fit <- function(fit){
  # prep
  m <- fit$m
  
  # psi
  L <- chol(crossprod(fit$M_til) + fit$P/fit$phi + diag(1e-7, nrow(fit$P)))
  y_til2 <- fit$M_til%*%fit$psi + (y-fit$mu)/sqrt(vmu(fit$mu))
  b <- forwardsolve(t(L), t(fit$M_til)%*%y_til2)
  psi_new <- backsolve(L, b)
  
  # lambda
  B <- forwardsolve(t(L), diag(ncol(L)))
  Q <- MASS::ginv(as.matrix(fit$P)) - crossprod(B)/fit$phi
  P <- 0
  for(j in 1:(fit$m)){
    # lambda + P
    lambdaj <- fit$lambda[j]/(t(psi_new)%*%fit$P_big2[[j]]%*%psi_new)
    fit$lambda[j] <- sum(diag(Q%*%fit$P_big2[[j]]))*lambdaj
    P <- P + fit$P_big2[[j]]*fit$lambda[j]
  }
  
  # phi
  vcov <- crossprod(B)
  edf <- diag(vcov %*% crossprod(fit$M_til))
  
  # re-compute fit
  beta <- psi_new[fit$psi_pos$ini[1]:fit$psi_pos$fin[1]]
  M <- X
  eta <- X%*%beta
  fit$beta <- beta
  
  # m-loop
  for(j in 1:m){
    # prep
    Zj <- Z[[j]]
    
    # gamma_tilj
    gamma_tilj <- psi_new[fit$psi_pos$ini[2*j]:fit$psi_pos$fin[2*j]]
    
    # alpha_tilj
    alpha_tilj <- psi_new[fit$psi_pos$ini[2*j+1]:fit$psi_pos$fin[2*j+1]]
    
    # alphaj
    alphaj <- c(1, alpha_tilj)
    kkj <- sqrt(sum(alphaj^2))
    alphaj <- alphaj/kkj
    
    # uj
    uj <- Zj %*% alphaj
    
    # N_tilj
    minj <- min(uj)
    maxj <- max(uj)
    delta <- maxj-minj
    minj <- minj - delta*0.001
    maxj <- maxj + delta*0.001
    h <- (maxj - minj)/(fit$mj_vec[j]+1)
    tj <- seq(minj-3*h, maxj+3*h, length.out=fit$mj_vec[j]+8)
    # minj <- min(uj)
    # maxj <- max(uj)
    # tj <- seq(minj, maxj, length.out=fit$mj_vec[j]+2)
    # tj <- tj[-c(1, fit$mj_vec[j] + 2)]
    # tj <- c(rep(minj, 4), tj, rep(maxj, 4))
    N_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T)
    N_tilj <- scale(N_tilj, scale=F)
    N_tilj <- N_tilj[,-ncol(N_tilj)]

    # add N_tilj
    M <- cbind(M, N_tilj)

    # dN_tilj
    dN_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T, derivs=1)
    dN_tilj <- scale(dN_tilj, scale=F)
    dN_tilj <- dN_tilj[,-ncol(dN_tilj)]
    
    # df_tilj
    df_tilj <- dN_tilj%*%gamma_tilj
    
    # Jj
    Jj <- outer(c(1,alpha_tilj), -alphaj[-1]/kkj^2)
    for (i in 1:ncol(Jj)) Jj[i+1,i] <- Jj[i+1,i] + 1/kkj
    
    # T_tilj
    T_tilj <- as.vector(df_tilj)*Zj%*%Jj
    
    # add T_tilj
    M <- cbind(M, T_tilj)
    
    # f_tilj
    f_tilj <- N_tilj%*%gamma_tilj
    
    # eta
    eta <- eta + f_tilj
    
    # save
    fit$gamma_til[[j]] <- gamma_tilj
    fit$alpha[[j]] <- alphaj
    fit$alpha_til[[j]] <- alpha_tilj
    fit$f[[j]] <- f_tilj
    fit$u[[j]] <- uj
  }

  # save
  ploglik1 <- ploglik(fit)
  fit$psi <- psi_new
  fit$mu <- inv_gmu(eta)
  fit$M_til <- sqrt(wmu(fit$mu))*M
  fit$P <- P
  fit$B <- B
  fit$edf <- edf
  # fit$vcov <- vcov/fit$phi
  fit$psi_var <- diag(vcov)/fit$phi
  ploglik2 <- ploglik(fit)
  fit$metric <- abs(ploglik2-ploglik1)/(abs(ploglik1) + 1e-4)
  fit
}

# fit
fit_gplsiam <- function(X, Z, y, best_fit=F){
  # initial step
  fit <- start_fit()
  
  # prep
  if(!is.list(best_fit)){
    # create
    fit$tot_ite <- 0
    best_fit <- fit
  }
  
  # prep
  tot_ite <- best_fit$tot_ite
  ite <- 0
  
  # iterative process
  while(fit$metric > 1e-6 & tot_ite < 499){
    # update
    fit <- update_fit(fit)
    
    # bad convergence
    pri_alpha <- min(sapply(fit$alpha, head, n=1)) < 0.05
    min_lambda <- min(fit$lambda) < 0
    big_metric <- fit$metric > 1e+6
    if(ite >= 80 | pri_alpha | min_lambda | big_metric){ 
      # restart
      best_fit$tot_ite <- tot_ite
      return(fit_gplsiam(X, Z, y, best_fit))
    } 
    
    # cat
    tot_ite <- tot_ite + 1
    ite <- ite + 1
    msg <- paste("\r", "Iteration actual:", ite, "- Iteration total:", tot_ite)
    cat(msg, "- The metric:", fit$metric, strrep(" ", 20))
    
    # save best_fit
    if(fit$metric < best_fit$metric){
      # best_fit
      fit$ite <- ite
      best_fit <- fit
    }
  }
  
  # pointwise band
  for(j in 1:best_fit$m){
    # prep
    ini <- best_fit$psi_pos$ini[2*j]
    fin <- best_fit$psi_pos$fin[2*j+1]
    
    # fj_upp + fj_low
    B_fj <- best_fit$B[ini:fin, ini:fin] 
    vcov_fj <- best_fit$M_til[,ini:fin] %*% t(B_fj) 
    vcov_fj <- rowSums(vcov_fj*vcov_fj)/best_fit$phi
    best_fit$f_upp[[j]] <- best_fit$f[[j]] + qnorm(0.975)*sqrt(vcov_fj)
    best_fit$f_low[[j]] <- best_fit$f[[j]] + qnorm(0.025)*sqrt(vcov_fj)
  }
  
  # prep
  best_fit$B <- NULL
  best_fit$M_til <- NULL
  best_fit$P_big2 <- NULL
  best_fit$P <- NULL
  best_fit$alpha_til <- NULL
  
  # return
  best_fit$tot_ite <- tot_ite
  term_name <- paste0("f", 1:best_fit$m)
  names(best_fit$gamma_til) <- term_name
  names(best_fit$alpha) <- term_name
  names(best_fit$f) <- term_name
  names(best_fit$f_upp) <- term_name
  names(best_fit$f_low) <- term_name
  names(best_fit$u) <- term_name
  best_fit
}

# fit with seed
gplsiam <- function(X, Z, y){
  # set
  set.seed(13)
  fit <- fit_gplsiam(X, Z, y, best_fit=F)
  fit
}

# residual function
res_gplsiam <- function(fit, R=1){
  # prep
  n <- fit$n
  mu <- fit$mu
  y <- fit$y
  
  # quantile residual
  a <- ppois(y-1, mu) 
  b <- ppois(y, mu) 
  rq_list <- list()
  for(j in 1:R) rq_list[[j]] <- list(rq=qnorm(runif(n, min=a, max=b)))
  
  # prep
  rq_matrix <- sapply(rq_list, "[[", "rq")
  rq <- c(rq_matrix)
  rq_theor <- qnorm(ppoints(n))
  rq_cente <- apply(rq_matrix, 2, sort) - rq_theor
  
  # prep
  par(mar=c(4.5,5,1,1), mfrow=c(1,3), pch=19, cex.lab=1.8, cex.axis=1.5)
  col <- c("white", turbo(11))
  smoothScatter(rq~rep(mu, R), xlab="fitted value", ylab="quantile residual", 
                colramp=colorRampPalette(col), 
                nbin=200, nrpoints=0, ylim=c(-3,3))
  abline(h=0, lwd=2, lty=2)
  
  # rq vs index
  smoothScatter(rq~rep(1:n, R), xlab="index", ylab="quantile residual", 
                colramp=colorRampPalette(col), 
                nbin=200, nrpoints=0, ylim=c(-3,3))
  abline(h=0, lwd=2, lty=2)
  
  # rq wormplot 
  p <- pnorm(rq_theor)
  se <- (1/dnorm(rq_theor)) * sqrt(p*(1-p)/n)
  conf_upp <- qnorm(0.975)*se
  conf_low <- qnorm(0.025)*se
  smoothScatter(rq_cente~rep(rq_theor, R), ylab="centered quantile residual",
                xlab="theoretical quantile", colramp=colorRampPalette(col), 
                nbin=200, nrpoints=0, ylim=c(-1,1))
  abline(h=0, lwd=2, lty=2)
  lines(conf_upp~rq_theor, lwd=2, lty=2)
  lines(conf_low~rq_theor, lwd=2, lty=2)
}

# # model
# dat <- data_gen(600, R=1)
# X <- dat$X
# Z <- list(Z1=dat$Z1, Z2=dat$Z2)
# y <- dat$y1
# b1 <- gplsiam(X, Z, y)
# 
# # plot
# b1$y <- dat$y1
# res_gplsiam(b1, R=200)
# 
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$u1~b1$u$f1, ylab="u1 true", xlab="u1 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# plot(dat$u2~b1$u$f2, ylab="u2 true", xlab="u2 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(b1$f$f1~b1$u$f1, ylab="f1 fitted", xlab="u1 fitted")
# lines(smooth.spline(y=b1$f_upp$f1, b1$u$f1, lambda=1e-3))
# lines(smooth.spline(y=b1$f_low$f1, b1$u$f1, lambda=1e-3))
# 
# # plot
# plot(b1$f$f2~b1$u$f2, ylab="f2 fitted", xlab="u2 fitted")
# lines(smooth.spline(y=b1$f_upp$f2, b1$u$f2, lambda=1e-3))
# lines(smooth.spline(y=b1$f_low$f2, b1$u$f2, lambda=1e-3))
# 
# # check alpha1
# alpha1 <- c(1, -1.4)
# kk1 <- sqrt(sum(alpha1^2))
# alpha1 <- alpha1/kk1
# alpha1
# b1$alpha$f1
# 
# # check alpha2
# alpha2 <- c(1, 1.7, -0.8)
# kk2 <- sqrt(sum(alpha2^2))
# alpha2 <- alpha2/kk2
# alpha2
# b1$alpha$f2
# 
# # check beta
# c(2, 0.7)
# b1$beta

# si
si <- function(alpha_til, y, X, Z, opt=T, qj=9, fx=F){
  # global iter count
  tot_ite <<- tot_ite + 1
  
  # alpha1 and u1
  s1 <- ncol(Z[[1]])-1
  alpha_til1 <- alpha_til[1:s1]
  alpha1 <- c(1, alpha_til1)
  kk1 <- sqrt(sum(alpha1^2))
  alpha1 <- alpha1/kk1 
  u1 <- Z[[1]]%*%alpha1
  
  # alpha2 and u2
  s2 <- ncol(Z[[2]])-1
  alpha_til2 <- alpha_til[(1:s2)+s1]
  alpha2 <- c(1, alpha_til2)
  kk2 <- sqrt(sum(alpha2^2))
  alpha2 <- alpha2/kk2
  u2 <- Z[[2]]%*%alpha2
  
  # model
  b <- gam(y~X +
             s(u1, fx=fx, k=qj+1, bs="ps", m=c(2,2)) + 
             s(u2, fx=fx, k=qj+1, bs="ps", m=c(2,2)) -
             1, family="poisson", method="ML")
  
  # return
  if(opt) b$gcv.ubre else{
    # alpha1 and J1
    b$alpha[[1]] <- alpha1
    # J1 <- outer(alpha1, -alpha_til1/kk1^2)
    # for(j in 1:length(alpha_til1)) J1[j+1, j] <- J1[j+1, j] + 1/kk1
    # b$J1 <- J1
    
    # alpha2 and J2
    b$alpha[[2]] <- alpha2
    # J2 <- outer(alpha2, -alpha_til2/kk2^2)
    # for(j in 1:length(alpha_til2)) J2[j+1, j] <- J2[j+1, j] + 1/kk2
    # b$J2 <- J2
    b
  }
}

# fit
gplsiam_2step <- function(X, Z, y){
  # tot_ite count
  tot_ite <<- 0
  
  # initial alpha_til
  s1 <- ncol(Z[[1]])-1
  s2 <- ncol(Z[[2]])-1
  alpha_til <- rep(0, s1+s2)
  
  # fit
  f0 <- optim(alpha_til, si, y=y, X=X, Z=Z, fx=T, qj=5)
  f1 <- optim(f0$par, si, y=y, X=X, Z=Z, hessian=T)
  b <- si(f1$par, y, X=X, Z=Z, opt=F)

  # save
  fit <- list()
  fit$m <- length(Z)
  fit$mu <- predict(b, type="response")
  fit$lambda <- b$sp/sapply(b$smooth, "[[", "S.scale")
  fit$phi <- 1/b$sig2 
  fit$tot_ite <- tot_ite
  fit$ite <- b$outer.info$iter
  fit$beta <- b$coefficients[1:ncol(X)]
  for(j in 1:fit$m){
    fit$alpha[[j]] <- b$alpha[[j]]
    fit$f[[j]] <- predict(b, type="terms")[,j+1]
    fit$u[[j]] <- b$model[,j+2]
  }
  fit$edf <- b$edf
  
  # return
  term_name <- paste0("f", 1:fit$m)
  names(fit$alpha) <- term_name
  names(fit$f) <- term_name
  names(fit$u) <- term_name
  fit
}
  
# # model
# dat <- data_gen(600, R=1)
# X <- dat$X
# Z <- list(Z1=dat$Z1, Z2=dat$Z2)
# y <- dat$y1
# b2 <- gplsiam_2step(X, Z, y)
# 
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$u1~b2$u$f1, ylab="u1 true", xlab="u1 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# plot(dat$u2~b2$u$f2, ylab="u2 true", xlab="u2 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(b2$f$f1~b2$u$f1, ylab="f1 fitted", xlab="u1 fitted")
# 
# # plot
# plot(b2$f$f2~b2$u$f2, ylab="f2 fitted", xlab="u2 fitted")
# 
# # check alpha1
# alpha1 <- c(1, -1.4)
# kk1 <- sqrt(sum(alpha1^2))
# alpha1 <- alpha1/kk1
# alpha1
# b2$alpha$f1
# 
# # check alpha2
# alpha2 <- c(1, 1.7, -0.8)
# kk2 <- sqrt(sum(alpha2^2))
# alpha2 <- alpha2/kk2
# alpha2
# b2$alpha$f2
# 
# # check beta
# c(2, 0.7)
# b2$beta

# fit
gplsiam_gamfactory <- function(X, Z, y, qj=9){
  # model
  a1 <- runif(ncol(Z$Z1),-1,1)
  a2 <- runif(ncol(Z$Z2),-1,1)
  b <- gam_nl(y ~ X + 
                s_nest(Z$Z1, trans=trans_linear(alpha=a1), k=qj, m=c(4,2)) +
                s_nest(Z$Z2, trans=trans_linear(alpha=a2), k=qj, m=c(4,2)) -
                1, family=fam_poisson(), method="efs")
  
  # save
  fit <- list()
  fit$m <- length(Z)
  fit$mu <- predict(b, type="response")
  fit$lambda <- b$sp/sapply(b$smooth, "[[", "S.scale")
  fit$phi <- 1/b$sig2 
  fit$tot_ite <- b$outer.info$iter
  fit$ite <- fit$tot_ite
  fit$beta <- b$coefficients[1:ncol(X)]
  md <- 0
  for(j in 1:fit$m){
    fit$alpha[[j]] <- b$smooth[[j]]$xt$si$alpha
    fit$alpha[[j]] <- fit$alpha[[j]]/sqrt(sum(fit$alpha[[j]]^2))
    fit$alpha[[j]] <- fit$alpha[[j]]/sign(fit$alpha[[j]][1])
    fit$f[[j]] <- predict(b, type="terms")[,j+1]
    md_j <- mean(fit$f[[j]])
    md <- md + md_j
    fit$f[[j]] <- fit$f[[j]] - md_j
    fit$u[[j]] <- b$model[,j+2] %*% fit$alpha[[j]]
  }
  fit$beta[1] <- fit$beta[1] + md
  fit$edf <- b$edf
  
  # return
  term_name <- paste0("f", 1:fit$m)
  names(fit$alpha) <- term_name
  names(fit$f) <- term_name
  names(fit$u) <- term_name
  fit
}

# # model
# dat <- data_gen(600, R=1)
# X <- dat$X
# Z <- list(Z1=dat$Z1, Z2=dat$Z2)
# y <- dat$y1
# b3 <- gplsiam_gamfactory(X, Z, y)
# 
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$u1~b3$u$f1, ylab="u1 true", xlab="u1 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# plot(dat$u2~b3$u$f2, ylab="u2 true", xlab="u2 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(b3$f$f1~b3$u$f1, ylab="f1 fitted", xlab="u1 fitted")
# 
# # plot
# plot(b3$f$f2~b3$u$f2, ylab="f2 fitted", xlab="u2 fitted")
# 
# # check alpha1
# alpha1 <- c(1, -1.4)
# kk1 <- sqrt(sum(alpha1^2))
# alpha1 <- alpha1/kk1
# alpha1
# b3$alpha$f1
# 
# # check alpha2
# alpha2 <- c(1, 1.7, -0.8)
# kk2 <- sqrt(sum(alpha2^2))
# alpha2 <- alpha2/kk2
# alpha2
# b3$alpha$f2
# 
# # check beta
# c(2, 0.7)
# b3$beta
