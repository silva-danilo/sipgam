
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
  dat$Z3 <- matrix(runif(4*n), nrow=n)
  
  # prep f1
  alpha1 <- c(1, -1.4)
  kk1 <- sqrt(sum(alpha1^2))
  alpha1 <- alpha1/kk1
  dat$u1 <- dat$Z1%*%alpha1
  dat$f1 <- -sin(dat$u1) + (1.8*dat$u1)^3
  dat$f1 <- dat$f1 - mean(dat$f1)
  
  # prep f2
  alpha2 <- c(1, 1.7, -0.8)
  kk2 <- sqrt(sum(alpha2^2))
  alpha2 <- alpha2/kk2
  dat$u2 <- dat$Z2%*%alpha2
  dat$f2 <- -3*(dat$u2)^3 + exp(dat$u2)
  dat$f2 <- dat$f2 - mean(dat$f2)
  
  # prep f3
  alpha3 <- c(1, 3.4, -2.5, -1.6)
  kk3 <- sqrt(sum(alpha3^2))
  alpha3 <- alpha3/kk3
  dat$u3 <- dat$Z3%*%alpha3
  dat$f3 <- (dat$u3)^2/6 - cos(pi*dat$u3)
  dat$f3 <- dat$f3 - mean(dat$f3)
  
  # response simu
  beta <- c(0.6, -1.8)
  eta <- dat$X%*%beta + dat$f1 + dat$f2 + dat$f3
  dat$mu <- 1/(exp(-eta)+1)
  
  y_list <- list()
  for(j in 1:R){
    yj <- rbinom(n, size=1, prob=dat$mu)
    y_list <- c(y_list, list(yj))
  }
  names(y_list) <- paste0("y", 1:R)
  
  # return
  dat <- c(dat, y_list)
  dat
}

# # prep
# dat <- data_gen(600, R=1)
# ids1 <- order(dat$u1)
# ids2 <- order(dat$u2)
# ids3 <- order(dat$u3)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$f1[ids1]~dat$u1[ids1], ylab=expression(tilde(f)[1]),
#       xlab=expression("u"[1]), type="l", lwd=2)
# plot(dat$f2[ids2]~dat$u2[ids2], ylab=expression(tilde(f)[2]),
#       xlab=expression("u"[2]), type="l", lwd=2)
# plot(dat$f3[ids3]~dat$u3[ids3], ylab=expression(tilde(f)[3]),
#      xlab=expression("u"[3]), type="l", lwd=2)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$mu~dat$u1, ylab=expression(mu), xlab=expression("u"[1]), lwd=2)
# plot(dat$mu~dat$u2, ylab=expression(mu), xlab=expression("u"[2]), lwd=2)
# plot(dat$mu~dat$u3, ylab=expression(mu), xlab=expression("u"[3]), lwd=2)
# 
# # plot
# par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(y1~u1, ylab="y", xlab=expression("u"[1]), data=dat)
# plot(y1~u2, ylab="y", xlab=expression("u"[2]), data=dat)
# plot(y1~u3, ylab="y", xlab=expression("u"[3]), data=dat)

# link
gmu <- function(mu) log(mu/(1-mu))

# inverse link
inv_gmu <- function(eta){ 
  # return
  thresh <- -log(.Machine$double.eps)
  eta <- pmin(thresh, pmax(eta, -thresh))
  1/(exp(-eta)+1)
}

# variance function
vmu <- function(mu) mu*(1-mu)

# weight
wmu <- function(mu){
  # return
  dgmu_dmu <- 1/mu + 1/(1-mu)
  dgmu_dmu^(-2)/vmu(mu)
}

# ploglik
ploglik <- function(fit){
  # return
  tmu <- log(fit$mu/(1-fit$mu))
  bmu <- -log(1-fit$mu)
  cy <- 0
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
  m_pb <- length(U)
  qj_vec_pb <- c(12, 12) 
  mj_vec_pb <- qj_vec_pb + 1 + 4 - 8
  m_si <- length(Z)
  qj_vec_si <- c(24, 24)
  mj_vec_si <- qj_vec_si + 1 + 4 - 8
  fit <- list()
  P_big <- list()
  
  # beta
  beta <- coef(gam(y~X-1, family=binomial("logit")))
  M <- X
  psi <- beta
  psi_ini <- 1
  psi_fin <- p
  eta <- X%*%beta
  
  # m_pb-loop
  for(j in 1:m_pb){
    # prep
    uj <- U[[j]]
    
    # N_tilj
    minj <- min(uj)
    maxj <- max(uj)
    delta <- maxj-minj
    minj <- minj - delta*0.001
    maxj <- maxj + delta*0.001
    h <- (maxj - minj)/(mj_vec_pb[j]+1)
    tj <- seq(minj-3*h, maxj+3*h, length.out=mj_vec_pb[j]+8)
    # minj <- min(uj)
    # maxj <- max(uj)
    # tj <- seq(minj, maxj, length.out=mj_vec_pb[j]+2)
    # tj <- tj[-c(1, mj_vec_pb[j] + 2)]
    # tj <- c(rep(minj, 4), tj, rep(maxj, 4))
    N_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T)
    N_tilj <- scale(N_tilj, scale=F)
    N_tilj <- N_tilj[,-ncol(N_tilj)]
    
    # gamma_tilj
    qj <- ncol(N_tilj)
    gamma_tilj <- coef(gam(y~N_tilj, family=binomial("logit")))[-1]
    
    # add N_tilj
    M <- cbind(M, N_tilj)
    psi <- c(psi, gamma_tilj)
    psi_ini <- c(psi_ini, tail(psi_fin,1) + 1)
    psi_fin <- c(psi_fin, tail(psi_fin,1) + qj)
    
    # f_tilj
    f_tilj <- N_tilj%*%gamma_tilj
    
    # eta
    eta <- eta + f_tilj
    
    # P_tilj
    D_tilj <- diff(diag(qj+1), differences=2)
    D_tilj <- D_tilj[,-(qj+1)]
    P_tilj <- Matrix(crossprod(D_tilj))
    P_big <- c(P_big, bdiag(P_tilj))
  }
  
  # m_si-loop
  for(j in 1:m_si){
    # prep
    Zj <- Z[[j]]
    sj <- ncol(Zj) - 1
    Zj <- Zj[by==j,]
    
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
    h <- (maxj - minj)/(mj_vec_si[j]+1)
    tj <- seq(minj-3*h, maxj+3*h, length.out=mj_vec_si[j]+8)
    # minj <- min(uj)
    # maxj <- max(uj)
    # tj <- seq(minj, maxj, length.out=mj_vec_si[j]+2)
    # tj <- tj[-c(1, mj_vec_si[j] + 2)]
    # tj <- c(rep(minj, 4), tj, rep(maxj, 4))
    N_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T)
    N_tilj <- scale(N_tilj, scale=F)
    N_tilj <- N_tilj[,-ncol(N_tilj)]
    
    # gamma_tilj
    qj <- ncol(N_tilj)
    gamma_tilj <- coef(gam(y[by==j]~N_tilj, family=binomial("logit")))[-1]

    # add N_tilj
    A <- matrix(0, n, qj)
    A[by==j,] <- N_tilj
    N_tilj <- A
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
    for(i in 1:sj) Jj[i+1,i] <- Jj[i+1,i] + 1/kkj

    # T_tilj
    T_tilj <- as.vector(df_tilj)*Zj%*%Jj

    # add T_tilj
    A <- matrix(0, n, sj)
    A[by==j,] <- T_tilj
    T_tilj <- A
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
  fit$m_pb <- m_pb
  fit$mj_vec_pb <- mj_vec_pb
  fit$m_si <- m_si
  fit$mj_vec_si <- mj_vec_si
  fit$psi <- as.matrix(psi)
  fit$psi_pos <- list(ini=psi_ini, fin=psi_fin)
  fit$mu <- inv_gmu(eta)
  fit$M_til <- sqrt(wmu(fit$mu))*M
  
  # lambda + phi
  lambda <- runif(m_pb+m_si, 1, 1000)
  phi <- 1

  # save 
  fit$lambda <- lambda
  fit$phi <- phi
  
  # penalization
  m <- m_pb + m_si
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

# fit-update
update_fit <- function(fit){
  # prep 
  m_pb <- fit$m_pb
  m_si <- fit$m_si
  m <- m_pb + m_si
    
  # psi
  L <- chol(crossprod(fit$M_til) + fit$P/fit$phi + diag(1e-7, nrow(fit$P)))
  y_til2 <- fit$M_til%*%fit$psi + (y-fit$mu)/sqrt(vmu(fit$mu))
  b <- forwardsolve(t(L), t(fit$M_til)%*%y_til2)
  psi_new <- backsolve(L, b)
  
  # lambda
  B <- forwardsolve(t(L), diag(ncol(L)))
  Q <- MASS::ginv(as.matrix(fit$P)) - crossprod(B)/fit$phi
  P <- 0
  for(j in 1:m){
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
  
  # m_pb-loop
  for(j in 1:m_pb){
    # prep
    uj <- U[[j]]
    
    # gamma_tilj
    gamma_tilj <- psi_new[fit$psi_pos$ini[j+1]:fit$psi_pos$fin[j+1]]
    
    # N_tilj
    minj <- min(uj)
    maxj <- max(uj)
    delta <- maxj-minj
    minj <- minj - delta*0.001
    maxj <- maxj + delta*0.001
    h <- (maxj - minj)/(fit$mj_vec_pb[j]+1)
    tj <- seq(minj-3*h, maxj+3*h, length.out=fit$mj_vec_pb[j]+8)
    # minj <- min(uj)
    # maxj <- max(uj)
    # tj <- seq(minj, maxj, length.out=fit$mj_vec_pb[j]+2)
    # tj <- tj[-c(1, fit$mj_vec_pb[j] + 2)]
    # tj <- c(rep(minj, 4), tj, rep(maxj, 4))
    N_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T)
    N_tilj <- scale(N_tilj, scale=F)
    N_tilj <- N_tilj[,-ncol(N_tilj)]
    
    # add N_tilj
    M <- cbind(M, N_tilj)
    
    # f_tilj
    f_tilj <- N_tilj%*%gamma_tilj
    
    # eta
    eta <- eta + f_tilj
    
    # save
    fit$gamma_til[[j]] <- gamma_tilj
    fit$f[[j]] <- f_tilj
    fit$u[[j]] <- uj
  }
  
  # m_si-loop
  for(j in 1:m_si){
    # prep
    Zj <- Z[[j]]
    pos <- 2*j+m_pb
    Zj <- Zj[by==j,]
    
    # gamma_tilj
    gamma_tilj <- psi_new[fit$psi_pos$ini[pos]:fit$psi_pos$fin[pos]]

    # alpha_tilj
    alpha_tilj <- psi_new[fit$psi_pos$ini[pos+1]:fit$psi_pos$fin[pos+1]]
    
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
    h <- (maxj - minj)/(fit$mj_vec_si[j]+1)
    tj <- seq(minj-3*h, maxj+3*h, length.out=fit$mj_vec_si[j]+8)
    # minj <- min(uj)
    # maxj <- max(uj)
    # tj <- seq(minj, maxj, length.out=fit$mj_vec_si[j]+2)
    # tj <- tj[-c(1, fit$mj_vec_si[j] + 2)]
    # tj <- c(rep(minj, 4), tj, rep(maxj, 4))
    N_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T)
    N_tilj <- scale(N_tilj, scale=F)
    N_tilj <- N_tilj[,-ncol(N_tilj)]

    # add N_tilj
    A <- matrix(0, fit$n, ncol(N_tilj))
    A[by==j,] <- N_tilj
    N_tilj <- A
    M <- cbind(M, N_tilj)

    # dN_tilj
    dN_tilj <- splineDesign(uj, knots=tj, ord=4, outer.ok=T, derivs=1)
    dN_tilj <- scale(dN_tilj, scale=F)
    dN_tilj <- dN_tilj[,-ncol(dN_tilj)]

    # df_tilj
    df_tilj <- dN_tilj%*%gamma_tilj

    # Jj
    Jj <- outer(c(1,alpha_tilj), -alphaj[-1]/kkj^2)
    for(i in 1:ncol(Jj)) Jj[i+1,i] <- Jj[i+1,i] + 1/kkj

    # T_tilj
    T_tilj <- as.vector(df_tilj)*Zj%*%Jj

    # add T_tilj
    A <- matrix(0, fit$n, ncol(T_tilj))
    A[by==j,] <- T_tilj
    T_tilj <- A
    M <- cbind(M, T_tilj)

    # f_tilj
    f_tilj <- N_tilj%*%gamma_tilj

    # eta
    eta <- eta + f_tilj

    # save
    fit$gamma_til[[j+m_pb]] <- gamma_tilj
    fit$alpha[[j+m_pb]] <- alphaj
    fit$alpha_til[[j+m_pb]] <- alpha_tilj
    fit$f[[j+m_pb]] <- f_tilj
    a <- numeric(fit$n)
    a[by==j] <- uj
    uj <- a
    fit$u[[j+m_pb]] <- uj
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
fit_gplsiam <- function(X, U, Z, y, by, best_fit=F){
  # initial step
  fit <- start_fit()
  m_pb <- fit$m_pb
  m_si <- fit$m_si
  m <- m_pb + m_si
  
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
    pri_alpha <- fit$alpha[!sapply(fit$alpha, is.null)]
    pri_alpha <- min(sapply(pri_alpha, head, n=1)) < 0.05
    min_lambda <- min(fit$lambda) < 0
    big_metric <- fit$metric > 1e+6
    if(ite >= 80 | pri_alpha | min_lambda | big_metric){ 
      # restart
      best_fit$tot_ite <- tot_ite
      return(fit_gplsiam(X, U, Z, y, by, best_fit))
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
  
  # pb pointwise band
  for(j in 1:m_pb){
    # prep
    ini <- best_fit$psi_pos$ini[j+1]
    fin <- best_fit$psi_pos$fin[j+1]
    
    # fj_upp + fj_low
    B_fj <- best_fit$B[ini:fin, ini:fin]
    vcov_fj <- best_fit$M_til[,ini:fin] %*% t(B_fj)
    vcov_fj <- rowSums(vcov_fj*vcov_fj)/best_fit$phi
    best_fit$f_upp[[j]] <- best_fit$f[[j]] + qnorm(0.975)*sqrt(vcov_fj)
    best_fit$f_low[[j]] <- best_fit$f[[j]] + qnorm(0.025)*sqrt(vcov_fj)
  }
  
  # si pointwise band
  for(j in 1:m_si){
    # prep
    ini <- best_fit$psi_pos$ini[2*j+m_pb]
    fin <- best_fit$psi_pos$ini[2*j+m_pb+1]

    # fj_upp + fj_low
    B_fj <- best_fit$B[ini:fin, ini:fin]
    vcov_fj <- best_fit$M_til[,ini:fin] %*% t(B_fj)
    vcov_fj <- rowSums(vcov_fj*vcov_fj)/best_fit$phi
    upp <- qnorm(0.975)*sqrt(vcov_fj)
    low <- qnorm(0.025)*sqrt(vcov_fj)
    best_fit$f_upp[[j+m_pb]] <- best_fit$f[[j+m_pb]] + upp
    best_fit$f_low[[j+m_pb]] <- best_fit$f[[j+m_pb]] + low
  }

  # prep
  best_fit$B <- NULL
  best_fit$M_til <- NULL
  best_fit$P_big2 <- NULL
  best_fit$P <- NULL
  best_fit$alpha_til <- NULL
  
  # return
  best_fit$tot_ite <- tot_ite
  term_name <- paste0("f", 1:m)
  names(best_fit$gamma_til) <- term_name
  names(best_fit$alpha) <- term_name
  names(best_fit$f) <- term_name
  names(best_fit$f_upp) <- term_name
  names(best_fit$f_low) <- term_name
  names(best_fit$u) <- term_name
  best_fit
}

# fit with seed
gplsiam <- function(X, U, Z, y, by){
  # set
  set.seed(13)
  fit <- fit_gplsiam(X, U, Z, y, by, best_fit=F)
  fit
}

# residual function
res_gplsiam <- function(fit, R=1){
  # prep
  n <- fit$n
  mu <- fit$mu
  y <- fit$y
  
  # quantile residual
  a <- pbinom(y-1, 1, mu) 
  b <- pbinom(y, 1, mu)
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
                nbin=200, nrpoints=0, ylim=c(-4.5,4.5))
  abline(h=0, lwd=2, lty=2)
  
  # rq vs index
  smoothScatter(rq~rep(1:n, R), xlab="index", ylab="quantile residual", 
                colramp=colorRampPalette(col), 
                nbin=200, nrpoints=0, ylim=c(-4.5,4.5))
  abline(h=0, lwd=2, lty=2)
  
  # rq wormplot 
  p <- pnorm(rq_theor)
  se <- (1/dnorm(rq_theor)) * sqrt(p*(1-p)/n)
  conf_upp <- qnorm(0.975)*se
  conf_low <- qnorm(0.025)*se
  smoothScatter(rq_cente~rep(rq_theor, R), ylab="centered quantile residual",
                xlab="theoretical quantile", colramp=colorRampPalette(col), 
                nbin=200, nrpoints=0, ylim=c(-0.2,0.2))
  abline(h=0, lwd=2, lty=2)
  lines(conf_upp~rq_theor, lwd=2, lty=2)
  lines(conf_low~rq_theor, lwd=2, lty=2)
}

# # model
# n <- 3000
# dat <- data_gen(n, R=1)
# X <- dat$X
# U <- list(dat$u1, dat$u2)
# Z <- list(dat$Z3, dat$Z3)
# y <- dat$y1
# by <- rbinom(n, 1, 1/2) + 1
# b1 <- gplsiam(X, U, Z, y, by)
# 
# # plot
# b1$y <- dat$y1
# res_gplsiam(b1, R=200)
# 
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(dat$u3[by==1]~b1$u$f3[by==1], ylab="u3 true", xlab="u3 fitted")
# abline(a=0, b=1, lwd=2)
# 
# # plot
# plot(dat$u3[by==2]~b1$u$f4[by==2], ylab="u4 true", xlab="u4 fitted")
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
# # plot
# par(mar=c(4.5,5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
# plot(b1$f$f3[by==1]~b1$u$f3[by==1], ylab="f3 fitted", xlab="u3 fitted")
# lines(smooth.spline(y=b1$f_upp$f3[by==1], b1$u$f3[by==1], lambda=1e-3))
# lines(smooth.spline(y=b1$f_low$f3[by==1], b1$u$f3[by==1], lambda=1e-3))
# 
# # plot
# plot(b1$f$f4[by==2]~b1$u$f4[by==2], ylab="f4 fitted", xlab="u4 fitted")
# lines(smooth.spline(y=b1$f_upp$f4[by==2], b1$u$f4[by==2], lambda=1e-3))
# lines(smooth.spline(y=b1$f_low$f4[by==2], b1$u$f4[by==2], lambda=1e-3))
# 
# # check alpha3 and alpha4
# alpha3 <- c(1, 3.4, -2.5, -1.6)
# kk3 <- sqrt(sum(alpha3^2))
# alpha3 <- alpha3/kk3
# alpha3
# b1$alpha$f3
# b1$alpha$f4
# 
# # check beta
# c(0.6, -1.8)
# b1$beta
