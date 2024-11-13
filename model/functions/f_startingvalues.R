
f_startingvalues <- function(data_mx, data_mz, data_q_flow, options) {
  
  ### Balanced subsample ###
  findbalancedsubsample <- f_findbalancedsubsample(data_mx[options$flag_usestartvals[1:options$Nmx]==TRUE, , drop=FALSE])
  data_bal <- findbalancedsubsample$Y_bal
  balstart <- findbalancedsubsample$index_balanced_start
  balend <- findbalancedsubsample$index_balanced_end
    
  data_bal[is.na(data_bal)] <- 0 # overwrite missing obs in middle of sample with 0!
  
  
  ### Initial PCA estimate of factors ###
  
  PCA <- f_PCA(data_bal, options$Nr) # use balanced dataset to obtain PCA estimates of factors
  factors <- PCA$F_hat
    
  if(options$restrOmeg == 1) {
    # standardize factors
    for(r in 1:options$Nr) {
      factors[r, , drop=FALSE] <- factors[r, , drop=FALSE] / sd(factors[r, , drop=FALSE])
    }
  }
  
  
  ### phi and Omeg ###
  
  y <- t(factors[, (options$Np+1):ncol(factors), drop=FALSE])
  X <- matrix(0,0,0)
 
  for(l in 1:options$Np) {
    X <- mcbind(X, t(factors[, (options$Np+1-l) : (ncol(factors)-l), drop=FALSE]))
  }
  
  A <- t(solve(t(X) %*% X) %*% t(X) %*% y)
  eps <- y - X %*% t(A)
  Omeg <- t(eps) %*% eps / nrow(eps) 
  
  if(options$restrOmeg == 1) {
    Omeg <- diag(1, options$Nr)
  }
  
  
  ### lambdaMx, rhoMx, and SigMx ###
  
  lambdaMx <- matrix(NA, nrow(data_mx), options$Nr)
  rhoMx <- matrix(NA, nrow(data_mx), options$Nj)
  SigMx <- matrix(NA, nrow(data_mx), 1)
  
  for (i in 1:nrow(data_mx)) {
    
    # lambda_m
    Y <- t(data_mx[i, balstart:balend, drop=FALSE]) 
    X <- t(factors) 
      # make sure no missings left in Y
    Yestim <- Y[!is.na(Y[, 1]), 1, drop=FALSE]
    Xestim <- X[!is.na(Y[, 1]), , drop=FALSE]
    lambdaMx[i, ] <- t(solve(t(Xestim) %*% Xestim, t(Xestim)) %*% Yestim)
    idios <- Yestim - Xestim %*% t(lambdaMx[i, , drop=FALSE])
    
    if (options$Nj > 0) {
      y <- idios[(options$Nj+1):nrow(idios), , drop=FALSE]
      X <- matrix(0,0,0)
      for (j in 1:options$Nj) {
        X <- mcbind(X, idios[(options$Nj+1-j):(nrow(idios)-j), , drop=FALSE]) 
      }
      
      rhoMx[i, ] <- solve(t(X) %*% X) %*% t(X) %*% y
      resids <- y - X %*% t(rhoMx[i, , drop=FALSE]) 
      SigMx[i, 1] <- var(resids)
      
    } else {
      SigMx[i, 1] <- t(idios) %*% idios / nrow(idios) 
      rhoMx <- matrix(0,0,0)
    }
  }
  
  
  ### lambdaMz, rhoMz and SigMz ###
  
  lambdaMz <- matrix(NA, nrow(data_mz), options$Nr)
  rhoMz <- matrix(NA, nrow(data_mz), options$Nj) 
  SigMz <- matrix(NA, nrow(data_mz), 1)

  if (is_empty(data_mz)) {
    lambdaMz <- matrix(0, 0, 0)
    rhoMz <- matrix(0, 0, 0) 
    SigMz <- matrix(0, 0, 0)
    
  } else {
    for (i in 1:nrow(data_mz)) {
      Y <- t(data_mz[i, balstart:balend, drop=FALSE]) 
      X <- t(factors) 
        # make sure no missings left in Y
      Yestim <- Y[!is.na(Y[, 1]), 1, drop=FALSE]
      Xestim <- X[!is.na(Y[, 1]), , drop=FALSE] 
      lambdaMz[i, ] <- t(solve(t(Xestim) %*% Xestim, t(Xestim)) %*% Yestim)
      idios <- Yestim - Xestim %*% t(lambdaMz[i, , drop=FALSE])

      if (options$Nj > 0) {
        y <- idios[(options$Nj+1):nrow(idios), , drop=FALSE]
        X <- matrix(0,0,0)
        for (j in 1:options$Nj) {
          X <- mcbind(X, idios[(options$Nj+1-j):(nrow(idios)-j), , drop=FALSE])
        }
        
        rhoMz[i, ] <- solve(t(X) %*% X) %*% t(X) %*% y
        resids <- y - X %*% t(rhoMz[i, , drop=FALSE])
        SigMz[i, 1] <- var(resids)
        
      } else {
        SigMz[i, 1] <- t(idios) %*% idios / nrow(idios) 
        rhoMz <- matrix(0,0,0)
      }
    }
  }
  
  
  ### lambdaQ_flow, rhoQ_flow and SigQ_flow ###
  
  if (options$Nq_flow > 0) {
    
    lambdaQ_flow <- matrix(NA, nrow(data_q_flow), options$Nr)
    rhoQ_flow <- matrix(NA, nrow(data_q_flow), options$Nj) 
    SigQ_flow <- matrix(NA, nrow(data_q_flow), 1)
    data_q_flow_bal <- data_q_flow[, balstart:balend, drop=FALSE] 
    X <- cbind(matrix(NA, nrow(factors), 2), 
               (factors[, 1:(ncol(factors)-2), drop=FALSE] + factors[, 2:(ncol(factors)-1), drop=FALSE] + factors[, 3:ncol(factors), drop=FALSE]) / 3)
    
    for (i in 1:nrow(data_q_flow)) {
      Yestim <- t(data_q_flow_bal[i, !is.na(data_q_flow_bal[i, ]) & !apply(is.na(X), 2, any), drop=FALSE])
      Xestim <- t(X[, !is.na(data_q_flow_bal[i, ]) & !apply(is.na(X), 2, any), drop=FALSE])
      lambdaQ_flow[i,] <- t(solve(t(Xestim) %*% Xestim, t(Xestim)) %*% Yestim)
      idios <- Yestim - Xestim %*% t(lambdaQ_flow[i, , drop=FALSE])
      
      if (options$Nj > 0) {
        y <- idios[(options$Nj+1):nrow(idios), , drop=FALSE]
        X <- matrix(0,0,0)
        
        for (j in 1:options$Nj){
          X <- mcbind(X, idios[(options$Nj+1-j):(nrow(idios)-j), , drop=FALSE]) 
        }
        
        rhoQ_flow[i, ] <- solve(t(X) %*% X) %*% t(X) %*% y
        resids <- y - X %*% t(rhoQ_flow[i, , drop=FALSE])
        SigQ_flow[i, 1] <- var(resids)
        
      } else {
        SigQ_flow[i,1] <- (3 / sqrt(19))^2 * t(idios) %*% idios / nrow(idios)
        rhoQ_flow <- matrix(0, 0, 0)
      }
    }

  } else {
    lambdaQ_flow <- matrix(0, 0, 0) 
    rhoQ_flow <- matrix(0, 0, 0) 
    SigQ_flow <- matrix(0, 0, 0)
  }
  
  # Collect parameters in structure
  params <- list()
  params$A <- A 
  params$Omeg <- Omeg 
  params$lambdaMx <- lambdaMx
  params$rhoMx <- rhoMx
  params$SigMx <- SigMx 
  params$lambdaMz <- lambdaMz 
  params$rhoMz <- rhoMz
  params$SigMz <- SigMz 
  params$lambdaQ_flow <- lambdaQ_flow
  params$rhoQ_flow <- rhoQ_flow
  params$SigQ_flow <- SigQ_flow 
  params$lambdaQ_stock <- matrix(0,0,0) 
  params$rhoQ_stock <- matrix(0,0,0)  
  params$SigQ_stock <- matrix(0,0,0)
  
  return(params)
}

f_findbalancedsubsample <- function(Y) {
  
  # Summary of this function: to do
  #   Detailed explanation: to do
  T = ncol(Y)
  
  # start of balanced subsample
  for (t in 1:T) {
    if (any(is.na(Y[, t]))) {
      next
    } else {
      break
    }
  }
  index_balanced_start <- t
  
  # end of balanced subsample
  for (t in rev(T):1) {
    if (any(is.na(Y[, t]))) {
      next
    } else {
      break
    }
  }
  index_balanced_end <- t
  
  Y_bal <- Y[, index_balanced_start:index_balanced_end, drop=FALSE]
  
  return(list(Y_bal = Y_bal, index_balanced_start = index_balanced_start, index_balanced_end = index_balanced_end))
}

f_PCA <- function(Y, R) {
  
  # covariance matrix of observables
  SIGMA <- Y %*% t(Y) / ncol(Y)
  
  # eigenvalue and -vector decomposition
  eig_vv <- eigen(SIGMA, symmetric=TRUE) 
  V <- eig_vv$vectors*(-1)
  eigenvalues_D <- eig_vv$values
  D <- diag(eigenvalues_D)
  
  F_hat <- t(V) %*% Y
  F_hat <- F_hat[1:R, , drop=FALSE]
  V <- t(V)
  
  return(list(F_hat = F_hat, V = V))
}

  
 
  
  
  
  
  
  





