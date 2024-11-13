
f_Mstep_lambdaQ_flow <- function(stT, PtT, data, params, options) {
  
  # Extract a few things and define identifiers
  
  id_Nq_flow <- (options$Nmx+options$Nmz+1) : (options$Nmx+options$Nmz+options$Nq_flow)
  id_f <- 1 : (5*options$Nr)
  f_lags <- stT[id_f, , drop=FALSE]
  
  if (options$Nj > 0) {
    if (options$Nmz > 0) {
      id_eQ_flow <- (options$Nr*options$Npmax + (options$Nj+1)*options$Nmx + (options$Nj+1)*options$Nmz+1) : (options$Nr*options$Npmax + (options$Nj+1)*options$Nmx + (options$Nj+1)*options$Nmz + options$Nq_flow)
    } else {
      id_eQ_flow <- (options$Nr*options$Npmax + (options$Nj+1)*options$Nmx+1) : (options$Nr*options$Npmax + (options$Nj+1)*options$Nmx + options$Nq_flow)
    }
    e_Q_flow <- stT[id_eQ_flow, , drop=FALSE]
  }
  
  denom <- matrix(0, 5*options$Nr*options$Nq_flow, 5*options$Nr*options$Nq_flow)
  numer <- matrix(0, options$Nq_flow, 5*options$Nr)
  f_f_R <- matrix(0, 5*options$Nr*options$Nq_flow, 5*options$Nr*options$Nq_flow)
  
  if (options$Nj > 0) {
    for (t in 1:options$Nt) {
      datatemp <- data[id_Nq_flow, t, drop=FALSE]
      Wt <- mdiag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0 
      
      denom <- denom + kronecker((f_lags[, t, drop=FALSE] %*% t(f_lags[, t, drop=FALSE]) + PtT[id_f, id_f, t]), Wt)
      f_f_R <- f_f_R + kronecker((f_lags[, t, drop=FALSE] %*% t(f_lags[, t, drop=FALSE]) + PtT[id_f, id_f, t]), mdiag(params$SigQ_flow))
      numer <- numer + Wt %*% datatemp %*% t(f_lags[, t, drop=FALSE]) - (Wt %*% e_Q_flow[, t, drop=FALSE] %*% t(f_lags[, t, drop=FALSE]) + PtT[id_eQ_flow, id_f, t])
    }
  } else {
    for (t in 1:options$Nt) {
      datatemp <- data[id_Nq_flow, t, drop=FALSE]
      Wt <- mdiag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0
      denom <- denom + kronecker((f_lags[,t, drop=FALSE] %*% t(f_lags[,t, drop=FALSE]) + PtT[id_f,id_f,t]), Wt)
      numer <- numer + Wt %*% datatemp %*% t(f_lags[, t, drop=FALSE])
      f_f_R <- f_f_R + kronecker((f_lags[,t, drop=FALSE] %*% t(f_lags[,t, drop=FALSE]) + PtT[id_f,id_f,t]), mdiag(params$SigQ_flow))
    }
  }
  
  vec_lambdaQ_flow_u <- as.matrix(solve(denom, c(numer))) 
  
  ###  Impose restrictions  ###
  
  H_lam <- kronecker(rbind(cbind(2*diag(1,options$Nq_flow), -diag(1,options$Nq_flow), matrix(0,options$Nq_flow,3*options$Nq_flow)),
                           cbind(3*diag(1,options$Nq_flow), matrix(0,options$Nq_flow,1*options$Nq_flow), -diag(1,options$Nq_flow), matrix(0,options$Nq_flow,2*options$Nq_flow)),
                           cbind(2*diag(1,options$Nq_flow), matrix(0,options$Nq_flow,2*options$Nq_flow), -diag(1,options$Nq_flow), matrix(0,options$Nq_flow,1*options$Nq_flow)),
                           cbind(diag(1,options$Nq_flow), matrix(0,options$Nq_flow,3*options$Nq_flow), -diag(1,options$Nq_flow))), 
                     diag(1,options$Nr))
  
  kappa_lam <- matrix(0, nrow(H_lam), 1)
  
  vec_lambdaQ_flow_r <- vec_lambdaQ_flow_u + (solve(f_f_R, t(H_lam)) %*% solve(H_lam %*% solve(f_f_R) %*% t(H_lam))) %*% (kappa_lam - H_lam %*% vec_lambdaQ_flow_u)
  dim(vec_lambdaQ_flow_r) = c(options$Nq_flow, options$Nr*5)

  params$lambdaQ_flow <- vec_lambdaQ_flow_r[, 1:options$Nr, drop=FALSE] 
  
  return(params)
}