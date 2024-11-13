
#?# Notes:
  #?# in case Nmz <> 0, adjust this file (drop=FALSE)

f_Mstep_lambdaMz <- function(stT, PtT, data, params, options){
  
  # Extract a few things and define identifiers
  
  id_Nmz <- (options$Nmx+1) : (options$Nmx+options$Nmz)
  id_f <- 1 : (options$Npmax * options$Nr) 
  f <- stT[id_f, , drop=FALSE]
  
  if (options$Nj > 0) {
    id_eMz <- (options$Nr*options$Npmax+(options$Nj+1)*options$Nmx+1) : (options$Nr*options$Npmax+(options$Nj+1)*options$Nmx+options$Nmz)
    e_Mz <- stT[id_eMz, , drop = FALSE] 
  }
  
  # lambdaMZ
  
  denom <- matrix(0, 12*options$Nr*options$Nmz, 12*options$Nr*options$Nmz)
  numer <- matrix(0, options$Nmz, 12*options$Nr)
  f_f_R <- matrix(0, 12*options$Nr*options$Nmz, 12*options$Nr*options$Nmz)
  
  if (options$Nj > 0) {
    for (t in 1:options$Nt) {
      datatemp <- data[id_Nmz, t]
      Wt <- mdiag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0 
      denom <- denom + kronecker(f[, t] %*% t(f[, t]) + PtT[id_f, id_f, t], Wt)
      f_f_R <- f_f_R + kronecker(f[, t] %*% t(f[, t]) + PtT[id_f, id_f, t], mdiag(params$SigMz))
      numer <- numer + Wt %*% datatemp %*% t(f[, t])  - (Wt %*% e_Mz[, t] %*% t(f[, t]) + PtT[id_eMz, id_f, t])
    }

  } else {
    for (t in 1:options$Nt) {
      datatemp <- data[id_Nmz, t]
      Wt <- mdiag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0
      if (is_empty(Wt)) { #?# check
      denom <- denom + kronecker(f[, t] %*% t(f[, t]) + PtT[id_f, id_f, t], 0)
      numer <- numer + 0 * datatemp %*% t(f[, t])
      } else {
        denom <- denom + kronecker(f[, t] %*% t(f[, t]) + PtT[id_f, id_f, t], Wt)
        numer <- numer + Wt %*% datatemp %*% t(f[, t])
      }
      f_f_R <- f_f_R + kronecker(f[, t] %*% t(f[, t]) + PtT[id_f, id_f, t], mdiag(params$SigMz))
    }
  }
  
  vec_lambdaMz_u <- as.matrix(solve(denom, c(numer))) #?# check
  
  # Impose restrictions
  
  H_lam <- kronecker(rbind(cbind(diag(1,options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,10*options$Nmz)), 
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,1*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,9*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,2*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,8*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,3*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,7*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,4*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,6*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,5*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,5*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,6*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,4*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,7*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,3*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,8*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,2*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,9*options$Nmz), -diag(1,options$Nmz), matrix(0,options$Nmz,1*options$Nmz)),
                           cbind(diag(1,options$Nmz), matrix(0,options$Nmz,10*options$Nmz), -diag(1,options$Nmz))), 
                     diag(1, options$Nr))
  
  kappa_lam <- matrix(0, nrow(H_lam), 1)
  
  vec_lambdaMz_r <- vec_lambdaMz_u + f_f_R %*% t(H_lam) %*% solve(H_lam %*% f_f_R %*% t(H_lam)) %*% (kappa_lam - H_lam %*% vec_lambdaMz_u)
  dim(vec_lambdaMz_r) <- c(options$Nmz, options$Nr*12)
  params$lambdaMz <- vec_lambdaMz_r[, 1:options$Nr, drop=FALSE] 

  return(params)
  
}