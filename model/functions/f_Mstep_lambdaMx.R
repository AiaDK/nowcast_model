#
f_Mstep_lambdaMx <- function(stT, PtT, data, params, options) {
  
  # Extract few things and define identifiers
  
  id_Nmx <- 1 : options$Nmx
  id_eMx <- (options$Nr*options$Npmax+1) : (options$Nr*options$Npmax+options$Nmx)
  id_f <- 1 : options$Nr
  f <- stT[id_f, , drop=FALSE]
  
  denom <- matrix(0, options$Nr * options$Nmx, options$Nr * options$Nmx)
  numer <- matrix(0, options$Nmx, options$Nr)
  
  # lambdaMx
  
  if (options$Nj > 0) {
    e_Mx <- stT[id_eMx, , drop=FALSE] 
    for (t in 1 : options$Nt){
     datatemp <- data[id_Nmx, t, drop=FALSE]
     Wt <- mdiag(!is.na(datatemp))
     datatemp[is.na(datatemp)] <- 0 
     denom <- denom + kronecker((f[,t,drop=FALSE] %*% t(f[,t,drop=FALSE]) + PtT[id_f,id_f,t]), Wt) # see Watson and Engle (1982, eqn. 16) for the precise formula (sum of squred smoothed factors + smoothed covariance)
     numer <- numer + Wt %*% datatemp %*% t(f[,t,drop=FALSE]) - (Wt %*% e_Mx[,t,drop=FALSE] %*% t(f[,t,drop=FALSE]) + PtT[id_eMx,id_f,t])
     }
  } else {
    for(t in 1 : options$Nt){
      datatemp <- data[id_Nmx, t, drop=FALSE]
      Wt <- mdiag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0
      denom <- denom + kronecker((f[,t, drop=FALSE] %*% t(f[,t, drop=FALSE]) + PtT[id_f, id_f, t]), Wt) # see Watson and Engle (1982, eqn. 16) for the precise formula (sum of squred smoothed factors + smoothed covariance)
      numer <- numer + Wt %*% datatemp %*% t(f[,t, drop=FALSE])
    }
  }
  
  lambdaMx <- solve(denom, c(numer))
  dim(lambdaMx) <- c(options$Nmx, options$Nr)
  params$lambdaMx <- lambdaMx 
  
  return(params)
  
}
