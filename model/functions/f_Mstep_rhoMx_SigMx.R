#
f_Mstep_rhoMx_SigMx <- function(stT, PtT, data, params, options) {
  
  # Extract a few things and define identifiers
  
  id_Nmx <- 1:options$Nmx
  id_eMx <- (options$Npmax*options$Nr+1) : (options$Npmax*options$Nr+options$Nmx)
  if ((options$Npmax*options$Nr+options$Nmx+1) < (options$Npmax*options$Nr+options$Nmx*(options$Nj+1))) {
    id_eMx_lags <- (options$Npmax*options$Nr+options$Nmx+1) : (options$Npmax*options$Nr+options$Nmx*(options$Nj+1))
  } else {
    id_eMx_lags <- NULL
  }
  id_f <- 1:options$Nr
  temp <- matrix(0, options$Nmx, options$Nmx)
  f <- stT[id_f, , drop=FALSE]
  
  # rhoMx & SigMx
  
  if (options$Nj >0) {
    
    for (i in 1:options$Nmx) {
      
      # rhoMx
      numer <-  stT[id_eMx[i]:id_eMx[i],,drop=FALSE] %*% t(stT[id_eMx_lags[i]:id_eMx_lags[i],,drop=FALSE]) + sum(PtT[id_eMx[i],id_eMx_lags[i],])
      denom <- stT[id_eMx_lags[i],,drop=FALSE] %*% t(stT[id_eMx_lags[i],,drop=FALSE]) + sum(PtT[id_eMx_lags[i],id_eMx_lags[i],])
      params$rhoMx[i, 1] <- solve(denom, numer) 
      
      # SigMx
      e_Mx <- stT[id_eMx[i], , drop=FALSE]
      e_Mx_minusone <- stT[id_eMx_lags[i], , drop=FALSE]
      temp <- (e_Mx %*% t(e_Mx) +  sum(PtT[id_eMx[i],id_eMx[i],])) - params$rhoMx[i,1,drop=FALSE] %*% (e_Mx %*% t(e_Mx_minusone) + sum(PtT[id_eMx_lags[i],id_eMx[i],]))
      params$SigMx[i,1] <- 1/options$Nt * temp
    }
    
  } else {
    
    for (t in 1:options$Nt) {
      datatemp <- data[id_Nmx, t, drop=FALSE]
      Wt <- mdiag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0
      temp <- temp + 
        Wt %*% (datatemp %*% t(datatemp)) %*% t(Wt) - 
        Wt %*% datatemp %*% t(f[, t, drop=FALSE]) %*% t(params$lambdaMx) %*% Wt -
        Wt %*% params$lambdaMx %*% f[, t, drop=FALSE] %*% t(datatemp) +
        Wt %*% params$lambdaMx %*% (f[, t, drop=FALSE] %*% t(f[, t, drop=FALSE]) + PtT[id_f, id_f, t]) %*% t(params$lambdaMx) %*% Wt +
        (diag(1, options$Nmx) - Wt) %*% mdiag(params$SigMx) %*% (diag(1, options$Nmx) - Wt)
    }
    
    params$SigMx <- as.matrix(mdiag(1/options$Nt * temp)) 
    params$rhoMx <- matrix(0,0,0)
  }
  
  return(params)
}
