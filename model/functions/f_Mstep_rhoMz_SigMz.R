
#?# Notes:
  #?#  in case Nmz <> 0, adjust the file (drop = FALSE)

f_Mstep_rhoMz_SigMz <- function(stT, PtT, data, params, options) {
  
  # rhoMz & SigMz
  
  if (options$Nj > 0) {
    id_eMz <- (options$Npmax*options$Nr+(options$Nj+1)*options.Nmx+1) : (options$Npmax*options$Nr+(options$Nj+1)*options$Nmx+options$Nmz)
    id_eMz_lags <- (options$Npmax*options$Nr+(options$Nj+1)*options.Nmx+options$Nmz+1) : (options$Npmax*options.Nr+(options$Nj+1)*options$Nmx+options$Nmz*(options$Nj+1))
    
    for (i in 1:options$Nmz) {
      
      ###  rhoMz  ###
      numer <- stT[id_eMz[i], ] %*% t(stT[id_eMz_lags[i], ]) + apply(PtT[id_eMz[i], id_eMz_lags[i], ], 2, rowSums)
      denom <- stT[id_eMz_lags[i], ] %*% t(stT[id_eMz_lags[i], ]) + apply(PtT[id_eMz_lags[i], id_eMz_lags[i], ], 2, rowSums)
      params$rhoMz[i, 1] <- solve(denom, numer) 
      
      ###  SigMz  ### 
      eMz <- stT[id_eMz[i], , drop = FALSE] 
      eMz_lags <- stT[id_eMz_lags[i], , drop = FALSE] 
      temp <- (eMz %*% t(eMz_lags) + apply(PtT[id_eMz[i], id_eMz[i], ], 2, rowSums)) - 
                 params$rhoMz[i, 1] %*% (eMz %*% t(eMz_lags) + apply(PtT[id_eMz[i], id_eMz_lags[i], ], 2, rowSums))
      params$SigMz[i, 1] <- 1/options$Nt * temp 
    }
    
  } else {
    id_Nmz <- (options$Nmx+1) : (options$Nmx+options$Nmz)
    id_f <- 1 : (options$Npmax*options$Nr)
    f <- matrix(stT[id_f, ], length(id_f))
    temp <- matrix(0, options$Nmz, options$Nmz)
    
    for (t in 1:options$Nt) {
      
      datatemp <- matrix(data[id_Nmz, t], length(id_Nmz))
      Wt <- diag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0
      
      if (is_empty(Wt)) { #?# check
        temp <- temp + diag(1, options$Nmz) %*% mdiag(params$SigMz) %*% diag(1, options$Nmz)
        
      } else {
        temp  <- temp + Wt %*% (datatemp %*% t(datatemp)) %*% t(Wt) - Wt %*% datatemp %*% t(f[, t]) %*% t(matrix(kronecker(matrix(1, 1, 12), params$lambdaMz), nrow(params$lambdaMz))) %*% Wt - 
          Wt %*% matrix(kronecker(matrix(1, 1, 12), params$lambdaMz), nrow(params$lambdaMz)) %*% f[, t] %*% t(datatemp) + 
          Wt %*% matrix(kronecker(matrix(1, 1, 12), params$lambdaMz), nrow(params$lambdaMz)) %*% (f[, t] %*% t(f[, t]) + PtT[id_f, id_f, t]) %*% t(matrix(kronecker(matrix(1, 1, 12), params$lambdaMz), nrow(params$lambdaMz))) %*% Wt + 
          (diag(1, options$Nmz) - Wt) %*% mdiag(params$SigMz) %*% (diag(1, options$Nmz) - Wt)
      }
    }
    params$SigMz <- diag(1/options$Nt %*% temp) 
    params$rhoMz <- matrix(0,0,0)
  }
  
  return(params)
}
  