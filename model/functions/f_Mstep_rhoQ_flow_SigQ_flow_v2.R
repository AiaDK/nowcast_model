#
f_Mstep_rhoQ_flow_SigQ_flow_v2 <- function(data, stT, PtT, params, options) {
  
  ###  rhoQ_flow & SigQ_flow  ###
  
  if (options$Nj > 0) {
    id_eQ_flow <- (options$Nr*options$Npmax + (options$Nj+1)*(options$Nmx+options$Nmz) + 1) : (options$Nr*options$Npmax + (options$Nj+1)*(options$Nmx+options$Nmz) + options$Nq_flow)
    id_eQ_flow_lags <- (options$Nr*options$Npmax + (options$Nj+1)*(options$Nmx+options$Nmz) + options$Nq_flow + 1) : (options$Nr*options$Npmax + (options$Nj+1)*(options$Nmx+options$Nmz) + (options$Nj+1)*options$Nq_flow)
    
    for (i in 1:options$Nq_flow) {
      
      ###  rhoQ_flow ###
      numer <- stT[id_eQ_flow[i], , drop=FALSE] %*% t(stT[id_eQ_flow_lags[i], , drop=FALSE]) + sum(PtT[id_eQ_flow[i], id_eQ_flow_lags[i], ])
      denom <- stT[id_eQ_flow_lags[i], , drop=FALSE] %*% t(stT[id_eQ_flow_lags[i], , drop=FALSE]) + sum(PtT[id_eQ_flow_lags[i], id_eQ_flow_lags[i], ])
      params$rhoQ_flow[i, 1] <- solve(denom, numer) 
      
      ###  SigQ_flow  ###
      e_Q_flow <- stT[id_eQ_flow[i], , drop=FALSE]
      e_Q_flow_lags <- stT[id_eQ_flow_lags[i], , drop=FALSE]
      temp <- (e_Q_flow %*% t(e_Q_flow) + sum(PtT[id_eQ_flow[i], id_eQ_flow[i], ])) - 
        params$rhoQ_flow[i, 1, drop=FALSE] %*% (e_Q_flow %*% t(e_Q_flow_lags) + sum(PtT[id_eQ_flow[i], id_eQ_flow_lags[i], ]))
      params$SigQ_flow[i, 1] <- 1/options$Nt * temp
    }
    
  } else {
    
    id_Nq_flow <- (options$Nmx+options$Nmz+1) : (options$Nmx+options$Nmz+options$Nq_flow)
    Zq_flow <- cbind(kronecker(matrix(c(1, 2, 3, 2, 1),1,5), params$lambdaQ_flow), 
                     kronecker(matrix(c(1, 2, 3, 2, 1),1,5), diag(1, options$Nq_flow)))
    temp <- matrix(0, options$Nq_flow, options$Nq_flow)
    
    for (t in 1:options$Nt) {
      datatemp <- data[id_Nq_flow, t, drop=FALSE]
      Wt <- diag(!is.na(datatemp))
      datatemp[is.na(datatemp)] <- 0 
      temp <- temp + 
        Wt %*% (datatemp %*% t(datatemp)) %*% t(Wt) - 
        Wt %*% datatemp %*% t(stT[, t, drop=FALSE]) %*% t(Zq_flow) %*% Wt - 
        Wt %*% Zq_flow %*% stT[, t, drop=FALSE] %*% t(datatemp) + 
        Wt %*% Zq_flow %*% (stT[, t, drop=FALSE] %*% t(stT[, t, drop=FALSE]) + PtT[, , t]) %*% t(Zq_flow) %*% Wt + 
        (diag(1, options$Nq_flow) - Wt) %*% mdiag(params$SigQ_flow) %*% (diag(1, options$Nq_flow) - Wt)
    }
    params$SigQ_flow <- as.matrix(diag(1/options$Nt * temp)) 
    params$rhoQ_flow <- matrix(0,0,0) #?# in the original was params$rhoMx
  }
  
  return(params)
}
