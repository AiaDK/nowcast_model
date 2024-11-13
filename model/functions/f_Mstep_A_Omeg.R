#
f_Mstep_A_Omeg <- function(stT, PtT, params, options){
  
  # Extract few things and define identifiers
  id_f <- 1:options$Nr
  id_f_lags <- (options$Nr+1) : (options$Nr*(options$Np + 1))
  f <- stT[id_f, , drop=FALSE]
  f_lags <- stT[id_f_lags, , drop=FALSE]

  # Update A
  
  if (length(id_f) == 1) {
    params$A <- (f %*% t(f_lags) + rowSums(PtT[id_f,id_f_lags,])) %*% solve((f_lags %*% t(f_lags) + apply(PtT[id_f_lags, id_f_lags, ], 2, rowSums)))
  } else {
    params$A <- (f %*% t(f_lags) + apply(PtT[id_f, id_f_lags, ], 2, rowSums)) %*% solve((f_lags %*% t(f_lags) + apply(PtT[id_f_lags, id_f_lags, ], 2, rowSums)))
  }
  
  
  # Update Omeg
  if (length(id_f) == 1) {
    params$Omeg <- 1 / options$Nt * ((f %*% t(f) + sum(PtT[id_f, id_f, ])) - params$A %*% t(f %*% t(f_lags) + rowSums(PtT[id_f,id_f_lags,])))
  } else {
    params$Omeg <- 1 / options$Nt * ((f %*% t(f) + apply(PtT[id_f, id_f, ], 2, rowSums)) - params$A %*% t(f %*% t(f_lags) + apply(PtT[id_f, id_f_lags, ], 2, rowSums)))
  }
  
  return(params)
}
