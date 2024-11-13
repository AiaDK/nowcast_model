#
f_KalmanSmootherDK <- function(data, T, Z, H, R, Q, s0, P0) {
  
  # Empty matrices to store filtered and smoothed states (as well as their respective covariance matrices)
  stT <- matrix(NA, nrow(T), ncol(data))
  PtT <- array(NA, dim = c(nrow(T), nrow(T), ncol(data)))
  a <- matrix(NA, nrow(T), ncol(data)+1) 
  P <- array(NA, dim = c(nrow(T), nrow(T), ncol(data)+1)) 
  
  # Forward recursions
  
  # Empty cells to store v, F, and L
  iF <- vector("list", length = ncol(data))
  L <- array(NA, dim = c(nrow(T), nrow(T), ncol(data)))
  v <- vector("list", length = ncol(data))
  
  # Initialize filter
  a[, 1] <- s0
  P[, , 1] <- P0
  eye_N <- diag(1, nrow(data))

  for (t in 1:ncol(data)) {
    
    # Check for missings
    missing <- is.na(data[, t, drop=FALSE])
    W <- eye_N[!missing, , drop=FALSE]
    WZ <- W %*% Z
    datatemp <- data[!missing, t, drop=FALSE]
    
    # Proceed with recursions
    if (is_empty(WZ)){
      L[, , t] <- T
      # Update state vector and its covariance matrix
      a[, t+1] <- T %*% a[, t, drop=FALSE]
      P[, , t+1] <- T %*% P[, , t] %*% t(L[, , t]) + R %*% Q %*% t(R)
    } else {
      v[[t]] <- datatemp - WZ %*% a[, t, drop=FALSE]
      F <- WZ %*% P[, , t] %*% t(WZ) + W %*% H %*% t(W)
      iF[[t]] <- diag(1, nrow(F)) %*% solve(F + diag(1, nrow(datatemp)) * 1e-5)
      K <- T %*% P[, , t] %*% t(WZ) %*% iF[[t]]
      L[, , t] <- T - K %*% WZ
      # Update state vector and its covariance matrix
      a[, t+1] <- T %*% a[, t, drop=FALSE] + K %*% v[[t]]
      P[, , t+1] <- T %*% P[, , t] %*% t(L[, , t]) + R %*% Q %*% t(R)
    }
  }
  
  # Backward recursions
  
  r <- matrix(0, nrow(T), 1)
  N <- array(NA, dim = c(nrow(T), nrow(T), ncol(data)+1)) 
  N[, , dim(N)[3]] <- matrix(0, nrow(T), nrow(T))
  
  for (t in ncol(data):1) {
    # Check for missings
    missing <- is.na(data[, t, drop=FALSE])
    W <- eye_N[!missing, , drop=FALSE]
    WZ <- W %*% Z
    
    # Compute r
    if (is_empty(WZ)) { # all obs are missing => nothing to smooth! 
      r <- matrix(0, nrow(T), 1)
      stT[,t] <- a[,t,drop=FALSE] + P[,,t] %*% r
      N[,,t] <- t(L[,,t]) %*% N[,,t+1] %*% L[,,t]
      PtT[,,t] <- P[,,t]
    } else {
      r <- t(WZ) %*% iF[[t]] %*% v[[t]] + t(L[,,t]) %*% r
      stT[,t] <- a[,t,drop=FALSE] + P[,,t] %*% r
      N[,,t] <- t(WZ) %*% iF[[t]] %*% WZ + t(L[,,t]) %*% N[,,t+1] %*% L[,,t]
      PtT[,,t] <- P[,,t] - P[,,t] %*% N[,,t] %*% P[,,t]
    }
  }
  
  # Store output
  
  ks_output <- list()
  ks_output$stT <- stT
  ks_output$PtT <- PtT
  ks_output$P <- P
  ks_output$L <- L
  ks_output$N <- N
  
  return(ks_output)
}