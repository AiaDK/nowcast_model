#
f_KS_DK_logL <- function(data, T, Z, H, R, Q, s0, P0) {
  
  # Empty matrices to store filtered and smoothed states (as well as their respective covariance matrices)
  stT <- matrix(NA, nrow(T), ncol(data))
  PtT <- array(NA, dim = c(nrow(T), nrow(T), ncol(data)))
  a <- matrix(NA, nrow(T), ncol(data)+1) 
  P <- array(NA, dim = c(nrow(T), nrow(T), ncol(data)+1)) 
  
  # Forward recursions
  
  # Empty cells to store v, F, and L
  F <- vector("list", length = ncol(data))
  L <- array(NA, dim = c(nrow(T), nrow(T), ncol(data)))
  v <- vector("list", length = ncol(data))
  
  # Initialize filter
  a[, 1] <- s0
  P[, , 1] <- P0
  LL <- -ncol(data)*nrow(data) / 2*log(2*pi)
  
  for (t in 1:ncol(data)) {
    
    # Check for missings
    missing <- is.na(data[, t])
    W <- diag(1, nrow(data))
    W <- W[!missing, ]
    WZ <- W %*% Z
    datatemp <- as.matrix(data[!missing, t, drop=FALSE])
    
    # Proceed with recursions
    v[[t]] <- datatemp - WZ %*% a[, t, drop=FALSE]
    F[[t]] <- WZ %*% P[,,t] %*% t(WZ) + W %*% H %*% t(W)
    K <- T %*% P[, , t] %*% t(WZ) %*% solve(F[[t]] + diag(1, nrow(datatemp)) * 1e-5) 
    L[,,t] <- T - K %*% WZ
    
    # Update state vector and its covariance matrix
    a[,t+1] <- T %*% a[,t, drop=FALSE] + K %*% v[[t]]
    P[,,t+1] <- T %*% P[,,t] %*% t(L[,,t]) + R %*% Q %*% t(R)
  
    # compute log likelyhood
    LL <- LL - 0.5 * (log(det(F[[t]])) + t(v[[t]]) %*% solve(F[[t]]) %*% v[[t]]) 
    
  }
  
  # Backward recursions
  
  r <- matrix(0, nrow(T), 1)
  N <- matrix(0, nrow(T), nrow(T)) 
  for (t in ncol(data):1) {
    # Check for missings
    missing <- is.na(data[, t])
    W <- diag(1, nrow(data))
    W <- W[!missing, ]
    WZ <- W %*% Z
    
    # Compute r
    r <- t(WZ) %*% solve(F[[t]] + diag(1, nrow(F[[t]])) * 1e-5) %*% v[[t]] + t(L[,,t]) %*% r
    
    # Smoothed state and covariance matrix
    stT[,t] <- a[,t, drop=FALSE] + P[,,t] %*% r
    N <- t(WZ) %*% solve(F[[t]] + diag(1, nrow(F[[t]])) * 1e-5) %*% WZ + t(L[,,t]) %*% N %*% L[,,t]
    PtT[,,t] <- P[,,t] - P[,,t] %*% N %*% P[,,t]
  }
  
  return(list(stT=stT, PtT=PtT, LL=LL))
}
  