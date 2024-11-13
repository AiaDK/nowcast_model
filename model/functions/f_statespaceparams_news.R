#
f_statespaceparams_news <- function(params, options) {
  
  if (options$Nj == 0) {
    # state vector is [factors_t
    #                  u^Q_t
    #                  factors_t-1
    #                  u^Q_t-1
    #                  ...
    #                  factors_t-4
    #                  u^Q_t-4]
    
    phi_temp <- matrix(0,0,0)
      for (p in 1:options$Np){
      phi_temp <- m3cbind(phi_temp, params$A[1:options$Nr, ((p-1)*options$Nr+1) : (p*options$Nr)], matrix(0, options$Nr, options$Nq))
      }
    phi_temp <- rbind(phi_temp, matrix(0, options$Nq, (options$Nr+options$Nq)*options$Np))
    
    T <- rbind(cbind(phi_temp, matrix(0, options$Nr+options$Nq, (5-options$Np)*(options$Nr+options$Nq))), 
               cbind(diag(1, 4*(options$Nr+options$Nq)), matrix(0, 4*(options$Nr+options$Nq), options$Nr+options$Nq)))
    
    Q <- matrix(0, options$Nr+options$Nq, options$Nr+options$Nq)
    Q[1:options$Nr, 1:options$Nr] <- params$Omeg
    Q[(options$Nr+1) : (options$Nr+options$Nq), (options$Nr+1) : (options$Nr+options$Nq)] <- mdiag(params$SigQ_flow)

    R <- rbind(diag(1, options$Nr+options$Nq), matrix(0, 4*(options$Nr+options$Nq), options$Nr+options$Nq))
    
    Z <- rbind(cbind(params$lambdaMx, matrix(0, options$Nm, options$Nq), matrix(0, options$Nm, 4*(options$Nr+options$Nq))), 
               cbind(kronecker(matrix(c(1, 2, 3, 2, 1), 1, 5), cbind(params$lambdaQ_flow, diag(1, options$Nq)))))
    
    kappa <- 1e-14

    H <- kappa * diag(1, options$Nm+options$Nq)
    H[1:options$Nm, 1:options$Nm] <- mdiag(params$SigMx)
    
  } else {
    
    if(options$Nj > 1){
      print("Code currently only supports Nj=1. Should crash....")
    }
    
    # state vector is [factors_t
    #                  u^Q_t
    #                  u^M_t
    #                  factors_t-1
    #                  u^Q_t-1
    #                  ...
    #                  factors_t-4
    #                  u^Q_t-4]
    
    phi_temp <- matrix(0,0,0) 
    for (p in 1:options$Np){
      if (p == 1) {
        phi_temp <- m3cbind(phi_temp, params$A[1:options$Nr, ((p-1)*options$Nr+1) : (p*options$Nr)], matrix(0, options$Nr, options$Nq+options$Nm))
      } else {
        phi_temp <- m3cbind(phi_temp, params$A[1:options$Nr, ((p-1)*options$Nr+1) : (p*options$Nr)], matrix(0, options$Nr, options$Nq))
      }
    }
    
    phi_temp <- rbind(phi_temp, 
                      cbind(matrix(0, options$Nq, options$Nr), mdiag(params$rhoQ_flow), matrix(0, options$Nq, options$Nm), matrix(0, options$Nq, (options$Np-1)*(options$Nq+options$Nr))),
                      cbind(matrix(0, options$Nm, options$Nr+options$Nq), mdiag(params$rhoMx), matrix(0, options$Nm, (options$Np-1)*(options$Nq+options$Nr))))
    
    T <- rbind(cbind(phi_temp, matrix(0, options$Nr+options$Nq+options$Nm, (5-options$Np)*(options$Nr+options$Nq))),
               cbind(diag(1, options$Nr+options$Nq), matrix(0, options$Nr+options$Nq, options$Ns-(options$Nr+options$Nq))), 
               cbind(matrix(0, 3*(options$Nr+options$Nq), options$Nr+options$Nq+options$Nm), diag(1, 3*(options$Nr+options$Nq)), matrix(0, 3*(options$Nr+options$Nq), options$Nr+options$Nq)))
    
    
    Q <- matrix(0, options$Nr+options$Nq+options$Nm, options$Nr+options$Nq+options$Nm)
    Q[1:options$Nr, 1:options$Nr] <- params$Omeg
    Q[(options$Nr+1) : (options$Nr+options$Nq), (options$Nr+1) : (options$Nr+options$Nq)] <- mdiag(params$SigQ_flow)
    Q[(options$Nr+options$Nq+1) : (options$Nr+options$Nq+options$Nm), (options$Nr+options$Nq+1) : (options$Nr+options$Nq+options$Nm)] <- mdiag(params$SigMx)
    
    R <- rbind(diag(1, options$Nr+options$Nq+options$Nm), matrix(0, 4*(options$Nr+options$Nq), options$Nr+options$Nq+options$Nm))
    
    Z <- rbind(cbind(params$lambdaMx, matrix(0, options$Nm, options$Nq), diag(1, options$Nm), matrix(0, options$Nm, 4*(options$Nr+options$Nq))),
               cbind(params$lambdaQ_flow, diag(1, options$Nq), matrix(0, options$Nq, options$Nm), kronecker(matrix(c(2, 3, 2, 1), 1, 4), cbind(params$lambdaQ_flow, diag(1, options$Nq)))))
    
    kappa <- 1e-08
    H <- kappa * diag(1, options$Nm+options$Nq)
  }

  return(list(T=T, Z=Z, R=R, Q=Q, H=H))
}