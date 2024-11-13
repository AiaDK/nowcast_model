
f_statespaceparamsEM <- function(params, options) {
  # - model is y_t = Z * a_t + e_t; e_t ~ N(0,H) 
  # -          a_t = T * a_t + R*u_t; u_t ~ N(0,Q) 
  # ----------------------------------------------------------------------- #
    # - Case distinction:
    # - If options$Nmz>0 => include 11 lags of the factors in state vector,
  # - i.e. a_t = [f_t, ..., f_t-12, e^Q_t, ..., e^Q_t-4]
  # - else a_t = [f_t, ..., f_t-4, e^Q_t, .., e^Q_t-4] to allow for I(1)
  # - flow variables such as GDP$
  # - If options$Nj>0, then we also include e^x and e^z in
  # - the state vector, i$e$ a_t = [f_t, ..., f_t-12, e^x_t,$$$,e^x_t-(Nj+1),e^z_t,$$$,e^z_t-(Nj+1),e^Q_t, ..., e^Q_t-4]
  # ----------------------------------------------------------------------- #
    # ----------------------------------------------------------------------- #
    # - COMMENTS
  # - 2018-5-5: currently only options$Np = 1 supported. Need to fix that. 
  # - 2018-5-13: include e^x_t and e^x_t-1 in state vector if options$Nj=1 => need
  # -            for estimation of rho 
  # - 2018-05-15: change code to model XXXXXXXXXXXXXX
  # ----------------------------------------------------------------------- #
  
  
  ###  Q  ###
  
  if(options$Nj > 0){
    Q <- matrix(0, options$Nr+options$Nmx+options$Nmz+options$Nq, options$Nr+options$Nmx+options$Nmz+options$Nq)
    Q[1:options$Nr, 1:options$Nr] <- params$Omeg
    Q[(options$Nr+1) : (options$Nr+options$Nmx), (options$Nr+1) : (options$Nr+options$Nmx)] <- mdiag(params$SigMx)
    Q[ifelse(((options$Nr+options$Nmx+1)>(options$Nr+options$Nm)), 0, ((options$Nr+options$Nmx+1):(options$Nr+options$Nm))),  
      ifelse(((options$Nr+options$Nmx+1)>(options$Nr+options$Nm)), 0, ((options$Nr+options$Nmx+1):(options$Nr+options$Nm)))] <- mdiag(params$SigMz)
    Q[(options$Nr+options$Nm+1):nrow(Q), (options$Nr+options$Nm+1):ncol(Q)] <- rbind(cbind(mdiag(params$SigQ_flow), 
                                                                                           matrix(0, options$Nq_flow, options$Nq_stock)), 
                                                                                     cbind(matrix(0, options$Nq_stock, options$Nq_flow), 
                                                                                           mdiag(params$SigQ_stock)))
  } else {
    Q <- matrix(0, options$Nr+options$Nq, options$Nr+options$Nq)
    Q[1:options$Nr, 1:options$Nr] <- params$Omeg
    Q[(options$Nr+1):nrow(Q), (options$Nr+1):ncol(Q)] <- rbind(cbind(mdiag(params$SigQ_flow), 
                                                                     matrix(0, options$Nq_flow, options$Nq_stock)), 
                                                               cbind(matrix(0, options$Nq_stock, options$Nq_flow), 
                                                                     mdiag(params$SigQ_stock)))
  }
  
  ###  R  ###
  
  if (options$Nj > 0) {
    R <- matrix(0, options$Nstates, options$Nr+options$Nmx+options$Nmz+options$Nq)
    R[1:options$Nr, 1:options$Nr] <- diag(1, options$Nr)
    R[(options$Npmax*options$Nr+1):(options$Npmax*options$Nr+options$Nmx), (options$Nr+1):(options$Nr+options$Nmx)] <- diag(1, options$Nmx)
    R[ifelse(((options$Npmax*options$Nr+(options$Nj+1)*options$Nmx+1)>(options$Npmax*options$Nr+(options$Nj+1)*options$Nmx+options$Nmz)), 0, ((options$Npmax*options$Nr+(options$Nj+1)*options$Nmx+1):(options$Npmax*options$Nr+(options$Nj+1)*options$Nmx+options$Nmz))), 
      ifelse(((options$Nr+options$Nmx+1)>(options$Nr+options$Nm)), 0, ((options$Nr+options$Nmx+1):(options$Nr+options$Nm)))] <- diag(1, options$Nmz)
    R[(options$Npmax*options$Nr+(options$Nj+1)*(options$Nmx+options$Nmz)+1):(options$Npmax*options$Nr+(options$Nj+1)*(options$Nmx+options$Nmz)+options$Nq), (options$Nr+options$Nm+1):ncol(R)] <- diag(1, options$Nq)
  } else {
    R <- matrix(0, options$Nstates, options$Nr+options$Nq)
    R[1:options$Nr, 1:options$Nr] <- diag(1, options$Nr)
    R[(options$Npmax*options$Nr+1):(options$Npmax*options$Nr+options$Nq), (options$Nr+1):ncol(R)] <- diag(1, options$Nq)
  }
  
  ###  T  ###
  
  if (options$Nj > 0) {
    Tr <- rbind(cbind(params$A, 
                      matrix(0, options$Nr, options$Nstates-options$Nr*options$Np)),
                cbind(diag(1, options$Nr*(options$Npmax-1)), 
                      matrix(0, options$Nr*(options$Npmax-1), options$Nr), 
                      matrix(0, options$Nr*(options$Npmax-1), (options$Nj+1)*(options$Nmx+options$Nmz)+5*options$Nq)))
    Tez_temp <- rbind(cbind(mdiag(params$rhoMz), 
                            matrix(0, options$Nmz, options$Nmz*options$Nj)), 
                      cbind(diag(1, options$Nmz*options$Nj), 
                            matrix(0, options$Nmz*options$Nj, options$Nmz))) 
    Tez <- cbind(matrix(0, (options$Nj+1)*options$Nmz, nrow(Tr)), 
                 matrix(0, (options$Nj+1)*options$Nmz, (options$Nj+1)*options$Nmx), 
                 Tez_temp, 
                 matrix(0, (options$Nj+1)*options$Nmz, 5*options$Nq))
    Teq_temp <- rbind(cbind(mdiag(rbind(params$rhoQ_flow, matrix(params$rhoQ_stock, ncol=ncol(params$rhoQ_flow)))), 
                            matrix(0, options$Nq, options$Nq*4)), 
                      cbind(diag(1, 4*options$Nq), 
                            matrix(0, 4*options$Nq, options$Nq))) 
    Teq <- cbind(matrix(0, 5*options$Nq, options$Nstates-5*options$Nq), 
                 Teq_temp)
    Tex <- rbind(cbind(matrix(0, options$Nmx, nrow(Tr)), 
                       mdiag(params$rhoMx), 
                       matrix(0, options$Nmx, options$Nj*options$Nmx), 
                       matrix(0, options$Nmx, options$Nstates-(nrow(Tr)+(options$Nj+1)*options$Nmx))),
                 cbind(matrix(0, options$Nj*options$Nmx, nrow(Tr)), 
                       diag(1, options$Nj*options$Nmx), 
                       matrix(0, options$Nj*options$Nmx, options$Nmx), 
                       matrix(0, options$Nj*options$Nmx, options$Nstates-(nrow(Tr)+(options$Nj+1)*options$Nmx))))
    T <- rbind(Tr, Tex, Tez, Teq)
  } else {
    Tr <- rbind(cbind(params$A, 
                      matrix(0, options$Nr, options$Nstates-options$Nr*options$Np)),
                cbind(diag(1, options$Nr*(options$Npmax-1)), 
                      matrix(0, options$Nr*(options$Npmax-1), options$Nr), 
                      matrix(0, options$Nr*(options$Npmax-1), 5*options$Nq)))
    Teq_temp <- rbind(cbind(matrix(0, options$Nq, options$Nq), 
                            matrix(0, options$Nq, options$Nq*4)),
                      cbind(diag(1, 4*options$Nq), 
                            matrix(0, 4*options$Nq, options$Nq))) 
    Teq <- cbind(matrix(0, 5*options$Nq, options$Nstates-5*options$Nq), Teq_temp)
    T <- rbind(Tr,  Teq)
  }
  
  ###  Z  ### 
  
  if (options$Nj > 0) {
    
    Zmx <- cbind(params$lambdaMx, 
                 matrix(0, options$Nmx, (options$Npmax-1)*options$Nr), 
                 diag(1, options$Nmx), 
                 matrix(0, options$Nmx, options$Nj*options$Nmx), 
                 matrix(0, options$Nmx, options$Nstates-options$Npmax*options$Nr-(options$Nj+1)*options$Nmx))
    Zmz <- cbind(kronecker(matrix(1, 1, 12), params$lambdaMz), 
                 matrix(0, options$Nmz, (options$Nj+1)*options$Nmx), 
                 diag(1, options$Nmz), 
                 nmatrix(0, options$Nmz, options$Nstates-12*(options$Nr)-(options$Nj+1)*options$Nmx-options$Nmz))
    Zq_flow <- cbind(kronecker(matrix(c(1, 2, 3, 2, 1), 1, 5), params$lambdaQ_flow), 
                     matrix(0, options$Nq_flow, options$Npmax*options$Nr+(options$Nj+1)*(options$Nmx+options$Nmz)-5*options$Nr), 
                     kronecker(matrix(c(1, 2, 3, 2, 1), 1, 5), cbind(diag(1, options$Nq_flow), matrix(0, options$Nq_flow, options$Nq_stock))))  
    
    if (options$Nmz == 0) {
      Z <- rbind(Zmx, Zq_flow)
    } else {
      Z <- rbind(Zmx, Zmz, Zq_flow)
    }
    
  } else {
    Zmx <- cbind(params$lambdaMx, 
                 nmatrix(0, options$Nmx, options$Nstates-options$Nr))
    Zmz <- cbind(kronecker(matrix(1, 1, 12), params$lambdaMz), 
                 nmatrix(0, options$Nmz, options$Nstates-12*options$Nr))
    Zq_flow <- cbind(kronecker(matrix(c(1, 2, 3, 2, 1), 1, 5), params$lambdaQ_flow), 
                     nmatrix(0, options$Nq_flow, (options$Npmax-5)*options$Nr), 
                     kronecker(matrix(c(1, 2, 3, 2, 1), 1, 5), cbind(diag(1, options$Nq_flow), nmatrix(0, options$Nq_flow, options$Nq_stock))))  
    
    if(options$Nmz == 0){
      Z <- rbind(Zmx, Zq_flow)
    } else {
      Z <- rbind(Zmx, Zmz, Zq_flow)
    }
  }
  
  ### H  ###
  
  kappa <- 1e-8
  if (options$Nj > 0) {
    H <- mdiag(kappa * matrix(1, options$Nm+options$Nq, 1))
  } else {
    H <- mdiag(rbind(params$SigMx, 
                     cbind(params$SigMz, matrix(0, nrow(params$SigMz), ncol(params$SigMx)-ncol(params$SigMz))), 
                     kappa * matrix(1, options$Nq, 1)))
  }
  
  return(list(T = T, Z = Z, R = R, Q = Q, H = H))  
}
