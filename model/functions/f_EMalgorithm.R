
f_EMalgorithm <- function(data_m, data_q, options) {
  
  # Specify some more options
  options$Nq_flow <- nrow(data_q)
  options$Nmx <- nrow(data_m)
  options$Nmz <- 0
  options$Nq_stock <- 0
  options$Nm <- options$Nmx + options$Nmz
  options$Nq <- options$Nq_flow + options$Nq_stock
  options$Nt <- ncol(data_m)
  
  # Calculate a few additional options
  if (options$Nmz > 0) {
    options$Npmax <-  max(12, options$Np + 1) # => need lagged factors in state as well for A's M-step!!!!!
  } else if (options$Nq_flow > 0) {
    options$Npmax <-  max(5, options$Np + 1)
  } else {
    options$Npmax <-  max(3, options$Np + 1)
  }
  
  if (options$Nj > 0) {
    options$Nstates = options$Npmax * (options$Nr) + 5 * options$Nq + (options$Nj + 1) * (options$Nmx + options$Nmz) 
  } else {
    options$Nstates = options$Npmax * (options$Nr) + 5 * options$Nq
  }
  
  # Starting values
  data_mx <- data_m
  data_mz <- data.frame()  
  data_q_flow <- data_q
  data <- as.matrix(rbind(data_mx, data_mz, data_q_flow)) 
  params <- f_startingvalues(data_mx, data_mz, data_q_flow, options)
  
  # EM algorithm
  maxiter <- 100
  iter <- 0
  LL_prev <- -999999
  
  # tic
  for (iter in 1:maxiter) {
    
    # E-step #
    
    # State space formulation
    statespaceparamsEM <- f_statespaceparamsEM(params,options) 
    T <- statespaceparamsEM$T
    Z <- statespaceparamsEM$Z
    R <- statespaceparamsEM$R
    Q <- statespaceparamsEM$Q
    H <- statespaceparamsEM$H
    
    # Kalman Smoother
    s0 <- matrix(0, nrow(T), 1)
    P0 <- diag(1, nrow(T)) 
    KS_DK_logL <- f_KS_DK_logL(data, T, Z, H, R, Q, s0, P0) 
    stT <- KS_DK_logL$stT
    PtT <- KS_DK_logL$PtT
    LL <- KS_DK_logL$LL
    
    # Check convergence
    cLL <- (LL - LL_prev) / (abs(LL) / 2 + abs(LL_prev) / 2) 
    if (iter>1 && cLL<1e-03) {
      break
    }
    LL_prev <- LL 
    
    # M-Step
    
    # A & Omeg
    params <-  f_Mstep_A_Omeg(stT, PtT, params, options) 
    
    # lambdaMx
    params <-  f_Mstep_lambdaMx(stT, PtT, data, params, options) 
 
    # rhoMx & SigMx
    params <-  f_Mstep_rhoMx_SigMx(stT, PtT, data, params, options)
    
    if (options$Nq_flow>0) {
      
      # lambdaQ_flow
      params <-  f_Mstep_lambdaQ_flow(stT,PtT,data,params,options)
      
      # rhoQ_flow & SigQ_flow
      params <-  f_Mstep_rhoQ_flow_SigQ_flow_v2(data,stT,PtT,params,options)
    }
    
    if (options$Nmz>0) { 
      #?# not checked with Nmz <> 0
      
      # lambdaMz
      params <-  f_Mstep_lambdaMz(stT,PtT,data,params,options) 
      
      # rhoMz & SigMz
      params <-  f_Mstep_rhoMz_SigMz(stT,PtT,data,params,options) 
    }
    
  }
  
  if (iter == maxiter) {
    cat("###------------------------------------------------### \n")
    cat("###  EM algorithm failed to converge after ", iter, " iterations   ### \n")
    cat("###------------------------------------------------### \n")
  } else {
    cat("###------------------------------------------------### \n")
    cat("###  EM algorithm converged after ", iter, " iterations   ### \n")
    cat("###------------------------------------------------### \n")
  }
  
  return(params)
}
  
  
  
  
  
  