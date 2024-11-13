

f_newsdecomp_v2 <- function(js, tjs, tks, data_new, ks_output_old, params, options) {
  # this function computes the news in a different way!
  # First, given the unrevised data and new observations, compute the news. 
  # Then, run the Kalman smoother on the new data to 
  # get the new forecast which includes revisions!!!!!!
  
  # News decomposition
  
  # State space parameters
  statespaceparams_news <- f_statespaceparams_news(params, options)
  T <- statespaceparams_news$T
  Z <- statespaceparams_news$Z
  R <- statespaceparams_news$R
  Q <- statespaceparams_news$Q
  H <- statespaceparams_news$H
  
  # Empty mats to store results
  E_II <- matrix(NA, length(js), length(js))
  E_ykI <- matrix(NA, 1, length(js))
  news <- matrix(NA, length(js), 1)
  actuals <- matrix(NA, length(js), 1)
  forecasts <- matrix(NA, length(js), 1)
  
  # loop over js
  for (j in 1:length(js)) {
    x_fore <- Z[js[j], , drop=FALSE] %*% ks_output_old$stT[, tjs[j], drop=FALSE]
    x_actual <- data_new[js[j], tjs[j], drop=FALSE]
    news[j] <- x_actual - x_fore
    actuals[j] <- x_actual
    forecasts[j] <- x_fore
    
    # second loop over js
    for (l in 1:length(js)) {
      if (tjs[j] < tjs[l]) {
        covarPtT <- ks_output_old$P[,,tjs[j]]
        for (t in tjs[j]:(tjs[l]-1)) { 
          covarPtT <- covarPtT %*% t(ks_output_old$L[, , t])
        }
        covarPtT <- covarPtT %*% (diag(1, options$Ns) - ks_output_old$N[,,tjs[l]] %*% ks_output_old$P[, , tjs[l]])
        E_II[j, l] <- Z[js[j], , drop=FALSE] %*% covarPtT %*% t(Z[js[l], , drop=FALSE]) + H[js[j], js[l], drop=FALSE]
        
      } else {
        covarPtT <- ks_output_old$P[, , tjs[l]]
        #?# check this loop with next
        for (t in tjs[l]:(tjs[j]-1)) {
          if (tjs[l] > (tjs[j]-1)){
            next
          } else {
            covarPtT <- covarPtT %*% t(ks_output_old$L[, , t])
          }
        }
        covarPtT <- covarPtT %*% (diag(1, options$Ns) - ks_output_old$N[, , tjs[j]] %*% ks_output_old$P[, , tjs[j]])
        E_II[j, l] <- Z[js[j], , drop=FALSE] %*% t(covarPtT) %*% t(Z[js[l], , drop=FALSE]) + H[js[j], js[l], drop=FALSE]
      }
    }
    
    if (tks < tjs[j]) {
      covarPtT <- ks_output_old$P[,,tks]
      for (t in tks:(tjs[j]-1)) {
        if (tks > (tjs[j]-1)) {
          next
        } else {
          covarPtT <- covarPtT %*% t(ks_output_old$L[, , t])
        }
      }
      covarPtT <- covarPtT %*% (diag(1, options$Ns) - ks_output_old$N[,,tjs[j]] %*% ks_output_old$P[,,tjs[j]])
      E_ykI[j] <- Z[options$index_gdp, , drop=FALSE] %*% covarPtT %*% t(Z[js[j], 1:options$Ns, drop=FALSE])
      
    } else {
      covarPtT <- ks_output_old$P[,,tjs[j]]
      for (t in tjs[j]:(tks-1)) { 
        covarPtT <- covarPtT %*% t(ks_output_old$L[, , t])
      }
      covarPtT <- covarPtT %*% (diag(1, options$Ns) - ks_output_old$N[, , tks] %*% ks_output_old$P[, , tks])
      E_ykI[j] <- Z[options$index_gdp, , drop=FALSE] %*% t(covarPtT) %*% t(Z[js[j], , drop=FALSE])
    }
  }
  

  # Weights and impact
  weights <- E_ykI %*% solve(E_II) 
  impacts <- t(weights) * news 
  impacts_restand <- options$stdgdp * impacts 
  
  # Store names and also re-standardize actuals, forecasts & weights
  varnames <- matrix(NA, length(js), 1)
  actuals_restand <- matrix(NA, length(js), 1)
  forecasts_restand <- matrix(NA, length(js), 1)
  weights_restand <- matrix(NA, length(js), 1)
  for (j in 1:length(js)) {
    varnames[j, 1] = paste(options$names[[js[j]]], ' (', options$groups[js[j]], ')', sep = "") 
    actuals_restand[j, 1] = actuals[j] * options$stds[js[j]] + options$means[js[j]] 
    forecasts_restand[j, 1] = forecasts[j] * options$stds[js[j]] + options$means[js[j]]
    weights_restand[j, 1] = weights[j] * options$stds[options$index_gdp] / options$stds[js[j]]
  }
  
  # Impact by groups
  varindex <- js
  temp <- options$groups[varindex]
  impact_by_group <- matrix(NA, length(options$groupnames), 1)
  for (g in 1:length(options$groupnames)) {     
    indexgroup <- temp == options$groupnames[[g]] 
    impact_by_group[g, 1] = sum(impacts_restand[indexgroup]) 
  }
  
  return(list(impact_by_group = impact_by_group, 
              impacts_restand = impacts_restand, 
              actuals_restand = actuals_restand, 
              forecasts_restand = forecasts_restand, 
              weights_restand = weights_restand, 
              varnames = varnames))
  
}
