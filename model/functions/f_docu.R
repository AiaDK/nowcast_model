#
f_docu <- function(nowcast_new, vintages, details, options, savename, flag_ewpool, threshold, dirname, str_cast) {
  
  fid <- file(paste0(dirname, "/docu/", savename, ".txt"), "w")
  
  writeLines("#################################################################", fid)
  writeLines("#################################################################", fid)
  writeLines("####### MODEL SPECIFICATION #####################################", fid)
  
  if (flag_ewpool == 1) {
    writeLines("####### equal-weight pool #######################################", fid)
  } else {
    writeLines(paste0("####### Nr = ", options$Nr, " ##################################################"), fid)
    writeLines(paste0("####### Np = ", options$Np, " ##################################################"), fid)
    writeLines(paste0("####### Nj = ", options$Nj, " ##################################################"), fid)
  }
  writeLines("#################################################################", fid)
  
  for (v in 2:length(nowcast_new)) {
    writeLines("#################################################################", fid)
    writeLines("#---------------------------------------------------------------#", fid)
    writeLines("#-", fid)
    writeLines(paste0("#- On ", vintages[v], ", the nowcast for ", str_cast, " was ", round(nowcast_new[v], 2), " percent. "), fid)
    writeLines("#-", fid)
    writeLines(paste0("#- Previous nowcast was ", round(nowcast_new[v-1], 2), " percent. "), fid)
    writeLines("#-", fid)
    writeLines(paste0("#- Variables with an absolute impact larger than ", threshold, " were..."), fid)
    writeLines("#-", fid)
    
    # Top movers
    
    index_sort <- order(abs(details[[v]]$impacts), decreasing = TRUE)
    impacts_sorted <- round(details[[v]]$impacts[index_sort], 2)
    names_sorted <- details[[v]]$varnames[index_sort]
    actuals_sorted <- round(details[[v]]$actuals[index_sort], 1)
    forecasts_sorted <- round(details[[v]]$forecasts[index_sort], 1)
    weights_sorted <- round(details[[v]]$weights[index_sort], 4)
    
    if (sum(abs(impacts_sorted) > threshold) == 0) {
      temp <- "# ...none!"
      writeLines("#################################################################", fid)
    } else {
      for (i in 1:sum(abs(impacts_sorted) > threshold)) {
        writeLines(paste0("#-    ", names_sorted[i]), fid)
        writeLines(paste0("#     forecast: ", sprintf("%.1f", forecasts_sorted[i]), " actual: ", sprintf("%.1f", actuals_sorted[i]), " weight: ", sprintf("%.4f", weights_sorted[i]), " impact: ", sprintf("%.2f", impacts_sorted[i])), fid)
        writeLines("#-", fid)
      }
    }
  }

  writeLines("#################################################################", fid)
  close(fid)
  
}