

# Function to estimate models with different combinations of Nr, Np, and Nj
f_estimatemodels <- function(samplestart, vintage, Nrs, Nps, Njs, list_removevars, dirname, dir_data) {
  
  # Load first vintage of data

  constructdataset_data <- f_constructdataset(dir_data, samplestart, vintage, list_removevars, c(), c()) 
  dataM_stand <- constructdataset_data$dataM_stand
  dataQ_stand <- constructdataset_data$dataQ_stand
  flag_usestartvals <- constructdataset_data$flag_usestartvals
  
  # Set options
  options <- list(flag_usestartvals = flag_usestartvals, restrOmeg = 0)
  
  for (Nr in Nrs) {
    for (Np in Nps) {
      for (Nj in Njs) {

        options$Nr <- Nr
        options$Np <- Np
        options$Nj <- Nj
        
        # Estimate model with the current combination of Nr, Np, and Nj
        params <- f_EMalgorithm(dataM_stand, dataQ_stand, options)
        
        # Save params as .Rdata file
        save(params, file = paste0(dirname, "/params/params_Nr", Nr, "_Np", Np, "_Nj", Nj, ".RData"))
        
      }
    }
  }
  
  return()
}
