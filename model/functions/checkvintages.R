# Function checking whether all vintages datasets exist

checkvintages <- function(vintages, dir_data) {
  
  dirname <- paste0(dir_data, "/vintages/")
  
  flag_missing <- rep(0, length(vintages))
  
  for (v in 1:length(vintages)) {
    
    if (file.exists(paste0(dirname, "dataset_", format(as.Date(vintages[v], "%d-%b-%Y"), format = "%Y_%m_%d"), ".Rdata"))) {
      # If exists, do nothing
    } else {
      message(paste0(format(as.Date(vintages[v], "%d-%b-%Y"), format = "%Y_%m_%d"), ".Rdata"), " is missing!")
      flag_missing[v] <- 1
    }
  }
  
  return(flag_missing)
}