
f_load_financial <- function(vintage, dir_rawdata) {
  
  # Add path for functions and data files
  dirname <- paste0(dir_rawdata, "/Finanzmarktdaten/")
  
  # Load options like names, groups, trafos and so on
  data_financial <- f_loadoptions_financialdataBuBa()
  
  # Manually set first date
  firstdatenum_master <- 1991 + 1/12 # January 1991
  
  # Monthly data
  data_financial$rawdata <- data.frame()

  for (n in 1:length(data_financial$seriesnames)) {
    temp <- as.data.frame(read.csv(paste0(dirname, data_financial$seriesnames[n], ".csv"), sep = ",", check.names = FALSE)) 
    temp_data <- temp[-(1:2)] 
    temp_data[temp_data == "NaN"] <- NA
    numnans <- nrow(data_financial$rawdata) - nrow(temp_data)
    if (numnans <= 0) {
      data_financial$rawdata <- as.data.frame(append(data_financial$rawdata,  temp_data))
    } else {
      data_financial$rawdata <- as.data.frame(cbind(data_financial$rawdata, c(rep(NA, numnans), unlist(temp_data))))  
    }
  }
  
  # Adjust dates
  data_financial$dates <- firstdatenum_master + (0:(nrow(data_financial$rawdata)-1))/12
  
  # Pseudo real-time adjustment
  month_v <- as.numeric(format(as.Date(vintage, "%d-%b-%Y"), format="%m"))
  year_v <- as.numeric(format(as.Date(vintage, "%d-%b-%Y"), format="%Y"))
  if (is_empty(which((abs(year_v + month_v / 12 - data_financial$dates) < 1e-05) == TRUE))) {
    ind_end <- nrow(data_financial$rawdata)
  } else {
    ind_end <- which((abs(year_v + month_v / 12 - data_financial$dates) < 1e-05) == TRUE)-1
  }
  
  data_financial$rawdata <- data_financial$rawdata[1:ind_end, ]
  data_financial$dates <- data_financial$dates[1:ind_end]
  
  names(data_financial$rawdata) <- NULL 
  
  # Transform (and seasonally adjust if needed)
  data_trafo <- matrix(NA, nrow(data_financial$rawdata), ncol(data_financial$rawdata)) 
  flag_dontuse <- matrix(0, 1, ncol(data_financial$rawdata)) 
  
  for (n in 1:ncol(data_financial$rawdata)) {
    # Seasonal adjustment? 
    if (data_financial$flag_sa[n] == 1) {
      # Check if we have enough observations (> 5 years)
        #?# until the 2nd else not checked
      if (sum(!is.na(data_financial$rawdata[,n])) > 60) {
        temp <- f_sa(data_financial$rawdata[,n]) #?# where does f_sa comes from??
      } else {
        # Exclude from the analysis
        flag_dontuse[n] <- 1 
      }
    } else {
      temp <- data_financial$rawdata[,n] 
    }
    
    if (data_financial$type[n] == "m") {        
      switch (data_financial$trafo[n],
              "1" = data_trafo[,n] <- temp,
              "2" = data_trafo[,n] <- c(NA, diff(temp)),
              "3" = data_trafo[,n] <- c(NA, 100 * diff(log(temp)))
      ) 
    } else {
      # quarterly vars
      switch (data_financial$trafo[n],
              "1" = data_trafo[,n] <- temp,
              "2" = data_trafo[,n] <- c(rep(NA, 3), temp[4:length(temp)] - temp[1:(length(temp) - 3)]), 
              "3" = data_trafo[,n] <- c(rep(NA, 3), 100 * (log(temp[4:length(temp)]) - log(temp[1:(length(temp) - 3)])))
      ) 
    }
  }
  
  data_financial$data <- data_trafo 
  
  # Remove series that could not be seasonally adjusted
  if (!all(flag_dontuse == 0)){
    data_financial$namesremoved <- data_financial$names[flag_dontuse == 1]
    data_financial$data <- data_financial$data[, !flag_dontuse]
    data_financial$rawdata <- data_financial$rawdata[, !flag_dontuse]
    data_financial$trafo <- data_financial$trafo[, !flag_dontuse]
    data_financial$names <- data_financial$names[, !flag_dontuse]
    data_financial$groups <- data_financial$groups[, !flag_dontuse]
    data_financial$type <- data_financial$type[, !flag_dontuse]
    data_financial$flag_usestartvals <- data_financial$flag_usestartvals[, !flag_dontuse]
    data_financial$flag_sa <- data_financial$flag_sa[, !flag_dontuse]
  }
  
  
  return(data_financial)
}
