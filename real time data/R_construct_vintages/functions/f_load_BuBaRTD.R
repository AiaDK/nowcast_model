
f_load_BuBaRTD <- function(vintage, dir_rawdata) {
  
  # Add path for functions and data files
  file.path(dir_rawdata, "BuBa RTD")
  file.path(dir_rawdata, "BuBa RTD", "production")
  file.path(dir_rawdata, "BuBa RTD", "orders")
  file.path(dir_rawdata, "BuBa RTD", "turnover")
  file.path(dir_rawdata, "BuBa RTD", "prices")
  file.path(dir_rawdata, "BuBa RTD", "labor market")
  file.path(dir_rawdata, "BuBa RTD", "national accounts")
  
  # Load options like names, groups, trafos and so on
  data_BuBaRTD <- f_loadoptions_BuBaRTD()
  
  # Loop over vars
  firstdate_master <- 1991 + 1/12
  data_BuBaRTD$rawdata = list() 
  flag_novintage = matrix(0,  1, length(data_BuBaRTD$names))
  
  # Create lists to store data
  data <- list()
  dates <- list()
  
  for (n in 1:length(data_BuBaRTD$names)) {
    
    # load data and dates
    if (file.exists(paste0(dir_rawdata, "/BuBa RTD/", data_BuBaRTD$groups[n], "/", data_BuBaRTD$seriesnames[n], "_cur.csv"))) {
      temp <- as.data.frame(read.csv(paste0(dir_rawdata, "/BuBa RTD/", data_BuBaRTD$groups[n], "/", data_BuBaRTD$seriesnames[n], "_cur.csv"), sep = ",", check.names = FALSE)) 
    } else {
      temp <- as.data.frame(read.csv(paste0(dir_rawdata, "/BuBa RTD/", data_BuBaRTD$groups[n], "/", data_BuBaRTD$seriesnames[n], ".csv"), sep = ",", check.names = FALSE))
    }
    
    num <- temp[,-1]
    tempvintages <- colnames(num)
    datavintages <- as.Date(tempvintages)
    firstdate <- as.numeric(substr(temp[1,1], 1, 4)) + as.numeric(substr(temp[1,1], 6, 8))/12
    
    # Get column index corresponding to latest available vintage
    index_col <- sum(datavintages <= as.Date(vintage, "%d-%b-%Y"))
    
    # Store data
    if (index_col > 0) {
      if (data_BuBaRTD$type[n] == "m") {
        data[[n]] <- num[, index_col]
        dates[[n]] <- firstdate + (0:(nrow(num)-1))/12
      } else if (data_BuBaRTD$type[n] == "q:E") {
        data[[n]] <- kronecker(num[, index_col], c(NA, NA, 1))
        dates[[n]] <- firstdate + (0:(nrow(num)*3-1))/12
      }
    } else {
      flag_novintage[1,n] <- 1 
    }
    enddates <- sapply(dates, tail, 1) 
  }
  rm(temp, tempvintages)
  
  lastenddate <- max(enddates)
  rawdata <- list()
  
  for (n in 1:length(data_BuBaRTD$names)){
    tempdata <- data[[n]]
    tempdates <- dates[[n]]
    missingsstart <- c() 
    index_start <- 1

    if (!is_empty(tempdates)){
      if (tempdates[1] < firstdate_master) { # series start before master date, 1991-01
        index_start <- which((abs(tempdates - firstdate_master) < 1e-05) == TRUE)
      }
      else if (tempdates[1] > firstdate_master) { # insert missing until series actually starts
        missingsstart <- rep(NA, round((tempdates[1]-firstdate_master)*12))
      }
      missingsend <-  c() 
      if (tempdates[length(tempdates)] < lastenddate){
        missingsend <- rep(NA, round((lastenddate-tempdates[length(tempdates)])*12))
      }
      rawdata[[n]] <- c(missingsstart, tempdata[index_start:length(tempdata)], missingsend)
    }
    rawdata <- as.data.frame(rawdata, fix.empty.names = FALSE)
  }
  
  # Adjust names, groups, trafos, etc. for missing vintages
  data_BuBaRTD$rawdata <- rawdata[, !flag_novintage]
  data_BuBaRTD$names_novintage <- data_BuBaRTD$names[flag_novintage == 1]
  data_BuBaRTD$names <- data_BuBaRTD$names[!flag_novintage]
  data_BuBaRTD$groups <- data_BuBaRTD$groups[!flag_novintage]
  data_BuBaRTD$trafo <- data_BuBaRTD$trafo[!flag_novintage]
  data_BuBaRTD$type <- data_BuBaRTD$type[!flag_novintage]
  data_BuBaRTD$flag_sa <- data_BuBaRTD$flag_sa[!flag_novintage]
  data_BuBaRTD$flag_usestartvals <- data_BuBaRTD$flag_usestartvals[!flag_novintage]

  # Remove all NaN rows
  data_BuBaRTD$rawdata <- data_BuBaRTD$rawdata[!apply(is.na(data_BuBaRTD$rawdata), 1, all), ]
  data_BuBaRTD$dates <- firstdate_master + (0:(nrow(data_BuBaRTD$rawdata)-1))/12

  # Transform (and seasonally adjust if needed)
  data_trafo <- matrix(NA, nrow(data_BuBaRTD$rawdata), ncol(data_BuBaRTD$rawdata))
  flag_dontuse <- matrix(0, 1, ncol(data_BuBaRTD$rawdata))

  for (n in 1:ncol(data_BuBaRTD$rawdata)) {
    temp <- data_BuBaRTD$rawdata[, n]
    if (data_BuBaRTD$type[[n]] == "m") {
      switch(data_BuBaRTD$trafo[n],
             "1" = data_trafo[, n] <- temp,
             "2" = data_trafo[, n] <- c(NA, diff(temp)),
             "3" = data_trafo[, n] <- c(NA, 100 * diff(log(temp))) 
      )
    } else {
      switch(data_BuBaRTD$trafo[n],
             "1" = data_trafo[, n] <- temp,
             "2" = data_trafo[, n] <- c(rep(NA, 3), temp[4:length(temp)] - temp[1:(length(temp) - 3)]),
             "3" = data_trafo[, n] <- c(rep(NA, 3), 100 * (log(temp[4:length(temp)]) - log(temp[1:(length(temp) - 3)]))) 
      )
    }
  }
  
  names(data_BuBaRTD$rawdata) <- NULL
  data_BuBaRTD$data <- data_trafo

  return(data_BuBaRTD)
}
