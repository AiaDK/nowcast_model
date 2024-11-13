# 
f_load_lkw_maut_index <- function(data_BuBaRTD, vintage, dir_rawdata) {
  
  # Load vintages
  dir <- paste0(dir_rawdata, "/lkw_maut_index/")
  tmp <- read.csv(paste0(dir, "vintages_lkwmautindex.csv"), sep = ",", check.names = FALSE)
  
  # Extract dates
  tmpdates <- tmp[, 1]
  firstdate <- as.numeric(substr(tmpdates[1], 1, 4)) + as.numeric(substr(tmpdates[1], 6, 7))/12
  dates <- firstdate + (0:(nrow(tmp)-1))/12
  
  # Load date of vintages and find column corresponding to current vintage
  tmpvintages <- as.Date(colnames(tmp[,-1]))
  index_col <- sum(tmpvintages <= as.Date(vintage, "%d-%b-%Y"))
  
  # Removing missings from data!
  tmpdata <- tmp[, index_col + 1] 
  dates <- dates[!is.na(tmpdata)]
  tmpdata <- tmpdata[!is.na(tmpdata)]
  
  # Extract data, matching size (i.e. dates) os data_BuBaRTD$rawdata and data_BuBaRTD.data
  rawdata <- matrix(NA, nrow(data_BuBaRTD$rawdata), 1)
  data <- matrix(NA, nrow(rawdata), ncol(rawdata)) 
  
  i_start <- which((abs(data_BuBaRTD$dates - dates[1]) < 1e-05) == TRUE)
  i_end <- which((abs(data_BuBaRTD$dates - dates[length(dates)]) < 1e-05) == TRUE)
  ind_production <- which(data_BuBaRTD$groups == "production")[length(which(data_BuBaRTD$groups == "production"))]
  
  
  if (is_empty(i_end)) { # extend sample by 1 month     
    rawdata <- c(rep(NA, i_start-1), tmpdata) 
    data <- c(rep(NA, i_start-1), NA, diff(log(tmpdata))*100) 
    
    data_BuBaRTD$rawdata <- cbind(rbind(data_BuBaRTD$rawdata[, 1:ind_production], rep(NA, ncol(data_BuBaRTD$rawdata[, 1:ind_production]))), 
                                          rawdata, 
                                          rbind(data_BuBaRTD$rawdata[, (ind_production+1):ncol(data_BuBaRTD$rawdata)], rep(NA, ncol(data_BuBaRTD$rawdata[, (ind_production+1):ncol(data_BuBaRTD$rawdata)])))) 
    
    data_BuBaRTD$data <- cbind(rbind(data_BuBaRTD$data[, 1:ind_production], rep(NA, ncol(data_BuBaRTD$data[, 1:ind_production]))), 
                                       data, 
                                       rbind(data_BuBaRTD$data[, (ind_production+1):ncol(data_BuBaRTD$data)], rep(NA, ncol(data_BuBaRTD$data[, (ind_production+1):ncol(data_BuBaRTD$data)])))) 
    
    data_BuBaRTD$dates <- c(data_BuBaRTD$dates, tail(data_BuBaRTD$dates, 1)+1/12) 
    
  } else {
    rawdata[i_start:i_end] <- tmpdata
    data[i_start:i_end] <- c(NA, diff(log(tmpdata))*100) 
    
    data_BuBaRTD$rawdata <- cbind(data_BuBaRTD$rawdata[, 1:ind_production], 
                                          rawdata, 
                                          data_BuBaRTD$rawdata[, (ind_production+1):ncol(data_BuBaRTD$rawdata)])
    
    data_BuBaRTD$data <- cbind(data_BuBaRTD$data[, 1:ind_production], 
                                       data, 
                                       data_BuBaRTD$data[, (ind_production+1):ncol(data_BuBaRTD$data)])
  }
  
  # Merge rest with BuBa structure
  data_BuBaRTD$groups <- c(data_BuBaRTD$groups[1:ind_production], 
                                   "production", 
                                   data_BuBaRTD$groups[(ind_production+1):length(data_BuBaRTD$groups)])
  
  data_BuBaRTD$names <- c(data_BuBaRTD$names[1:ind_production], 
                                  "lkw_maut", 
                                  data_BuBaRTD$names[(ind_production+1):length(data_BuBaRTD$names)])
  
  data_BuBaRTD$flag_usestartvals <- c(data_BuBaRTD$flag_usestartvals[1:ind_production], 
                                              1, 
                                              data_BuBaRTD$flag_usestartvals[(ind_production+1):length(data_BuBaRTD$flag_usestartvals)])
  
  data_BuBaRTD$seriesnames <- c(data_BuBaRTD$seriesnames[1:ind_production], 
                                        "", 
                                        data_BuBaRTD$seriesnames[(ind_production+1):length(data_BuBaRTD$seriesnames)])
  
  data_BuBaRTD$type <- c(data_BuBaRTD$type[1:ind_production], 
                                 "m", 
                                 data_BuBaRTD$type[(ind_production+1):length(data_BuBaRTD$type)])
  
  data_BuBaRTD$trafo <- c(data_BuBaRTD$trafo[1:ind_production], 
                                  3, 
                                  data_BuBaRTD$trafo[(ind_production+1):length(data_BuBaRTD$trafo)])
  
  data_BuBaRTD$flag_sa <- c(data_BuBaRTD$flag_sa[1:ind_production], 
                                    0, 
                                    data_BuBaRTD$flag_sa[(ind_production+1):length(data_BuBaRTD$flag_sa)])
  
  
  return(data_BuBaRTD) 
}