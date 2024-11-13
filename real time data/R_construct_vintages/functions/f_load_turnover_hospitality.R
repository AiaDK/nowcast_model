# 
f_load_turnover_hospitality <- function(data_BuBaRTD, vintage, dir_rawdata) {
  
  # Load vintages
  dir <- paste0(dir_rawdata, "/umsatz_gastgewerbe/")
  tmp <- read.csv(paste0(dir, "vintages_gastgewerbe.csv"), sep = ",", check.names = FALSE)
  
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
  
  # Extract data, matching size (i.e. dates) as data_BuBaRTD$rawdata and data_BuBaRTD.data
  rawdata <- matrix(NA, nrow(data_BuBaRTD$rawdata), 1)
  data <- matrix(NA, nrow(rawdata), ncol(rawdata)) 

  i_start <- which((abs(data_BuBaRTD$dates - dates[1]) < 1e-05) == TRUE)
  i_end <- which((abs(data_BuBaRTD$dates - dates[length(dates)]) < 1e-05) == TRUE)
  rawdata[i_start:i_end] <- tmpdata
  data[i_start:i_end] <- c(NA, diff(log(tmpdata))*100) 
  
  # Merge with BuBa structure
  ind_turnover <- which(data_BuBaRTD$groups == "turnover")[length(which(data_BuBaRTD$groups == "turnover"))]
  
  data_BuBaRTD$rawdata <- cbind(data_BuBaRTD$rawdata[, 1:ind_turnover], 
                                rawdata, 
                                data_BuBaRTD$rawdata[, (ind_turnover+1):ncol(data_BuBaRTD$rawdata)])
  
  data_BuBaRTD$data <- cbind(data_BuBaRTD$data[, 1:ind_turnover], 
                             data, 
                             data_BuBaRTD$data[, (ind_turnover+1):ncol(data_BuBaRTD$data)])
  
  data_BuBaRTD$groups <- c(data_BuBaRTD$groups[1:ind_turnover], 
                               "turnover", 
                               data_BuBaRTD$groups[(ind_turnover+1):length(data_BuBaRTD$groups)])
  
  data_BuBaRTD$names <- c(data_BuBaRTD$names[1:ind_turnover], 
                              "hospitality", 
                              data_BuBaRTD$names[(ind_turnover+1):length(data_BuBaRTD$names)])
  
  data_BuBaRTD$flag_usestartvals <- c(data_BuBaRTD$flag_usestartvals[1:ind_turnover], 
                                          1, 
                                          data_BuBaRTD$flag_usestartvals[(ind_turnover+1):length(data_BuBaRTD$flag_usestartvals)])
  
  data_BuBaRTD$seriesnames <- c(data_BuBaRTD$seriesnames[1:ind_turnover], 
                                    "", 
                                    data_BuBaRTD$seriesnames[(ind_turnover+1):length(data_BuBaRTD$seriesnames)])
  
  data_BuBaRTD$type <- c(data_BuBaRTD$type[1:ind_turnover], 
                             "m", 
                             data_BuBaRTD$type[(ind_turnover+1):length(data_BuBaRTD$type)])
  
  data_BuBaRTD$trafo <- c(data_BuBaRTD$trafo[1:ind_turnover], 
                              3, 
                              data_BuBaRTD$trafo[(ind_turnover+1):length(data_BuBaRTD$trafo)])
  
  data_BuBaRTD$flag_sa <- c(data_BuBaRTD$flag_sa[1:ind_turnover], 
                                0, 
                                data_BuBaRTD$flag_sa[(ind_turnover+1):length(data_BuBaRTD$flag_sa)])
                                                                     
  
  return(data_BuBaRTD) 
}
