
#?# ?change dates using zoo library


f_load_ifo <- function(vintage, dir_rawdata) {
  
  # Add path data files
  dir_name <- paste0(dir_rawdata, "/ifo/")
  
  # Ifo options 
  data_ifo <- list(c("ifo_lage", "ifo_erwartung"), c("ifo", "ifo"), c(2, 2), c("m", "m"), c(1, 1), c(0, 0))
  names(data_ifo) <- c("names", "groups", "trafo", "type", "flag_usestartvals", "flag_sa")
  
  # Read in data and select vars 
  ifo_xlsx <- read.xlsx(paste0(dir_name, "ifo_current.xlsx"), sheetName = "Wirtschaftsbereiche", header = FALSE)
  row_offset <- nrow(ifo_xlsx) - (nrow(ifo_xlsx)-9) 
  col_offset <- ncol(ifo_xlsx) - (ncol(ifo_xlsx)-1) 
  data_ifo_xlsx <- ifo_xlsx[-(1:row_offset), ]
  dates_str <- as.Date(as.yearmon(data_ifo_xlsx[, 1], "%m/%Y"), frac = 1)
  dates <- as.numeric(substr(data_ifo_xlsx[1,1], 5, 8)) + as.numeric(substr(data_ifo_xlsx[1,1], 1, 3))/12 + 0:(length(data_ifo_xlsx[,1])-1)/12
  ind_vars <- c(3, 4) 
  
  # Determine available observations according to vintage 
  releasedates <- read.csv(paste0(dir_name, "releasedates_ifo_csv.csv"), sep = ";")
  index_row <- get_row_index_vintage(dates_str, releasedates, vintage, "m") 
  
  data_ifo$rawdata <- cbind(as.numeric(data_ifo_xlsx[1:index_row, ind_vars[1]]), as.numeric(data_ifo_xlsx[1:index_row, ind_vars[2]]))  
  data_ifo$dates <- dates[1:index_row]
  
  # Transform 
  data_trafo <- matrix(NA, nrow(data_ifo$rawdata), ncol(data_ifo$rawdata))
  flag_dontuse <- matrix(0, 1, ncol(data_ifo$rawdata))
  
  for (n in 1:ncol(data_ifo$rawdata)) {
    switch(data_ifo$trafo[n],
           "1" = data_trafo[,n] <- data_ifo$rawdata[,n],
           "2" = data_trafo[,n] <- c(NA, diff(data_ifo$rawdata[,n])),
           "3" = data_trafo[,n] <- c(NA, 100*diff(log(data_ifo$rawdata[,n]))) 
    )
  }
  
  data_ifo$data <- data_trafo
  
  # Last checks 
  if (!all(flag_dontuse == 0)) {
    data_ifo$namesremoved <- data_ifo$names[flag_dontuse == 1]  
    data_ifo$data <- data_ifo$data[, !flag_dontuse]
    data_ifo$rawdata <- data_ifo$rawdata[, !flag_dontuse]
    data_ifo$trafo <- data_ifo$trafo[, !flag_dontuse]
    data_ifo$names <- data_ifo$names[, !flag_dontuse]
    data_ifo$groups <- data_ifo$groups[, !flag_dontuse]
    data_ifo$type <- data_ifo$type[, !flag_dontuse]
    data_ifo$flag_usestartvals <- data_ifo$flag_usestartvals[, !flag_dontuse]
    data_ifo$flag_sa <- data_ifo$flag_sa[, !flag_dontuse]
  }
  
  return(data_ifo)
}
  
  
  