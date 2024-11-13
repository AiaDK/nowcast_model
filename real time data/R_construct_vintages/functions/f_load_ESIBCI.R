
f_load_ESIBCI <- function(vintage, dir_rawdata) {
  
  # Add path for data files 
  dirname <- paste0(dir_rawdata, "/ESI BCI/")
  
  # Load options, names, groups, trafos, etc. 
  data_ESIBCI <- f_loadoptions_ESIBCI()
  data_ESIBCI$groups <- rep("ESI", length(data_ESIBCI$groups)) 
  
  # Load release dates
  releasedates_alt <- read.xlsx(paste0(dirname, "releasedates_ESIBCI.xlsx"), 1)
  releasedates <- read.csv(paste0(dirname, "releasedates_ESIBCI_csv.csv"), sep=";")
  
  # Loop over different categories (industry, retail, services, etc.) 
  filenames <- c("industry", "retail", "building", "services", "consumer")
  sheetnames <- c("INDUSTRY", "RETAIL TRADE", "BUILDING", "SERVICES", "CONSUMER")
  data_ESIBCI$rawdata <- list() 
  
  for (f in 1:length(filenames)) {
    
    # Monthly data
    
    # Load xlsx 
      #?# check: reads 1026 variables for 'services'
    ESIBCI_xlsx_m <- as.data.frame(read_excel(paste0(dirname, filenames[f], "_total_sa_nace2.xlsx"), sheet = paste0(sheetnames[f], " MONTHLY"))) 
    
    # Get dates 
    dates_str <- as.Date(ESIBCI_xlsx_m[, 1])
    dates <- as.numeric(format(ESIBCI_xlsx_m[1, 1], "%Y")) + as.numeric(format(ESIBCI_xlsx_m[1, 1], "%m"))/12 + 0:(length(ESIBCI_xlsx_m[,1])-1)/12

    # Get column indices of variables
    index_monthly <- data_ESIBCI$type == "m"
    index_cols <- match(intersect(colnames(ESIBCI_xlsx_m), data_ESIBCI$seriesnames[index_monthly]), colnames(ESIBCI_xlsx_m))
    
    # Find end of available observations according to vintage 
    index_row <- get_row_index_vintage(dates_str, releasedates, vintage, "m")
    
    # Get data 
    data_ESIBCI$rawdata <- as.data.frame(append(data_ESIBCI$rawdata, ESIBCI_xlsx_m[1:index_row, index_cols]))
    
    
    # Quarterly data 
    
    if(filenames[f] == "industry" || filenames[f] == "services"){
      
      ESIBCI_xlsx_q <- as.data.frame(read_excel(paste0(dirname, filenames[f], "_total_sa_nace2.xlsx"), sheet = paste0(sheetnames[f], " QUARTERLY")))
      
      # Get column indices of variables
      index_quarterly <- !(data_ESIBCI$type == "m") 
      index_cols <- match(intersect(colnames(ESIBCI_xlsx_q), data_ESIBCI$seriesnames[index_quarterly]), colnames(ESIBCI_xlsx_q))
      
      # Find end of available observations according to vintage
      index_row <- get_row_index_vintage(ESIBCI_xlsx_q[,1], releasedates, vintage, "q") 
      
      # Get data 
      if (!is_empty(index_cols)) {
        #! changed to as.numeric(ESIBCI__)
        datatemp <- as.data.frame(cbind(kronecker(as.numeric(ESIBCI_xlsx_q[1:index_row, index_cols[1]]), c(1, NA, NA)), kronecker(as.numeric(ESIBCI_xlsx_q[1:index_row, index_cols[2]]), c(1, NA, NA))))
      } else {
        next
      }

      # Adjust number of rows to match monthly data
      if (nrow(datatemp) > nrow(data_ESIBCI$rawdata)) {
        datatemp <- datatemp[1:nrow(data_ESIBCI$rawdata), ]
      } else if(nrow(datatemp) < nrow(data_ESIBCI$rawdata)) {
        datatemp <- rbind(datatemp, matrix(NA, abs(nrow(data_ESIBCI$rawdata)-nrow(datatemp)), ncol(datatemp)))
      }
      
      data_ESIBCI$rawdata <- append(data_ESIBCI$rawdata, datatemp)
    }
  }
  
  # Transform all elements of the data_ESIBCI$rawdata list into numeric and turn into a dataframe
  for (i in 1:length(data_ESIBCI$rawdata)) {  
    data_ESIBCI$rawdata[i] <- lapply(data_ESIBCI$rawdata[i], as.numeric)
  }
  data_ESIBCI$rawdata <- as.data.frame(data_ESIBCI$rawdata) 
  names(data_ESIBCI$rawdata) <- NULL 
  
  data_ESIBCI$dates <- dates[1:nrow(data_ESIBCI$rawdata)]
  
  # Manually adjust data which are repeated quarterly values before Feb 2001
  index_Jan2001 <- which((abs(dates - (2001 + 1/12)) < 1e-05) == TRUE)
  index_series <- (data_ESIBCI$groups == "ESI: services") & (data_ESIBCI$names %in% c("Confidence Indicator",
                                                                                      "Business situation development over the past 3 months",
                                                                                      "Evolution of the demand over the past 3 months",
                                                                                      "Expectation of the demand over the next 3 months",
                                                                                      "Evolution of the employment over the past 3 months"))
  data_ESIBCI$rawdata[1:index_Jan2001, index_series] <- NA

  index_Aug1997 <- which((abs(dates - (1997 + 8/12)) < 1e-05) == TRUE)
  index_series <- data_ESIBCI$groups == "ESI: industry" & data_ESIBCI$names == "Employment expectations for the months ahead"
  data_ESIBCI$rawdata[1:index_Aug1997, index_series] <- NA

  # Transform (and seasonally adjust if needed)
  data_trafo <- matrix(NA, nrow(data_ESIBCI$rawdata), ncol(data_ESIBCI$rawdata))
  flag_dontuse <- matrix(0, 1, ncol(data_ESIBCI$rawdata))

  # Loop through columns of data_ESIBCI$rawdata
  for (n in 1:ncol(data_ESIBCI$rawdata)) {

    # Check if the data is seasonally adjusted
    
    # Overwrite sa_flag!
    data_ESIBCI$flag_sa[n] <- 0
    if (data_ESIBCI$flag_sa[n] == 1) {
      # Check if we have enough observations (> 5 years)
      if (sum(!is.na(data_ESIBCI$rawdata[,n])) > 60) {
        temp <- f_sa(data_ESIBCI$rawdata[,n]) #?# f_sa?
      } else {
        # Exclude from the analysis
        flag_dontuse[n] <- 1
      }
    } else {
      temp <- data_ESIBCI$rawdata[,n]
    }

    # Perform the appropriate transformation
    if (data_ESIBCI$type[n] == "m") { 
      switch (data_ESIBCI$trafo[n],
             "1" = data_trafo[,n] <- temp,
             "2" = data_trafo[,n] <- c(NA, diff(temp)),
             "3" = data_trafo[,n] <- c(NA, 100 * diff(log(temp))) 
      )
    } else {                          
      # For quarterly variables
      switch (data_ESIBCI$trafo[n],
             "1" =  data_trafo[,n] <- temp,
             "2" =  data_trafo[,n] <- c(rep(NA, 3), temp[4:length(temp)] - temp[1:(length(temp) - 3)]),
             "3" =  data_trafo[,n] <- c(rep(NA, 3), 100 * (log(temp[4:length(temp)]) - log(temp[1:(length(temp) - 3)]))) 
      ) 
    }
  }
  
  data_ESIBCI$data <- data_trafo

  # Remove series that could not be seasonally adjusted

  # also remove quarterly series
  if (!all(flag_dontuse == 0)){
    data_ESIBCI$namesremoved = data_ESIBCI$names[flag_dontuse == 1]
    data_ESIBCI$data = data_ESIBCI$data[, !flag_dontuse]
    data_ESIBCI$rawdata = data_ESIBCI$rawdata[, !flag_dontuse]
    data_ESIBCI$trafo = data_ESIBCI$trafo[, !flag_dontuse]
    data_ESIBCI$names = data_ESIBCI$names[, !flag_dontuse]
    data_ESIBCI$groups = data_ESIBCI$groups[, !flag_dontuse]
    data_ESIBCI$type = data_ESIBCI$type[, !flag_dontuse]
    data_ESIBCI$flag_usestartvals = data_ESIBCI$flag_usestartvals[, !flag_dontuse]
    data_ESIBCI$flag_sa = data_ESIBCI$flag_sa[, !flag_dontuse]
  }


  return(data_ESIBCI)
 }


