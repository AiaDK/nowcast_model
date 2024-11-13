

f_constructdataset <- function(dir_data, samplestart, vintage, list_removevars, means, stds) {
  
  # Load datasets
  load(paste0(dir_data, "/vintages/dataset_", format(as.Date(vintage, "%d-%b-%Y"), format = "%Y_%m_%d"), ".RData"))
  
  # Get vintage date 
  vintagedate <- as.Date(vintage, "%d-%b-%Y")
 
  # Sample start
  index_ifo <- which((abs(dataset$data_ifo$dates - samplestart) < 1e-05) == TRUE)
  index_ESIBCI <- which((abs(dataset$data_ESIBCI$dates - samplestart) < 1e-05) == TRUE)
  index_BuBaRTD <- which((abs(dataset$data_BuBaRTD$dates - samplestart) < 1e-05) == TRUE)
  index_financial <- which((abs(dataset$data_financial$dates - samplestart) < 1e-05) == TRUE)
  
  # Maximum length of data
  maxobs <- max(c(nrow(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), , drop=FALSE]), 
                 nrow(dataset$data_ifo$data[index_ifo:nrow(dataset$data_ifo$data), , drop=FALSE]),
                 nrow(dataset$data_ESIBCI$data[index_ESIBCI:nrow(dataset$data_ESIBCI$data), , drop=FALSE]),
                 nrow(dataset$data_financial$data[index_financial:nrow(dataset$data_financial$data), , drop=FALSE])))
  
  # Extract names, groups and flag_usestartvals
  namesM <- c(dataset$data_BuBaRTD$names[dataset$data_BuBaRTD$type == "m", drop=FALSE], 
             dataset$data_ifo$names[dataset$data_ifo$type == "m" | dataset$data_ifo$type == "q:A", drop=FALSE], 
             dataset$data_ESIBCI$names[dataset$data_ESIBCI$type == "m" | dataset$data_ESIBCI$type == "q:A", drop=FALSE], 
             dataset$data_financial$names[dataset$data_financial$type == "m", drop=FALSE])
  
  groupsM <- c(dataset$data_BuBaRTD$groups[dataset$data_BuBaRTD$type == "m", drop=FALSE], 
              dataset$data_ifo$groups[dataset$data_ifo$type == "m" | dataset$data_ifo$type == "q:A", drop=FALSE], 
              dataset$data_ESIBCI$groups[dataset$data_ESIBCI$type == "m" | dataset$data_ESIBCI$type == "q:A", drop=FALSE], 
              dataset$data_financial$groups[dataset$data_financial$type == "m", drop=FALSE])
  
  flag_usestartvalsM <- c(dataset$data_BuBaRTD$flag_usestartvals[dataset$data_BuBaRTD$type == "m", drop=FALSE], 
                         dataset$data_ifo$flag_usestartvals[dataset$data_ifo$type == "m" | dataset$data_ifo$type == "q:A", drop=FALSE], 
                         dataset$data_ESIBCI$flag_usestartvals[dataset$data_ESIBCI$type == "m" | dataset$data_ESIBCI$type == "q:A", drop=FALSE], 
                         dataset$data_financial$flag_usestartvals[dataset$data_financial$type == "m", drop=FALSE])
  
  namesQ = dataset$data_BuBaRTD$names[dataset$data_BuBaRTD$type != "m", drop=FALSE]
  groupsQ = dataset$data_BuBaRTD$groups[dataset$data_BuBaRTD$type != "m", drop=FALSE]
  flag_usestartvalsQ = dataset$data_BuBaRTD$flag_usestartvals[dataset$data_BuBaRTD$type != "m", drop=FALSE]
  
  # Merge names and groups
  names = c(namesM, namesQ)
  groups = c(groupsM, groupsQ)
  
  # Data
  dataM <- t(cbind(rbind(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type == "m", drop=FALSE], 
                         matrix(NA, maxobs - nrow(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type == "m", drop=FALSE]), ncol(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type == "m", drop=FALSE]))),
                   
                   rbind(dataset$data_ifo$data[index_ifo:nrow(dataset$data_ifo$data), (dataset$data_ifo$type == "m" | dataset$data_ifo$type == "q:A"), drop=FALSE],
                         matrix(NA, maxobs - nrow(dataset$data_ifo$data[index_ifo:nrow(dataset$data_ifo$data), (dataset$data_ifo$type == "m" | dataset$data_ifo$type == "q:A"), drop=FALSE]), ncol(dataset$data_ifo$data[index_ifo:nrow(dataset$data_ifo$data), (dataset$data_ifo$type == "m" | dataset$data_ifo$type == "q:A"), drop=FALSE]))),
                   
                   rbind(dataset$data_ESIBCI$data[index_ESIBCI:nrow(dataset$data_ESIBCI$data), (dataset$data_ESIBCI$type == "m" | dataset$data_ESIBCI$type == "q:A"), drop=FALSE],
                         matrix(NA, maxobs - nrow(dataset$data_ESIBCI$data[index_ESIBCI:nrow(dataset$data_ESIBCI$data), (dataset$data_ESIBCI$type == "m" | dataset$data_ESIBCI$type == "q:A"), drop=FALSE]), ncol(dataset$data_ESIBCI$data[index_ESIBCI:nrow(dataset$data_ESIBCI$data), (dataset$data_ESIBCI$type == "m" | dataset$data_ESIBCI$type == "q:A"), drop=FALSE]))),
                   
                   rbind(dataset$data_financial$data[index_financial:nrow(dataset$data_financial$data), dataset$data_financial$type == "m", drop=FALSE], 
                         matrix(NA, maxobs - nrow(dataset$data_financial$data[index_financial:nrow(dataset$data_financial$data), dataset$data_financial$type == "m", drop=FALSE]), ncol(dataset$data_financial$data[index_financial:nrow(dataset$data_financial$data), dataset$data_financial$type == "m", drop=FALSE])))))
  
  if (maxobs == nrow(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type != "m", drop=FALSE])) {
    dataQ <- t(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type != "m", drop=FALSE])
  } else {
    dataQ <- t(rbind(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type != "m", drop=FALSE], 
                     matrix(NA, maxobs - nrow(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type != "m", drop=FALSE]), 
                            ncol(dataset$data_BuBaRTD$data[index_BuBaRTD:nrow(dataset$data_BuBaRTD$data), dataset$data_BuBaRTD$type != "m", drop=FALSE]))))
  }
  
  rownames(dataQ) <- NULL
  rownames(dataM) <- NULL
  
  # Dates
  dates <- samplestart + (0:(ncol(dataM)-1))/12

  # Merge flag_usestartvals after adjusting for partially missing obs
  sumNaNs <- rowSums(is.na(dataM))
  flag_usestartvalsM[flag_usestartvalsM == 1 & sumNaNs > 0.2 * ncol(dataM)] <- 0 
  
  flag_usestartvals <- c(flag_usestartvalsM, flag_usestartvalsQ)
  
  
  # Remove specified vars
  if (!is_empty(list_removevars)) {
    index_remove <- c()
    index_keep <- c()
    for (n in 1:length(names)){
      index_remove[n] <- any(list_removevars$names == names[n]) & any(list_removevars$groups == groups[n]) 
      index_keep[n] <- !(any(list_removevars$names == names[n]) & any(list_removevars$groups == groups[n])) # changed
    }
    dataM <- dataM[index_keep[1:nrow(dataM)],]
    flag_usestartvals <- flag_usestartvals[index_keep]
    names <- names[index_keep]
    groups <- groups[index_keep]
  }
  
  
  # Standardize
  if (is_empty(means) && is_empty(stds)) {
    
    dataM_stand <- matrix(NA, nrow(dataM), ncol(dataM))
    meansM <- c()
    stdsM <- c()
    for (n in 1:nrow(dataM)) {
      meansM[n] <- mean(dataM[n,], na.rm = TRUE)
      stdsM[n] <- sd(dataM[n,], na.rm = TRUE)
      dataM_stand[n,] <- (dataM[n,] - meansM[n])/stdsM[n]
    }
    
    dataQ_stand <- matrix(NA, nrow(dataQ), ncol(dataQ))
    meansQ <- c()
    stdsQ <- c()
    for (n in 1:nrow(dataQ)) {
      meansQ[n] <- mean(dataQ[n,], na.rm = TRUE)
      stdsQ[n] <- sd(dataQ[n,], na.rm = TRUE)
      dataQ_stand[n,] <- (dataQ[n,] - meansQ[n])/stdsQ[n]
    }
    
    means <- c(meansM, meansQ)
    stds <- c(stdsM, stdsQ)
    
  } else {
    # use given means and stds to standardize variables
    dataM_stand <- matrix(NA, nrow(dataM), ncol(dataM))
    for (n in 1:nrow(dataM)) {
      dataM_stand[n, ] <- (dataM[n,] - means[n])/stds[n]
    }
    dataQ_stand <- matrix(NA, nrow(dataQ), ncol(dataQ))
    for (n in 1:nrow(dataQ)) {
      dataQ_stand[n, ] <- (dataQ[n,] - means[nrow(dataM) + n])/stds[nrow(dataM) + n]
    }
  }
  
  return(list(dataM_stand = dataM_stand, 
              dataQ_stand = dataQ_stand, 
              means = means, 
              stds = stds, 
              flag_usestartvals = flag_usestartvals, 
              names = names, 
              groups = groups, 
              dates = dates, 
              vintagedate = vintagedate))
}
