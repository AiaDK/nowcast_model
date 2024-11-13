
# Function to determine variables to remove
determine_vars_remove <- function(dir_data, vintages, samplestart, dates) {
  list_removevars <- list()

  for (v in 2:length(vintages)) {
   
    # Get data and names for the previous vintage
    data_prev_vintage <- f_constructdataset(dir_data, samplestart, vintages[v - 1], NULL, NULL, NULL)
    dataM <- data_prev_vintage$dataM
    dataQ <- data_prev_vintage$dataQ
    names_old <- data_prev_vintage$names
    groups_old <- data_prev_vintage$groups
    
    data_old <- cbind(rbind(dataM, dataQ), matrix(NA, (nrow(dataM)+nrow(dataQ)), length(dates)-ncol(dataM)))
    
    
    # Get data and names for the current vintage
    data_current_vintage <- f_constructdataset(dir_data, samplestart, vintages[v], NULL, NULL, NULL)
    dataM <- data_current_vintage$dataM
    dataQ <- data_current_vintage$dataQ
    names_new <- data_current_vintage$names
    groups_new <- data_current_vintage$groups
    
    # Create a new dataset with NaNs for the current vintage
    data_new <- cbind(rbind(dataM, dataQ), matrix(NA, (nrow(dataM)+nrow(dataQ)), (length(dates)-ncol(dataM))))

   
    # Check if names and groups are the same in two adjacent vintages
    if (identical(names_new, names_old) && identical(groups_new, groups_old)) {
      newobs <- !is.na(data_new) & is.na(data_old)
      n_newobs <- matrix(rowSums(newobs, na.rm = TRUE))
      
      if (any(n_newobs > 1)) {
        list_removevars$names <- c(list_removevars$names, names_new[n_newobs > 1])
        list_removevars$groups <- c(list_removevars$groups, groups_new[n_newobs > 1])
      }
    } else {
      stop("Not the same variables in two adjacent vintages! Abort and manually check vintage construction!")
    }
  }
  
  list_removevars$namegroup <- paste(list_removevars$names, list_removevars$groups, sep = "_")
  ind_unique <- order(which(!duplicated(list_removevars$namegroup)), decreasing = T)
  list_removevars$names <- list_removevars$names[ind_unique]
  list_removevars$groups <- list_removevars$groups[ind_unique]
  list_removevars$namegroup <- list_removevars$namegroup[ind_unique]
  
  return(list_removevars)
}
