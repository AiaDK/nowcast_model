
f_convertdates <- function(dates_num) {
  dates_str <- rep(NA, length(dates_num))
  
  for (t in 1:length(dates_num)) {
    date_year <- floor(dates_num[t])
    date_month <- round((dates_num[t] - floor(dates_num[t]))*12)
    
    if (date_month == 0) {
      temp <- paste0(as.character(date_year - 1), "-", "12")
    } else {
      temp <- paste0(as.character(date_year), "-", as.character(date_month))
    }
    
    dates_str[t] <- temp
  }
  
  return(dates_str)
}





