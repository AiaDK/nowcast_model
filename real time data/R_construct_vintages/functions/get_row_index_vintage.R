#
get_row_index_vintage <- function(dates, releasedates, vintage, freq) {
  
  is_released <- FALSE
  t <- length(dates) + 1 # start with latest observations (+ 1, then reduce in loop)
  while (!is_released) {
    t <- t-1
    if (freq == "m") {
      ind_ym_release <- which(releasedates[["year"]] == as.numeric(format(dates[t], format="%Y")) & 
                                releasedates[["month"]] == as.numeric(format(dates[t], format="%m")))
      if (length(ind_ym_release) == 0) {
        stop("##- Could not find date. Check that the ifo release dates files is up to date!!!")
      }
    } else if (freq == "q") {
      # assumes that quarterly values are published in the first month of the quarter!
      ind_ym_release <- which(releasedates[["year"]] == as.numeric(substr(dates[t], 1, 4)) & 
                                releasedates[["month"]] == as.numeric(substr(dates[t], nchar(dates[t]), nchar(dates[t])))*3-2)
    } else {
      stop("##- freq has to either be m or q")
    }
    is_released <- as.Date(releasedates[["release_date"]][ind_ym_release], "%d/%m/%Y") <= as.Date(vintage, "%d-%b-%Y") 
  }
  
  index_row <- t
}

