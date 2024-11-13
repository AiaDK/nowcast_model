# 
f_table <- function(nowcast, revision, groupcontribs, vintages, savename, groupnames, dirname) {
  
  # prepare group contributions and names
  temp_groupcontrib <- matrix(0,1,length(vintages))
  tempnames <- c()
  ind_temp <- 1
  
  index_group <-  groupnames=='ESI' | groupnames=='ifo'
  temp_groupcontrib[ind_temp,] <- colSums(groupcontribs[index_group,]) 
  tempnames[ind_temp] <- 'surveys'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='production'
  if (any(index_group)) {temp_groupcontrib <- rbind(temp_groupcontrib, groupcontribs[index_group, ])} else {temp_groupcontrib <- rbind(temp_groupcontrib, rep(0, length(vintages)))}
  tempnames[ind_temp] <- 'production'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='orders'
  if (any(index_group)) {temp_groupcontrib <- rbind(temp_groupcontrib, groupcontribs[index_group, ])} else {temp_groupcontrib <- rbind(temp_groupcontrib, rep(0, length(vintages)))}
  tempnames[ind_temp] <- 'orders'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='turnover'
  if (any(index_group)) {temp_groupcontrib <- rbind(temp_groupcontrib, groupcontribs[index_group, ])} else {temp_groupcontrib <- rbind(temp_groupcontrib, rep(0, length(vintages)))}
  tempnames[ind_temp] <- 'turnover'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='financial'
  if (any(index_group)) {temp_groupcontrib <- rbind(temp_groupcontrib, groupcontribs[index_group, ])} else {temp_groupcontrib <- rbind(temp_groupcontrib, rep(0, length(vintages)))}
  tempnames[ind_temp] <- 'financial'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='labor market'
  if (any(index_group)) {temp_groupcontrib <- rbind(temp_groupcontrib, groupcontribs[index_group, ])} else {temp_groupcontrib <- rbind(temp_groupcontrib, rep(0, length(vintages)))}
  tempnames[ind_temp] <- 'labor market'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='prices'
  if (any(index_group)) {temp_groupcontrib <- rbind(temp_groupcontrib, groupcontribs[index_group, ])} else {temp_groupcontrib <- rbind(temp_groupcontrib, rep(0, length(vintages)))}
  tempnames[ind_temp] <- 'prices'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='national accounts'
  if (any(index_group)) {temp_groupcontrib <- rbind(temp_groupcontrib, groupcontribs[index_group, ])} else {temp_groupcontrib <- rbind(temp_groupcontrib, rep(0, length(vintages)))}
  tempnames[ind_temp] <- 'national accounts'
  
  if (length(tempnames) != 8) {
    stop("Number of categories does not match number of variables in table. Abort!")
  }
  
  # Create table
  T <- data.frame(as.matrix(nowcast), 
                  t(temp_groupcontrib),
                  matrix(revision, length(revision), 1))
  colnames(T) <- c("prognose", tempnames, "revision")
  rownames(T) <- vintages 
  
  # Export to xls
  write.xlsx(T, paste0(dirname, "/tables/", savename, ".xlsx"), row.names = TRUE)
}
