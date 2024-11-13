
#?# mistake: temp & neg_temp & pos_temp difference
f_graphnowcastevolve <- function(nowcasts, groupcontribs, nowcastrevision, titlename, vintagedates, groupnames) {
  
  
  # Figure 1
  nc_pdata <- data.frame(vintagedates, nowcasts)
  fig_casts <- ggplot(nc_pdata, aes(vintagedates, nowcasts, group=1)) + 
    geom_point() + 
    geom_line() +
    scale_x_date(breaks = nc_pdata$vintagedates, date_labels = "%d-%b-%Y") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5)) +
    labs(title = titlename, 
         x = element_blank(),
         y = "Percent", 
         subtitle = paste("nowcast on", format(vintagedates[length(vintagedates)], "%d-%b-%Y"), ":", round(nowcasts[length(nowcasts)], 2)))
  
  
  # Figure 2
  
  # Aggregate data
  temp <- matrix(0,1,length(vintagedates))
  tempnames <- c()
  ind_temp <- 1
  
  index_group <-  groupnames=='ESI' | groupnames=='ifo'
  temp[ind_temp,] <- colSums(groupcontribs[index_group,]) 
  tempnames[ind_temp] <- 'surveys'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='production'
  if (any(index_group)) {temp <- rbind(temp, groupcontribs[index_group, ])} else {temp <- rbind(temp, rep(0, length(vintagedates)))}
  tempnames[ind_temp] <- 'production'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='orders'
  if (any(index_group)) {temp <- rbind(temp, groupcontribs[index_group, ])} else {temp <- rbind(temp, rep(0, length(vintagedates)))}
  tempnames[ind_temp] <- 'orders'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='turnover'
  if (any(index_group)) {temp <- rbind(temp, groupcontribs[index_group, ])} else {temp <- rbind(temp, rep(0, length(vintagedates)))}
  tempnames[ind_temp] <- 'turnover'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='financial'
  if (any(index_group)) {temp <- rbind(temp, groupcontribs[index_group, ])} else {temp <- rbind(temp, rep(0, length(vintagedates)))}
  tempnames[ind_temp] <- 'financial'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='labor market'
  if (any(index_group)) {temp <- rbind(temp, groupcontribs[index_group, ])} else {temp <- rbind(temp, rep(0, length(vintagedates)))}
  tempnames[ind_temp] <- 'labor market'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='prices' 
  if (any(index_group)) {temp <- rbind(temp, groupcontribs[index_group, ])} else {temp <- rbind(temp, rep(0, length(vintagedates)))}
  tempnames[ind_temp] <- 'prices'
  
  ind_temp <- ind_temp + 1
  index_group <- groupnames=='national accounts'
  if (any(index_group)) {temp <- rbind(temp, groupcontribs[index_group, ])} else {temp <- rbind(temp, rep(0, length(vintagedates)))}
  tempnames[ind_temp] <- 'national accounts'
  
  ind_temp <- ind_temp + 1
  temp <- rbind(temp, nowcastrevision[1,])
  tempnames[ind_temp] <- 'data revisions'

  # Plot fig2
  rownames(temp) <- tempnames
  colnames(temp) <- vintagedates
  temp_df <- melt(temp)
  colnames(temp_df)=c("group", "vintage", "values")
  temp_df["vintage"] <- as.Date(temp_df$vintage)
  
  color_palette <- c("#000000", "#2ca02c", "darkgreen", "#9467bd", "#56B4E9", "#d62728", "#E69F00", "#0072B2", "#D55E00")
  if (length(tempnames) != length(color_palette)) {
    stop("Number of groups and specified colors do not match. Abort!")
  }
  
  temp_df$group <- factor(temp_df$group, levels = c("data revisions", "national accounts", "prices", "labor market", "financial", "turnover", "orders", "production", "surveys"))
  fig_newsdecomp <- ggplot(temp_df, aes(vintage, values, fill=group)) + 
    geom_bar(position = "stack", stat = "identity") +
    geom_hline(yintercept = 0, linewidth = 0.1) +
    scale_x_date(breaks = temp_df$vintage, date_labels = "%d-%b-%Y") +
    scale_y_continuous(breaks = c(2, 1.5, 1, 0.5, 0, -0.5, -1, -1.5, -2)) +
    scale_fill_manual(values = color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5),
          legend.title = element_blank()) +
    labs(title = "News decomposition", 
         x = element_blank(),
         y = "Percent")
  
  return(list(fig_casts=fig_casts, fig_newsdecomp=fig_newsdecomp))
}
