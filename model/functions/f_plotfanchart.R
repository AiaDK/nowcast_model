
#?# my simplified version

f_plotfanchart <- function(dens, nametitle, dates) {

  xvals <- 1:nrow(dens)
  centerline <- apply(dens, 1, mean)
  prctiles <- c(5, 25, 50, 75, 95)
  bands <- t(apply(dens, 1, function(x) quantile(x, probs = prctiles/100)))
  
  df_q <- data.frame(x = c(xvals, rev(xvals)),
                     y = c(bands[,1], rev(bands[,5])),
                     z = c(bands[,2], rev(bands[,4])))
  df_cl <- data.frame(xvals, centerline, dates)
  
  fig_pfc <- ggplot() + 
    geom_polygon(data = df_q, aes(x = x, y = y, fill = "90 %"),  alpha = 0.5) +
    geom_polygon(data = df_q, aes(x = x, y = z, fill = "50 %"), alpha = 0.5) +
    geom_line(data = df_cl, aes(xvals, centerline, color = "mean"), linewidth = 0.5) +
    scale_fill_manual(values = c("coral2", "darksalmon")) +
    scale_color_manual(values = "darkred") +
    scale_x_continuous(breaks = xvals, labels = dates) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5),
          legend.title = element_blank()) +
    labs(title = nametitle, 
         x = element_blank(),
         y = element_blank())
  
  return(fig_pfc)
}


  





