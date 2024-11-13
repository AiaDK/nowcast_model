args <- commandArgs(trailingOnly = TRUE)
dir_root <- args[1]
year_nowcast <- as.numeric(args[2])
quarter_nowcast <- as.numeric(args[3])

library(readxl)
library(purrr)
library(dplyr)
library(xlsx)
library(lubridate)
library(zoo)
library(R.utils)
library(miceadds)
library(ggplot2)
library(reshape2)

plot_forecasts <- function(y, xi, dates, date_start, str_title) {
  med_xi <- t(colMeans(xi, na.rm = TRUE))
  upper_xi <- t(matrix(apply(xi, 2, quantile, probs = c(0.95),  na.rm = TRUE, type=5)))
  lower_xi <- t(matrix(apply(xi, 2, quantile, probs = c(0.05),  na.rm = TRUE, type=5)))
  
  n_start <- which(dates == date_start)
  n_obs <- sum(!is.na(y[n_start:length(y)]))
  
  dates_plot <- dates[n_start:length(dates)]
  
  df <- data.frame(date = dates_plot,
                   y = y[n_start:length(y)], 
                   med_xi = c(y[n_start : (n_start-1+n_obs)], med_xi[(n_start+n_obs):length(med_xi)]),
                   upper_xi = c(y[n_start : (n_start-1+n_obs)], upper_xi[(n_start + n_obs):length(upper_xi)]),
                   lower_xi = c(y[n_start : (n_start-1+n_obs)], lower_xi[(n_start + n_obs):length(lower_xi)]))
  
  
  p <- ggplot(df, aes(x = factor(date, levels=date), group=1)) +
    geom_line(aes(y = y), color = 'blue') +
    geom_line(aes(y = med_xi),linetype = 'dashed', color = 'blue') +
    geom_line(aes(y = upper_xi),linetype = 'dotted', color = 'blue') +
    geom_line(aes(y = lower_xi),linetype = 'dotted', color = 'blue') +
    labs(title = str_title,
         x = element_blank(),
         y = element_blank()) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
          plot.title = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

dir_nongdp_forecasts <- paste0(dir_root, '/Nowcasts/', year_nowcast, 'Q', quarter_nowcast, '/non gdp forecasts')

Nrs <- read.csv('../model_specs_Nrs.csv', header = FALSE)
Nps <- read.csv('../model_specs_Nps.csv', header = FALSE)
Njs <- read.csv('../model_specs_Njs.csv', header = FALSE)

Nmodels <- length(Nrs) * length(Nps) * length(Njs)

vintages <- read.table("../dates_vintages.txt", header = FALSE)
vintages <- as.character(vintages[, 1])
vintage <- vintages[length(vintages)]

vars <- c('IP', 'IFOLAGE', 'ORD', 'IFOERW')
str_titles <- c('Industrieproduktion, m/m', 'Ifo - Lage (1. Diff.)', 'Auftragseingänge, m/m', 'Ifo - Erwartungen (1. Diff.)')
Nvars <- length(vars)
load(file.path(dir_nongdp_forecasts, "dates.RData"))
date_start <- dates_converted[length(dates_converted) - 11]
Nobs <- length(dates_converted)

ys <- array(NA, dim = c(1, Nobs, Nvars))
xis <- array(NA, dim = c(Nmodels, Nobs, Nvars))

pdf(paste0(dir_nongdp_forecasts, '/forecasts_', paste(vars, collapse = '_'), '.pdf'), width = 9, height = 5)

for (i in 1:Nvars) {
  for (m in 1:Nmodels) {
    load(file.path(dir_nongdp_forecasts, paste0(vars[i], '_for_model_', m, '_v_', vintage, '.RData')))
    ys[1, , i] <- out$dat
    xis[m, , i] <- out$xi
  }
  
  p <- plot_forecasts(ys[1, , i], xis[, , i], dates_converted, date_start, str_titles[i])
  print(p)
}

dev.off()

cat("##- Done plotting non GDP forecasts\n")
out <- NULL





