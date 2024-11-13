 
# setwd("C:/Users/Aickolpon/Desktop/IfW/nowcasting/R_datatests/model")
# dir_root <- "C:/Users/Aickolpon/Desktop/IfW/nowcasting/R_datatests"
# year_nowcast <- 2023
# quarter_nowcast <- 3
# switch_estimatemodels <- 0

args <- commandArgs(trailingOnly = TRUE)
dir_root <- args[1]
year_nowcast <- as.numeric(args[2])
quarter_nowcast <- as.numeric(args[3])
switch_estimatemodels <- as.numeric(args[4])

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

### Preparatory arrangements ###
# define directories
dir_data <- paste0(dir_root, "/Echtzeitdatensatz")
dir_nowcast <- paste0(dir_root, "/Nowcasts/", year_nowcast, "Q", quarter_nowcast)
if (!file.exists(dir_nowcast)) dir.create(dir_nowcast)

# Add nowcasting functions
source.all(paste0(dir_root, "/model/functions"))

# Make sure graphs and params folders exist
if (!file.exists(file.path(dir_nowcast, "graphs"))) dir.create(file.path(dir_nowcast, "graphs"))
if (!file.exists(file.path(dir_nowcast, "params"))) dir.create(file.path(dir_nowcast, "params"))
if (!file.exists(file.path(dir_nowcast, "docu"))) dir.create(file.path(dir_nowcast, "docu"))
if (!file.exists(file.path(dir_nowcast, "tables"))) dir.create(file.path(dir_nowcast, "tables"))
if (!file.exists(file.path(dir_nowcast, "monthlyGDP"))) dir.create(file.path(dir_nowcast, "monthlyGDP"))
if (!file.exists(file.path(dir_nowcast, "non gdp forecasts"))) dir.create(file.path(dir_nowcast, "non gdp forecasts"))


### User specified settings ###

# Vintages
vintages <- read.table("../dates_vintages.txt", header = FALSE)
vintages <- as.character(vintages[, 1])
vintage_estim <- vintages[1]

# Check whether all vintages are available
flag_missing <- checkvintages(vintages, dir_data)
if (any(flag_missing == 1)) {
  stop("##- Missing vintages. Abort execution")
}

# Check whether vintage dates are increasing
for (v in 2:length(vintages)) {
  if (as.Date(vintages[v], "%d-%b-%Y") <= as.Date(vintages[v-1], "%d-%b-%Y")) {
    stop("##- Vintage dates are not increasing. Abort execution")
  }
}

# Sample starts in ...
samplestart <- 1996 + 1/12 # January 1, 1996

# Dates corresponding to now- and forecast
date_nowcast <- year_nowcast + quarter_nowcast*3/12
if (quarter_nowcast*3 == 12) {
  date_forecast <- year_nowcast + 1 + 3/12
} else {
  date_forecast <- year_nowcast + (quarter_nowcast*3+3)/12
}


### Model Specifications ###

# Number of factors
Nrs <- as.matrix(read.csv('../model_specs_Nrs.csv', header = FALSE))
# Number of lags in factor VAR
Nps <-  as.matrix(read.csv('../model_specs_Nps.csv', header = FALSE))
# Number of lags in idiosyncratic component
Njs <-  as.matrix(read.csv('../model_specs_Njs.csv', header = FALSE))

# Switches
  # 1 <- yes
  # 0 <- no
switch_savetables <- 1
switch_savegraphs <- 1
switch_savedocus <- 1

# List of vars to be removed from the data set
list_removevars <- determine_vars_remove(dir_data, vintages, samplestart, seq(samplestart, date_forecast, by=1/12))
writeLines(list_removevars$namegroup, file.path(dir_nowcast, "list_removed_vars.txt"))
# Monthly variables for which we store the forecasts based on the latest available vintages
names_export <- c("Industrie", "Industrie", "lkw_maut", "ifo_lage", "ifo_erwartung")
groups_export <- c("production", "orders", "production", "ifo", "ifo")
mnemonic_export <- c("ip", "ord", "lkwm", "ifoLage", "ifoErw")

if (length(names_export) != length(groups_export) || length(names_export) != length(mnemonic_export)) {
  stop("names, groups and mnemonics vector must be of the same length!")
}


###  Estimate Models  ###
if (switch_estimatemodels == 1) {
  f_estimatemodels(samplestart, vintage_estim, Nrs, Nps, Njs, list_removevars, dir_nowcast, dir_data)
}

Nmodels <- length(Nrs) * length(Nps) * length(Njs) # number of models
modcounter <- 1 # set model counter to 1

# Number of vintages 
Nvintages <- length(vintages)

# Construct dates and find indices of now- and forecast
options <- list()
options$dates <- seq(from=samplestart, to=date_forecast, by=1/12)
options$index_nowcast <- (abs(options$dates - date_nowcast)<1e-05) == TRUE
options$index_forecast <- (abs(options$dates - date_forecast)<1e-05) == TRUE

# Create data storage constructs outside of loop
results <- list()
results$vintages <- list()

results$nowcast <- list()
results$nowcast$new <- array(0, dim=c(1, Nvintages, Nmodels))
results$nowcast$impact_by_group <- array(0, dim = c(0,0,0)) 
results$nowcast$details <- array(list(), dim = c(Nvintages, Nmodels))
results$nowcast$revised_data <- array(0, dim = c(1, Nvintages, Nmodels))

results$forecast <- list()
results$forecast$new <- array(0, dim=c(1, Nvintages, Nmodels))
results$forecast$impact_by_group <- array(0, dim = c(0,0,0)) 
results$forecast$details <- array(list(), dim = c(Nvintages, Nmodels))
results$forecast$revised_data <- array(0, dim = c(1, Nvintages, Nmodels))


# Loop over models
for (Nr in Nrs) {
  for (Np in Nps) {
    for (Nj in Njs) {

      # Store Nr, Np and Nj in options
      options$Nr <- Nr
      options$Np <- Np
      options$Nj <- Nj

      # Load params
      load(paste0(dir_nowcast, "/params/params_Nr", Nr, "_Np", Np, "_Nj", Nj, ".RData"))

      # Load/construct data set
      data <- f_constructdataset(dir_data, samplestart, as.Date(vintages[1], "%d-%b-%Y"), list_removevars, c(), c())
      dataM <- data$dataM_stand
      dataQ <- data$dataQ_stand
      options$means <- data$means
      options$stds <- data$stds
      options$names <- data$names
      options$groups <- data$groups
      results$vintages[[1]] <- vintages[1] 
      data <- cbind(rbind(dataM, dataQ), matrix(NA, nrow(dataM) + nrow(dataQ), length(options$dates) - ncol(dataM)))

      # Calculate some more options
      options$index_gdp <- options$names == "Gross Domestic Product"
      options$groupnames <- sort(unique(options$groups))
      options$meangdp <- options$means[options$index_gdp] 
      options$stdgdp <- options$stds[options$index_gdp] 
      options$Nm <- nrow(dataM)
      options$Nq <- nrow(dataQ)

      if (options$Nj > 0) {
        options$Ns <- 5*(options$Nr+options$Nq)+options$Nm
      } else {
        options$Ns <- 5*(options$Nr+options$Nq)
      }

      # Calculate state space parameters
      statespaceparams_news <- f_statespaceparams_news(params, options)
      T <- statespaceparams_news$T
      Z <- statespaceparams_news$Z
      R <- statespaceparams_news$R
      Q <- statespaceparams_news$Q
      H <- statespaceparams_news$H

      # Compute first nowcast outside of loop
      s0 <- matrix(0, nrow(T), 1)
      P0 <- 10 * diag(1, nrow(T), nrow(T))
      ks_output_old <- f_KalmanSmootherDK(data, T, Z, H, R, Q, s0, P0)

      results$nowcast$new[1, 1, modcounter] <- options$stdgdp * (Z[options$index_gdp,] %*% ks_output_old$stT[,options$index_nowcast]) + options$meangdp
      results$forecast$new[1, 1, modcounter] <- options$stdgdp * (Z[options$index_gdp,] %*% ks_output_old$stT[,options$index_forecast]) + options$meangdp

      # Update results$nowcast$impact_by_group
      if (dim(results$nowcast$impact_by_group)[[1]] == 0 && dim(results$forecast$impact_by_group)[[1]] == 0) {
        results$nowcast$impact_by_group <- array(0, dim = c(length(options$groupnames), Nvintages, Nmodels))
        results$forecast$impact_by_group <- array(0, dim = c(length(options$groupnames), Nvintages, Nmodels))
      } else {
        # do nothing
      }
      
      # rename data as old
      data_old <- data
      rm(dataM, dataQ, data)

      # Now loop over 2:Nvintages
      for (v in 2:Nvintages) {

        # for (v in Nvintages)
        cat(paste0('Current model ', modcounter, ' of ', Nmodels, '\n'))
        cat(paste0('Current vintage ', v-1, ' of ', Nvintages-1, '\n'))

        # Load new data
        data <- f_constructdataset(dir_data, samplestart, as.Date(vintages[[v]], "%d-%b-%Y"), list_removevars, options$means, options$stds)
        dataM_new <- data$dataM_stand
        dataQ_new <- data$dataQ_stand
        names_temp <- data$names
        groups_temp <- data$groups
        results$vintages[[v]] <- vintages[v]
        data_new <- cbind(rbind(dataM_new, dataQ_new), matrix(NA, nrow(dataM_new) + nrow(dataQ_new), length(options$dates) - ncol(dataM_new)))

        # nowcast with new data 
        ks_output_new <- f_KalmanSmootherDK(data_new, T, Z, H, R, Q, s0, P0)
        results$nowcast$new[1, v, modcounter] <- options$stdgdp * (Z[options$index_gdp,] %*% ks_output_new$stT[, options$index_nowcast == TRUE]) + options$meangdp
        results$forecast$new[1, v, modcounter] <- options$stdgdp * (Z[options$index_gdp,] %*% ks_output_new$stT[, options$index_forecast == TRUE]) + options$meangdp

        # calculate monthly m/m and 3m/3m GDP change
        gdp_mm <- Z[options$index_gdp, 1:(options$Nr+1)] %*% ks_output_new$stT[1:(options$Nr+1),]
        gdp_3m3m <- Z[options$index_gdp, ] %*% ks_output_new$stT

        # export with dates as mat file to folder
        mean_gdp <- options$meangdp
        std_gdp <- options$stdgdp
        foldername <- paste0(dir_nowcast, '/monthlyGDP/', vintages[[v]])
        if (!dir.exists(foldername)) dir.create(foldername)
        dates_converted <- f_convertdates(options$dates)
        gdp_realizations <- options$stdgdp * data_new[options$index_gdp,] + options$meangdp
        save(gdp_mm, gdp_3m3m, dates_converted, gdp_realizations, mean_gdp, std_gdp,
             file = paste0(dir_nowcast, '/monthlyGDP/', vintages[[v]], '/monthlyGDP_Nr', options$Nr, '_Np', options$Np, '_Nj', options$Nj, '.RData'))

        # Calculate forecasts for specific monthly variables
        out <- list()
        if (v == Nvintages) {
          for (i in 1:length(names_export)) {
            ind <- which(options$names == names_export[i] & options$groups == groups_export[i])
            out$dat <- options$stds[ind] * data_new[ind, ] + options$means[ind]
            out$xi <- options$stds[ind] * (Z[ind, ] %*% ks_output_new$stT) + options$means[ind]
            save(out, file = paste0(dir_nowcast, "/non gdp forecasts/", toupper(mnemonic_export[i]), "_for_model_", modcounter, "_v_", results$vintages[v], ".RData"))
          }
          save(dates_converted, file = paste0(dir_nowcast, "/non gdp forecasts/dates.RData"))
        }

        # Index of new obs
        newobs <- !is.na(data_new) & is.na(data_old)
        # Check that we have no more than one new obs!
        if (any(rowSums(newobs) > 1 & rowSums(newobs) < 12)) { # restrict comparison to past year as for some variables, e.g. hospitality, earlier values may sometimes become available
          cat("##- The following series have more than one new observations published at once. Remove from data set!\n")
          groups_temp[rowSums(newobs) > 1] 
          names_temp[rowSums(newobs) > 1] 
        }

        # Indices of new obs
        js <- which(newobs == TRUE, arr.ind = TRUE)[, "row", drop=FALSE]
        tjs <- which(newobs == TRUE, arr.ind = TRUE)[, "col", drop=FALSE]

        ###  News decomposition  ###

        # Nowcast #
        tks <- which(options$index_nowcast == 1)
        nd_nowcast <- f_newsdecomp_v2(js, tjs, tks, data_new, ks_output_old, params, options)
        results$nowcast$impact_by_group[, v, modcounter] <- nd_nowcast$impact_by_group
        results$nowcast$details[[v, modcounter]]$impacts <- nd_nowcast$impacts
        results$nowcast$details[[v, modcounter]]$actuals <- nd_nowcast$actuals
        results$nowcast$details[[v, modcounter]]$forecasts <- nd_nowcast$forecasts
        results$nowcast$details[[v, modcounter]]$weights <- nd_nowcast$weights
        results$nowcast$details[[v, modcounter]]$varnames <- nd_nowcast$varnames

        # Forecast #
        tks <- which(options$index_forecast == 1)
        nd_forecast <- f_newsdecomp_v2(js, tjs, tks, data_new, ks_output_old, params, options)
        results$forecast$impact_by_group[, v, modcounter] <- nd_forecast$impact_by_group
        results$forecast$details[[v, modcounter]]$impacts <- nd_forecast$impacts
        results$forecast$details[[v, modcounter]]$actuals <- nd_forecast$actuals
        results$forecast$details[[v, modcounter]]$forecasts <- nd_forecast$forecasts
        results$forecast$details[[v, modcounter]]$weights <- nd_forecast$weights
        results$forecast$details[[v, modcounter]]$varnames <- nd_forecast$varnames

        # Compute nowcast due to data revisions
        results$nowcast$revised_data[1, v, modcounter] <- results$nowcast$new[1, v, modcounter] - sum(results$nowcast$impact_by_group[, v, modcounter])
        results$forecast$revised_data[1, v, modcounter] <- results$forecast$new[1, v, modcounter] - sum(results$forecast$impact_by_group[, v, modcounter])

        # Update data Kalman smoother output
        ks_output_old <- ks_output_new
        data_old <- data_new

      }

      ###  Plot nowcast & forecast evolution for current model  ###

      # nowcast
      if (options$dates[options$index_nowcast] == floor(options$dates[options$index_nowcast])) { # Q4
        str_nowcast <- paste0(floor(options$dates[options$index_nowcast]) - 1, "Q4")
      } else {
        str_nowcast <- paste0(floor(options$dates[options$index_nowcast]), "Q", (options$dates[options$index_nowcast]-floor(options$dates[options$index_nowcast]))*12/3)
      }
      
      if (switch_savegraphs == 1) {
        titlename <- paste0("Nowcasts ", str_nowcast, " (Nr = ", options$Nr, ", Np = ", options$Np, ", Nj = ", options$Nj, ")")
        savename <- paste0("nowcasts_", str_nowcast, "_Nr", options$Nr, "_Np", options$Np, "_Nj", options$Nj)
        nw_figures <- f_graphnowcastevolve(results$nowcast$new[1,,modcounter],
                                           results$nowcast$impact_by_group[,,modcounter],
                                           matrix(c(0, results$nowcast$revised_data[1,2:ncol(results$nowcast$revised_data), modcounter]
                                                    - results$nowcast$new[1,1:(ncol(results$nowcast$new)-1),modcounter]), 1),
                                           titlename,
                                           as.Date(unlist(results$vintages), "%d-%b-%Y"),
                                           options$groupnames)
        
        pdf(paste0(dir_nowcast, "/graphs/", savename, ".pdf"), width = 9, height = 5 )
        print(nw_figures$fig_casts)
        print(nw_figures$fig_newsdecomp)
        dev.off()
      }

      # forecast
      if (options$dates[options$index_forecast] == floor(options$dates[options$index_forecast])) { # Q4
        str_forecast <- paste0(floor(options$dates[options$index_forecast]) - 1, "Q4")
      } else {
        str_forecast <- paste0(floor(options$dates[options$index_forecast]), "Q", (options$dates[options$index_forecast]-floor(options$dates[options$index_forecast]))*12/3)
      }
      
      if (switch_savegraphs == 1) {
        titlename <- paste("Nowcasts", str_forecast, "(Nr =", options$Nr, ", Np =", options$Np, ", Nj =", options$Nj, ")")
        savename <- paste0("nowcasts_", str_forecast, "_Nr", options$Nr, "_Np", options$Np, "_Nj", options$Nj)
        fc_figures <- f_graphnowcastevolve(results$forecast$new[1,,modcounter],
                                           results$forecast$impact_by_group[,,modcounter],
                                           matrix(c(0, results$forecast$revised_data[1,2:ncol(results$forecast$revised_data), modcounter]
                                                    - results$forecast$new[1,1:(ncol(results$forecast$new)-1),modcounter]), 1),
                                           titlename,
                                           as.Date(unlist(results$vintages), "%d-%b-%Y"),
                                           options$groupnames)
        pdf(paste0(dir_nowcast, "/graphs/", savename, ".pdf"), width = 9, height = 5 )
        print(fc_figures$fig_casts)
        print(fc_figures$fig_newsdecomp)
        dev.off()
      }


      ###  Assemble forecast revision docu and store as txt-file  ###

      if (switch_savedocus == 1) {
        flag_ewpool <- 0
        threshold <- 0.02

        # nowcasts
        savename <- paste0("nowcasts_", str_nowcast, "_Nr", options$Nr, "_Np", options$Np, "_Nj", options$Nj)
        f_docu(results$nowcast$new[1,,modcounter],
               results$vintages,
               results$nowcast$details[, modcounter],
               options,
               savename,
               flag_ewpool,
               threshold,
               dir_nowcast,
               str_nowcast)

        # forecasts
        savename <- paste0("nowcasts_", str_forecast, "_Nr", options$Nr, "_Np", options$Np, "_Nj", options$Nj)
        f_docu(results$forecast$new[1,,modcounter],
               results$vintages,
               results$forecast$details[, modcounter],
               options,
               savename,
               flag_ewpool,
               threshold,
               dir_nowcast,
               str_forecast)
      }

      ###  Export tables to xls  ###

      if (switch_savetables == 1) {

        # nowcast
        savename <- paste0("nowcasts_", str_nowcast, "_Nr", options$Nr, "_Np", options$Np, "_Nj", options$Nj)
        
        f_table(results$nowcast$new[1,,modcounter],
                c(0, results$nowcast$revised_data[1, 2:ncol(results$nowcast$revised_data), modcounter] - results$nowcast$new[1, 1:(ncol(results$nowcast$new) - 1), modcounter]),
                results$nowcast$impact_by_group[, , modcounter],
                results$vintages,
                savename,
                options$groupnames,
                dir_nowcast)

        # forecast
        savename <- paste0("nowcasts_", str_forecast, "_Nr", options$Nr, "_Np", options$Np, "_Nj", options$Nj)
        f_table(results$forecast$new[1,,modcounter],
                c(0, results$forecast$revised_data[1, 2:ncol(results$forecast$revised_data), modcounter] - results$forecast$new[1, 1:(ncol(results$forecast$new) - 1), modcounter]),
                apply(results$forecast$impact_by_group[,,1:modcounter], c(1,2), mean),
                results$vintages,
                savename,
                options$groupnames,
                dir_nowcast)
      }

      ###  Update model counter  ###

      modcounter <- modcounter + 1
    }
  }
}

###  Plot nowcast & forecast evolution for equal-weigth pool   ###

# nowcast
if (switch_savegraphs == 1) {

  titlename <- paste0("Nowcasts ", str_nowcast, " (equal-weight pool)")
  savename <- paste0("nowcasts_", str_nowcast, "_equalweightpool")
  nw_figs <- f_graphnowcastevolve(apply(results$nowcast$new[1,,], 1, mean),
                                  apply(results$nowcast$impact_by_group, c(1,2), mean),
                                  matrix(c(0, apply(results$nowcast$revised_data[1,2:ncol(results$nowcast$new),], 1, mean)
                                           - apply(results$nowcast$new[1,1:(ncol(results$nowcast$new)-1),], 1, mean)), 1),
                                  titlename,
                                  as.Date(unlist(results$vintages), "%d-%b-%Y"),
                                  options$groupnames)
  pdf(paste0(dir_nowcast, "/graphs/", savename, ".pdf"), width = 9, height = 5 )
  print(nw_figs$fig_casts)
  print(nw_figs$fig_newsdecomp)
  dev.off()
}

# forecast
if (switch_savegraphs == 1) {

  titlename <- paste0("Nowcasts ", str_forecast, " (equal-weight pool)")
  savename <- paste0("nowcasts_", str_forecast, "_equalweightpool")
  fc_figs <- f_graphnowcastevolve(apply(results$forecast$new[1,,], 1, mean),
                                  apply(results$forecast$impact_by_group, c(1,2), mean),
                                  matrix(c(0, apply(results$forecast$revised_data[1,2:ncol(results$forecast$new),], 1, mean)
                                           - apply(results$forecast$new[1,1:(ncol(results$forecast$new)-1),], 1, mean)), 1),
                                  titlename,
                                  as.Date(unlist(results$vintages), "%d-%b-%Y"),
                                  options$groupnames)
  pdf(paste0(dir_nowcast, "/graphs/", savename, ".pdf"), width = 9, height = 5 )
  print(fc_figs$fig_casts)
  print(fc_figs$fig_newsdecomp)
  dev.off()
}

###  Plot fan charts ###

if (switch_savegraphs == 1) {

  nametitle <- paste0("fan chart, nowcasts ", str_nowcast)
  nw_pfc <- f_plotfanchart(drop(results$nowcast$new), nametitle, unlist(results$vintages))

  nametitle <- paste("fan chart, nowcasts", str_forecast)
  fc_pfc <- f_plotfanchart(drop(results$forecast$new), nametitle, unlist(results$vintages))

  pdf(paste0(dir_nowcast, "/graphs/fancharts.pdf"), width = 9, height = 5)
  print(nw_pfc)
  print(fc_pfc)
  dev.off()
}

###  assemble forecast revision docu and store as txt-file  ###

if (switch_savedocus == 1) {
  flag_ewpool <- 1
  threshold <- 0.02

  # nowcasts
  ew_pool_details <- array(list(), dim = c(Nvintages))

  for (v in 2:Nvintages) {
    temp_forecasts <- matrix(0,0,0)
    temp_weights <-  matrix(0,0,0)
    temp_impacts <-  matrix(0,0,0)
    for (m in 1:Nmodels) {
      temp_forecasts <- mcbind(temp_forecasts, results$nowcast$details[[v, m]]$forecasts)
      temp_weights <- mcbind(temp_weights, results$nowcast$details[[v, m]]$weights)
      temp_impacts <- mcbind(temp_impacts, results$nowcast$details[[v, m]]$impacts)
      if (m == 1) {
        ew_pool_details[[v]]$varnames <- results$nowcast$details[[v, m]]$varnames
        ew_pool_details[[v]]$actuals <- results$nowcast$details[[v, m]]$actuals
      }
    }
    ew_pool_details[[v]]$forecasts <- as.matrix(rowMeans(temp_forecasts))
    ew_pool_details[[v]]$weights <- as.matrix(rowMeans(temp_weights))
    ew_pool_details[[v]]$impacts <- as.matrix(rowMeans(temp_impacts))
  }

  savename <- paste0("nowcasts_", str_nowcast, "_equalweightpool")
  f_docu(apply(results$nowcast$new[1,,], 1, mean),
         unlist(results$vintages),
         ew_pool_details,
         options,
         savename,
         flag_ewpool,
         threshold,
         dir_nowcast,
         str_nowcast)
  f_docuII(apply(results$nowcast$new[1,,], 1, mean),
           unlist(results$vintages),
           ew_pool_details,
           options,
           savename,
           flag_ewpool,
           dir_nowcast,
           str_nowcast)

  # forecasts
  ew_pool_details <- array(list(), dim = c(Nvintages))

  for (v in 2:Nvintages) {
    temp_forecasts <- matrix(0,0,0)
    temp_weights <-  matrix(0,0,0)
    temp_impacts <-  matrix(0,0,0)
    for (m in 1:Nmodels) {
      temp_forecasts <- mcbind(temp_forecasts, results$forecast$details[[v, m]]$forecasts)
      temp_weights <- mcbind(temp_weights, results$forecast$details[[v, m]]$weights)
      temp_impacts <- mcbind(temp_impacts, results$forecast$details[[v, m]]$impacts)
      if (m == 1) {
        ew_pool_details[[v]]$varnames <- results$forecast$details[[v, m]]$varnames
        ew_pool_details[[v]]$actuals <- results$forecast$details[[v, m]]$actuals
      }
    }
    ew_pool_details[[v]]$forecasts <- as.matrix(rowMeans(temp_forecasts))
    ew_pool_details[[v]]$weights <- as.matrix(rowMeans(temp_weights))
    ew_pool_details[[v]]$impacts <- as.matrix(rowMeans(temp_impacts))
  }

  savename <- paste0("nowcasts_", str_forecast, "_equalweightpool")
  f_docu(apply(results$forecast$new[1,,], 1, mean),
         unlist(results$vintages),
         ew_pool_details,
         options,
         savename,
         flag_ewpool,
         threshold,
         dir_nowcast,
         str_forecast)
  f_docuII(apply(results$forecast$new[1,,], 1, mean),
           unlist(results$vintages),
           ew_pool_details,
           options,
           savename,
           flag_ewpool,
           dir_nowcast,
           str_forecast)
}

### Export tables to xls  ###

if (switch_savetables == 1) {
  
  # nowcast
  savename <- paste0("nowcasts_", str_nowcast, "_equalweightpool")
  f_table(apply(results$nowcast$new[,,], 1, mean),
          matrix(c(0, apply(results$nowcast$revised_data[1,2:ncol(results$nowcast$new),], 1, mean)
                   - apply(results$nowcast$new[1,1:(ncol(results$nowcast$new)-1),], 1, mean)), 1),
          apply(results$nowcast$impact_by_group, c(1,2), mean),
          results$vintages,
          savename,
          options$groupnames,
          dir_nowcast)

  # forecast
  savename <- paste0("nowcasts_", str_forecast, "_equalweightpool")
  f_table(apply(results$forecast$new[,,], 1, mean),
          matrix(c(0, apply(results$forecast$revised_data[1,2:ncol(results$forecast$new),], 1, mean)
                   - apply(results$forecast$new[1,1:(ncol(results$forecast$new)-1),], 1, mean)), 1),
          apply(results$forecast$impact_by_group, c(1,2), mean),
          results$vintages,
          savename,
          options$groupnames,
          dir_nowcast)
}

cat("##- Done estimating models and generating results! \n")







