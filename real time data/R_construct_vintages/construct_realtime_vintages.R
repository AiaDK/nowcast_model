args <- commandArgs(trailingOnly = TRUE)
dir_realtimedata <- args[1]

### Install needed libraries ###
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
library(tidyr)


source.all("functions") #real time data/R_construct_vintages/

dir_rawdata <- paste0(dir_realtimedata, "/raw data")
dir_vintages <- paste0(dir_realtimedata, "/vintages")
vintages <- read.table("../../dates_vintages.txt", header = FALSE)
vintages <- as.character(vintages[, 1]) 

for (v in 1:length(vintages)) {
  dataset <- list() 
  # toc
  dataset$data_ifo <- f_load_ifo(vintages[v], dir_rawdata) 
  # toc
  dataset$data_ESIBCI <- f_load_ESIBCI(vintages[v], dir_rawdata)
  # toc
  dataset$data_BuBaRTD <- f_load_BuBaRTD(vintages[v], dir_rawdata)
  # toc
  dataset$data_BuBaRTD <- f_load_turnover_hospitality(dataset$data_BuBaRTD, vintages[v], dir_rawdata)
  # toc
  dataset$data_BuBaRTD <- f_load_lkw_maut_index(dataset$data_BuBaRTD, vintages[v], dir_rawdata)
  # toc
  dataset$data_financial <- f_load_financial(vintages[v], dir_rawdata)
  # toc
  dataset$vintagedate <- as.Date(vintages[v], "%d-%b-%Y")
  # save dataset to Rdata
  save(dataset, file = paste0(dir_vintages,  "/dataset_", format(as.Date(vintages[v], "%d-%b-%Y"), format = "%Y_%m_%d"), ".RData"))
  rm(dataset)
}

cat("##- Done constructing real-time vintages! \n")





