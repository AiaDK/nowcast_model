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
source.all(paste0(dir_root, "/model/functions"))

dir_monthly_gdp <- paste0(dir_root, '/Nowcasts/', year_nowcast, 'Q', quarter_nowcast, '/monthlyGDP')

Nrs <- read.csv('../model_specs_Nrs.csv', header = FALSE)
Nps <- read.csv('../model_specs_Nps.csv', header = FALSE)
Njs <- read.csv('../model_specs_Njs.csv', header = FALSE)

specs <- character(0)
for (Nr in Nrs) {
  for (Np in Nps) {
    for (Nj in Njs) {
      specs <- c(specs, paste0('Nr', Nr, '_Np', Np, '_Nj', Nj))
    }
  }
}

vintages <- read.table("../dates_vintages.txt", header = FALSE)
vintages <- as.character(vintages[, 1])
vintage <- vintages[length(vintages)]

gdp_mms <- matrix(NA, 0, ncol = length(specs)) 
gdp_3m3ms <- matrix(NA, 0, ncol = length(specs))  

for (s in 1:length(specs)) {
  load(file.path(dir_monthly_gdp, vintage, paste0('monthlyGDP_', specs[s], '.RData')))
  gdp_mms <- mcbind(gdp_mms, t(gdp_mm))
  gdp_3m3ms <- mcbind(gdp_3m3ms, t((std_gdp * gdp_3m3m + mean_gdp)))
}


# Monats_BIP_mm

# plot all monthly data
gdp_mm_avg <- matrix(rowMeans(gdp_mms, na.rm = TRUE))

df_mmm_all <- data.frame(date = dates_converted, value = gdp_mm_avg)
df_mmm_all <- cbind(df_mmm_all, read.zoo(text = dates_converted, FUN = as.yearmon))
df_mmm_all <- tibble::rownames_to_column(df_mmm_all, "date_jan")
ind <- grepl("Jan ", df_mmm_all$date_jan)
df_mmm_all$date_jan[!ind] <- ""

p_mmm <- ggplot(df_mmm_all, aes(x = date, y = value, group=1)) +
  geom_line(color = 'blue') +
  scale_x_discrete(breaks = df_mmm_all$date, labels = df_mmm_all$date_jan) +
  labs(title = 'GDP m/m, corresponding to standardized q/q growth rate, mean over all specifications',
       x = element_blank(),
       y = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
        plot.title = element_text(size = 12, hjust = 0.5)) 

# plot last 12 months
ind_last12m <- (length(gdp_mm_avg)-11) : length(gdp_mm_avg)
df_mmm <- data.frame(date = read.zoo(text = dates_converted[ind_last12m], FUN = as.yearmon), value = gdp_mm_avg[ind_last12m])
df_mmm <- tibble::rownames_to_column(df_mmm, "date")

p_mmm_bar <- ggplot(df_mmm, aes(x = factor(date, levels=date), y = value)) +
  geom_bar(stat = 'identity', fill = 'blue', width = 0.7) +
  scale_x_discrete(breaks = df_mmm$date) +
  labs(title = 'GDP m/m last 12 months, corresponding to standardized q/q growth rate, mean over all specifications',
       x = element_blank(),
       y = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
        plot.title = element_text(size = 12, hjust = 0.5)) 

# print plots into pdf file
pdf(paste0(dir_monthly_gdp, "/Monats_BIP_mm.pdf"), width = 9, height = 5 )
print(p_mmm)
print(p_mmm_bar)
dev.off()


# Monats_BIP_qq

# plot all quarterly data
gdp_3m3m_avg <- matrix(rowMeans(gdp_3m3ms, na.rm = TRUE))

df_qqq <- data.frame(date = dates_converted, value = gdp_3m3m_avg)
df_qqq <- cbind(df_qqq, read.zoo(text = dates_converted, FUN = as.yearmon))
df_qqq <- tibble::rownames_to_column(df_qqq, "date_jan")
ind <- grepl("Jan ", df_qqq$date_jan)
df_qqq$date_jan[!ind] <- ""

df_realizations <- data.frame(date = dates_converted, value = gdp_realizations)
df_realizations <- cbind(df_realizations, read.zoo(text = dates_converted, FUN = as.yearmon))
df_realizations <- tibble::rownames_to_column(df_realizations, "date_jan")
ind <- grepl("Jan ", df_realizations$date_jan)
df_realizations$date_jan[!ind] <- ""


p_qqq <- ggplot(df_qqq, aes(x = date, y = value, group=1)) +
  geom_line(color = 'blue') +
  geom_point(data = df_realizations, aes(y = value), color = 'blue', size = 0.8) + # Warning message: Removed 224 rows containing missing values (`geom_point()`).
  scale_x_discrete(breaks = df_qqq$date, labels = df_qqq$date_jan) +
  labs(title = 'GDP monthly q/q, mean over all specifications',
       x = element_blank(),
       y = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
        plot.title = element_text(size = 12, hjust = 0.5)) 


# plot last 12 month
gdp_3m3m_avg_short <- matrix(gdp_3m3m_avg[ind_last12m])
dates_short <- matrix(dates_converted[ind_last12m])
ind_eoq <- matrix(grepl('-3|-6|-9|-12', dates_short))
tmp <- cbind(gdp_3m3m_avg_short, gdp_3m3m_avg_short)
tmp[ind_eoq, 1] <- NA
tmp[!ind_eoq, 2] <- NA

df_qqq_stacked <- data.frame(date = read.zoo(text = dates_short, FUN = as.yearmon), value1 = tmp[, 1], value2 = tmp[, 2])
df_qqq_stacked <- tibble::rownames_to_column(df_qqq_stacked, "date")

p_qqq_stacked <- ggplot(df_qqq_stacked, aes(x = factor(date, levels=date))) +
  geom_bar(aes(y = value1), stat = 'identity', fill = 'blue', width = 0.7) +
  geom_bar(aes(y = value2), stat = 'identity', fill = 'orange', width = 0.7) +
  scale_x_discrete(breaks = df_qqq_stacked$date) +
  labs(title = 'GDP monthly q/q last 12 months',
       x = element_blank(),
       y = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5),
        plot.title = element_text(size = 12, hjust = 0.5))

# print plots into pdf file
pdf(paste0(dir_monthly_gdp, "/Monats_BIP_qq.pdf"), width = 9, height = 5 )
print(p_qqq)
print(p_qqq_stacked)
dev.off()

print("##- Done plotting monthly GDP")
  
  

