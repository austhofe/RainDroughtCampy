#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(data.table)

#### Merging all datasets from 4 states
final_co_df_analysis <- read_csv("data/final_co_df_analysis.csv")
final_az_df_analysis <- read_csv("data/final_az_df_analysis.csv")
final_nm_df_analysis <- read_csv("data/final_nm_df_analysis.csv")
final_ut_df_analysis <- read_csv("data/final_ut_df_analysis.csv")
dfs <- list(final_co_df_analysis, final_az_df_analysis, final_nm_df_analysis, final_ut_df_analysis)
complete_daily_df <- rbindlist(dfs, use.names = TRUE, fill=TRUE)
write.csv(complete_daily_df, "data\\complete_daily_df.csv", row.names = T)

final_co_df_analysis_week <- read_csv("data/final_co_df_analysis_week.csv")
final_az_df_analysis_week <- read_csv("data/final_az_df_analysis_week.csv")
final_nm_df_analysis_week <- read_csv("data/final_nm_df_analysis_week.csv")
final_ut_df_analysis_week <- read_csv("data/final_ut_df_analysis_week.csv")
dfs <- list(final_az_df_analysis_week, final_co_df_analysis_week, final_nm_df_analysis_week, final_ut_df_analysis_week)
complete_weekly_df <- rbindlist(dfs, use.names = TRUE, fill=TRUE)
write.csv(complete_weekly_df, "data\\complete_weekly_df.csv", row.names = T)

final_co_df_analysis_month <- read_csv("data/final_co_df_analysis_month.csv")
final_az_df_analysis_month <- read_csv("data/final_az_df_analysis_month.csv")
final_nm_df_analysis_month <- read_csv("data/final_nm_df_analysis_month.csv")
final_ut_df_analysis_month <- read_csv("data/final_ut_df_analysis_month.csv")
dfs <- list(final_az_df_analysis_month, final_co_df_analysis_month, final_nm_df_analysis_month, final_ut_df_analysis_month)
complete_monthly_df <- rbindlist(dfs, use.names = TRUE, fill=TRUE)
write.csv(complete_monthly_df, "data\\complete_monthly_df.csv", row.names = T)

## Calculate PET
dat.s <- split(complete_monthly_df, complete_monthly_df$FIPS) #split df by lat
dat.s$spei <- lapply(dat.s, function(x) x$`ppt (inches)`-(thornthwaite(x$`tmean (degrees F)`, x$lat[1]))) 
complete_monthly_df <- do.call(rbind, dat.s)

dat.s <- ts(dat.s[,-c(1,2)], end=c(2021,12), frequency=12)
dat.s$spei12 <- lapply(dat.s, function(x)
complete_monthly_df <- do.call(rbind, dat.s)