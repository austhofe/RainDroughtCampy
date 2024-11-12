#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)

health$DtEntered <- ymd(health$DtEntered)
health$week <- week(health$DtEntered)
health$year <- year(health$DtEntered)
health$month <- month(health$DtEntered)


#### Creating separate dataframes for Campy and Sal information by WEEK####
healthdf_campy_week <- health %>%
  mutate(campycount = case_when(health$Pathogen == 'CMP' ~1,
                                health$Pathogen == 'SLM' ~ 0)) %>%
  group_by(week, year, County) %>%
  summarise(total__campy_cases=sum(campycount)) %>%
  na.omit() 


## Salmonella
healthdf_sal_week <- health %>%
  mutate(salcount = case_when(health$Pathogen == 'SLM' ~1,
                              health$Pathogen == 'CMP' ~0)) %>%
  group_by(week, year, County) %>% 
  summarise(total__sal_cases=sum(salcount)) %>%
  na.omit() 







#Merging in county info for merging
clean_campy_data_week <- healthdf_campy_week %>% 
  left_join(az_county_info, by=c("County" = "county"))

clean_sal_data_week <- healthdf_sal_week %>% 
  left_join(az_county_info, by=c("County" = "county"))



myhealthvars_week <- c("week", "year", "total__campy_cases", "FIPS")
clean_campy_data_week <- clean_campy_data_week[myhealthvars_week]


myhealthvars_week <- c("week", "year", "total__sal_cases", "FIPS")
clean_sal_data_week <- clean_sal_data_week[myhealthvars_week]

count(clean_campy_data_week, year, week, FIPS) %>%
  filter(n>1)

#### PRISM weekly data
clean_prism_data$week <-week(clean_prism_data$date)
clean_prism_data$month <-month(clean_prism_data$date)

clean_prism_data_week <- clean_prism_data %>%
  group_by(week, year, FIPS) %>%
  summarise(
    maxtemp = max(`tmax (degrees F)`),
    mintemp = min(`tmin (degrees F)`),
    avgtemp = mean(`tmean (degrees F)`),
    avgppt = mean(`ppt (inches)`),
    maxppt = max(`ppt (inches)`),
    minppt = min(`ppt (inches)`),
    sumppt = sum(`ppt (inches)`),
  )



#### merge county info
clean_prism_data_week <- clean_prism_data_week %>%
  left_join(az_county_info, by=c("FIPS"))
myprismvars <- names(clean_prism_data_week) %in% c("LGID", "POSTAL_FIPS_ID...4", "POSTAL_FIPS_ID...8", "NCDC_FIPS_ID", "filepath")
clean_prism_data_week = clean_prism_data_week[!myprismvars]
names(clean_prism_data_week)[names(clean_prism_data_week) == "CLIMDIV_ID"] <- "ClimDiv"

#### Date data to add in month to weekly
mydatevars <- names(clean_prism_data) %in% c("year", "month", "week")
az_dates = unique(clean_prism_data[mydatevars])

clean_prism_data_week$ID <- seq_along(clean_prism_data_week[[1]])

#### MERGING IN ALL DATA BY WEEK AND MONTH
final_az_df_week <- clean_prism_data_week %>% 
  left_join(clean_campy_data_week, by=c("week", "year", "FIPS")) %>% 
  left_join(clean_sal_data_week, by=c("week", "year", "FIPS")) %>%
  left_join(az_dates, by=c("week", "year")) %>%
  left_join(clean_prism30_data, by=c("month", "FIPS")) %>%
  left_join(clean_pdsi_data, by=c("year", "month", "ClimDiv")) %>%
  left_join(clean_zndx_data, by=c("year", "month", "ClimDiv")) %>%
  left_join(sovidf, by=c("FIPS","year"))

#### CLEANING UP FOR ANALYSIS ####
final_az_df_analysis_week <- filter(final_az_df_week, year>=2009)

final_az_df_analysis_week$enso <- ifelse(final_az_df_analysis_week$year==2006 | final_az_df_analysis_week$year==2007 | final_az_df_analysis_week$year==2009 | final_az_df_analysis_week$year==2010 | final_az_df_analysis_week$year==2014 | final_az_df_analysis_week$year==2015 | final_az_df_analysis_week$year==2016 | final_az_df_analysis_week$year==2018 | final_az_df_analysis_week$year==2019,1,0)

#### EXPORTING DATA FRAME ####
write.csv(final_az_df_analysis_week, "data\\final_az_df_analysis_week.csv", row.names = T)
