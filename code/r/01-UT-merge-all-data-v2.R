#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
# pacman::p_load("zipcodeR")


#### FUNCTIONS ####
# PRISM function 
read.prism.csv.and.add.filename <- function(filepath){
  read_csv(filepath, skip = 10) %>%
    mutate(filepath=filepath)
}
# Drought Function
read.drought.csv.and.add.filename <- function(filepath){
  read_csv(filepath, skip = 3) %>%
    mutate(filepath=filepath)
}

#### Import County data information ####
ut_county_info <- read_excel("data/01_ut_county-info.xlsx")

#### Import and Export CO Campy and Salmonella Data ####
health <- read_excel("data/HealthData/UT2012-2021.xlsx")

# zipdf <- zip_code_db
# 
# health <- merge(x=health, y=zipdf, by.x="Zip Code", by.y="zipcode", all.x=T, na.omit())
# mycountyvars <- c("Zip Code", "Disease", "Status", "DtEntered", "county")
# health <- health[mycountyvars]

health$DtEntered <- date(health$DtEntered)

# #### Cleaning County
# health$county <- sub(" .*", "", health$county)
# counties = c('Salt'='Salt Lake', 'San'='San Juan', 'Box'='Box Elder')
# health$county <- str_replace_all(health$county, counties)


#### Creating separate dataframes for Campy and Sal information ####
healthdf_campy <- health %>%
  mutate(day = as.Date(health$DtEntered, format="%Y-%m-%d")) %>%
  mutate(campycount = case_when(health$Pathogen == 'Campylobacteriosis' ~1,
                                health$Pathogen == 'Salmonellosis' ~ 0)) %>%
  group_by(day, health$County) %>% 
  summarise(total__campy_cases=sum(campycount)) %>%
  na.omit() 

## Salmonella
healthdf_sal <- health %>%
  mutate(day = as.Date(health$DtEntered, format="%Y-%m-%d")) %>%
  mutate(salcount = case_when(health$Pathogen == 'Salmonellosis' ~1,
                              health$Pathogen == 'Campylobacteriosis' ~ 0)) %>%
  group_by(day, health$County) %>% 
  summarise(total__sal_cases=sum(salcount)) %>% 
  na.omit() 

# Clean up names in the dataframes
names(healthdf_campy)[names(healthdf_campy) == 'day'] <- 'date'
names(healthdf_campy)[2] <- "county"
names(healthdf_sal)[2] <- "county"
names(healthdf_sal)[names(healthdf_sal) == 'day'] <- 'date'

#Merging in county info for merging
clean_campy_data <- healthdf_campy %>% 
  left_join(ut_county_info, by=c("county"))

clean_sal_data <- healthdf_sal %>% 
  left_join(ut_county_info, by=c("county"))

myhealthvars <- c("date", "total__campy_cases", "FIPS")
clean_campy_data <- clean_campy_data[myhealthvars]
myhealthvars <- c("date", "total__sal_cases", "FIPS")
clean_sal_data <- clean_sal_data[myhealthvars]
clean_campy_data$FIPS <- as.numeric(clean_campy_data$FIPS)
clean_sal_data$FIPS <- as.numeric(clean_sal_data$FIPS)

#### IMPORT AND MERGE DAILY PRISM Data ####
# Import the file names so that you can loop through them
raw_prism_files <- data_frame(filename = list.files('data/PRISMData/UTDailyObserved/'))

raw_prism_file_paths <- raw_prism_files  %>%
  mutate(filepath = paste0("data/PRISMData/UTDailyObserved/", filename))

# Make sure the file paths and names worked
raw_prism_file_paths %>%
  head(3)

# Use the prism function to read in the CSVs
raw_prism_data <- raw_prism_file_paths %>%
  rowwise() %>%
  do(., read.prism.csv.and.add.filename(file=.$filepath))

# Extract data from the filenames for merging and matching with other datasets
raw_prism_data$lat <- substr(raw_prism_data$filepath,87,93)
raw_prism_data$long <- substr(raw_prism_data$filepath,95,103)

raw_prism_data$lat <-as.numeric(raw_prism_data$lat)
raw_prism_data$long <-as.numeric(raw_prism_data$long)

# Merge in county information
clean_prism_data <- raw_prism_data %>% 
  left_join(ut_county_info, by=c("lat","long"))

myprismvars <- names(clean_prism_data) %in% c("LGID", "POSTAL_FIPS_ID...3", "POSTAL_FIPS_ID...7", "NCDC_FIPS_ID", "filepath")
clean_prism_data = clean_prism_data[!myprismvars]

#rename variables for merging
names(clean_prism_data)[names(clean_prism_data) == 'Date'] <- 'date'
names(clean_prism_data)[names(clean_prism_data) == "CLIMDIV_ID"] <- "ClimDiv"

#clean up date field variables
clean_prism_data$year <- format(as.Date(clean_prism_data$date), "%Y")
clean_prism_data$year <- as.numeric(clean_prism_data$year)

clean_prism_data$month <- format(as.Date(clean_prism_data$date), "%m")
clean_prism_data$month <- as.numeric(clean_prism_data$month)

clean_prism_data$day <- format(as.Date(clean_prism_data$date), "%d")
clean_prism_data$day <- as.numeric(clean_prism_data$day)

clean_prism_data$FIPS <- as.numeric(clean_prism_data$FIPS)


#### IMPORT AND MERGE 30 YEAR NORMAL PRISM Data ####
# Import the file names so that you can loop through them
raw_prism30_files <- data_frame(filename = list.files('data/PRISMData/UT30YearNormals/'))

raw_prism30_file_paths <- raw_prism30_files  %>%
  mutate(filepath = paste0("data/PRISMData/UT30YearNormals/", filename))

# Make sure the file paths and names worked
raw_prism30_file_paths %>%
  head(3)

# Use the prism function to read in the CSVs
raw_prism30_data <- raw_prism30_file_paths %>%
  rowwise() %>%
  do(., read.prism.csv.and.add.filename(file=.$filepath))

# Extract data from the filenames for merging and matching with other datasets
raw_prism30_data$lat <- substr(raw_prism30_data$filepath,85,91)
raw_prism30_data$long <- substr(raw_prism30_data$filepath,93,101)

raw_prism30_data$lat <-as.numeric(raw_prism30_data$lat)
raw_prism30_data$long <-as.numeric(raw_prism30_data$long)

# Merge in county information
clean_prism30_data <- raw_prism30_data %>% 
  left_join(ut_county_info, by=c("lat","long"))



names(clean_prism30_data)[names(clean_prism30_data) == 'month'] <- 'Date'
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="January", 01, NA)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="February", 2, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="March", 3, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="April", 4, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="May", 5, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="June", 6, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="July", 7, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="August", 8, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="September", 9, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="October", 10, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="November", 11, clean_prism30_data$month)
clean_prism30_data$month <- ifelse(clean_prism30_data$Date=="December", 12, clean_prism30_data$month)

#Create a subset of the data
myvars <- names(clean_prism30_data) %in% c("LGID", "lat", "long", "county", "CLIMDIV_ID", "POSTAL_FIPS_ID...3", "POSTAL_FIPS_ID...7", "NCDC_FIPS_ID", "elevation", "filepath", "POP_COU",	"POPPCT_URBAN",	"POPPCT_UC", "POPPCT_UA", "POPPCT_RURAL", "Urban")
clean_prism30_data = clean_prism30_data[!myvars]

#Clean up variable names
names(clean_prism30_data)[names(clean_prism30_data) == 'ppt (inches)'] <- 'ppt30avg'
names(clean_prism30_data)[names(clean_prism30_data) == 'tmin (degrees F)'] <- 'tmin30avg'
names(clean_prism30_data)[names(clean_prism30_data) == 'tmax (degrees F)'] <- 'tmax30avg'
names(clean_prism30_data)[names(clean_prism30_data) == 'tmean (degrees F)'] <- 'tmean30avg'
names(clean_prism30_data)[names(clean_prism30_data) == 'Date'] <- 'PRISM30avgMonth'
clean_prism30_data$FIPS <- as.numeric(clean_prism30_data$FIPS)





#### Import and Export merged SoVI data ####
# For 2000 and 2010, SoVI is captured at CT level. So we need to average to get to county.
raw_SoVI_2010 <- read_csv("data/SoVI/Utah_CT2010.csv")
SoVI_2010 <- raw_SoVI_2010[!(raw_SoVI_2010$STATE_NAME=="49" | raw_SoVI_2010$R_PL_THEMES=="-999"),]

clean_SoVI_2010 <- SoVI_2010 %>%
  group_by(STCOFIPS) %>%
  summarise(across(R_PL_THEMES, mean, 
                   STCOFIPS = unique(STCOFIPS),
                   na.rm=TRUE))
# Renaming variables to help with merging later
names(clean_SoVI_2010)[names(clean_SoVI_2010) == "STCOFIPS"] <- "FIPS"
names(clean_SoVI_2010)[names(clean_SoVI_2010) == "R_PL_THEMES"] <- "RPL_THEMES"
clean_SoVI_2010$year <- "2010"
# Create new datasets for other years
clean_SoVI_2009 <- clean_SoVI_2010
clean_SoVI_2009$year <- "2009"
clean_SoVI_2011 <- clean_SoVI_2010
clean_SoVI_2011$year <- "2011"
clean_SoVI_2012 <- clean_SoVI_2010
clean_SoVI_2012$year <- "2012"
clean_SoVI_2013 <- clean_SoVI_2010
clean_SoVI_2013$year <- "2013"

# Import county-level SoVI for other years
raw_SoVI_2014 <- read_csv("data/SoVI/Utah_COUNTY2014.csv")
clean_SoVI_2014 <- raw_SoVI_2014 %>%
  select(FIPS,RPL_THEMES)
clean_SoVI_2014$year <- "2014"

raw_SoVI_2016 <- read_csv("data/SoVI/Utah_COUNTY2016.csv")
clean_SoVI_2016 <- raw_SoVI_2016 %>%
  select(FIPS,RPL_THEMES)
clean_SoVI_2016$year <- "2016"

raw_SoVI_2018 <- read_csv("data/SoVI/Utah_COUNTY2018.csv")
clean_SoVI_2018 <- raw_SoVI_2018 %>%
  select(FIPS,RPL_THEMES)
clean_SoVI_2018$year <- "2018"

# Create dataframes for missing years
clean_SoVI_2015 <- clean_SoVI_2014
clean_SoVI_2015$year <- "2015"

clean_SoVI_2017 <- clean_SoVI_2016
clean_SoVI_2017$year <- "2017"

clean_SoVI_2019 <- clean_SoVI_2018
clean_SoVI_2019$year <- "2019"

clean_SoVI_2020 <- clean_SoVI_2018
clean_SoVI_2020$year <- "2020"

clean_SoVI_2021 <- clean_SoVI_2018
clean_SoVI_2021$year <- "2021"

# Create a new dataframe, df that stores all of the individual SoVI data in a dataframe
sovidf <- mget(ls(pattern = "clean_SoVI_")) %>% 
  map_df(I, .id = "src")

sovidf$year <- as.numeric(sovidf$year)
sovidf$FIPS <- as.numeric(sovidf$FIPS)










#### Import and Export Drought Indices ####
# PDSI
# Import the file names so that you can loop through them
raw_pdsi_files <- data_frame(filename = list.files('data/NOAA/NCEI/UT/', pattern = '-pdsi'))

raw_pdsi_file_paths <- raw_pdsi_files  %>%
  mutate(filepath = paste0("data/NOAA/NCEI/UT/", filename))

# Make sure the file paths and names worked
raw_pdsi_file_paths %>%
  head(3)

# Use the drought function to read in the CSVs
raw_pdsi_data <- raw_pdsi_file_paths %>%
  rowwise() %>%
  do(., read.drought.csv.and.add.filename(file=.$filepath))

# Extract data from the filenames for merging and matching with other datasets
raw_pdsi_data$ClimDiv <- substr(raw_pdsi_data$filepath,19,22)
raw_pdsi_data$Index <- substr(raw_pdsi_data$filepath,24,27)

raw_pdsi_data$year <- substr(raw_pdsi_data$Date,1,4)
raw_pdsi_data$year <- as.numeric(raw_pdsi_data$year)

raw_pdsi_data$month <- substr(raw_pdsi_data$Date,5,6)
raw_pdsi_data$month <- as.numeric(raw_pdsi_data$month)

mydroughtvars <- names(raw_pdsi_data) %in% c("filepath", "Date")
clean_pdsi_data = raw_pdsi_data[!mydroughtvars]

#Z Index
# Import the file names so that you can loop through them
raw_zndx_files <- data_frame(filename = list.files('data/NOAA/NCEI/UT/', pattern = '-zndx'))

raw_zndx_file_paths <- raw_zndx_files  %>%
  mutate(filepath = paste0("data/NOAA/NCEI/UT/", filename))

# Make sure the file paths and names worked
raw_zndx_file_paths %>%
  head(3)

# Use the function to read in the CSVs
raw_zndx_data <- raw_zndx_file_paths %>%
  rowwise() %>%
  do(., read.drought.csv.and.add.filename(file=.$filepath))

# Extract data from the filenames for merging and matching with other datasets
raw_zndx_data$ClimDiv <- substr(raw_zndx_data$filepath,19,22)
raw_zndx_data$Index <- substr(raw_zndx_data$filepath,24,27)
raw_zndx_data$year <- substr(raw_zndx_data$Date,1,4)
raw_zndx_data$year <- as.numeric(raw_zndx_data$year)
raw_zndx_data$month <- substr(raw_zndx_data$Date,5,6)
raw_zndx_data$month <- as.numeric(raw_zndx_data$month)

mydroughtvars <- names(raw_zndx_data) %in% c("filepath", "Date")
clean_zndx_data = raw_zndx_data[!mydroughtvars]








#### MERGING IN ALL DATA ####
final_ut_df <- clean_prism_data %>% 
  left_join(clean_prism30_data, by=c("month", "FIPS")) %>%
  left_join(clean_campy_data, by=c("date","FIPS")) %>%
  left_join(clean_sal_data, by=c("date","FIPS")) %>%
  left_join(clean_pdsi_data, by=c("year", "month", "ClimDiv")) %>%
  left_join(clean_zndx_data, by=c("year", "month", "ClimDiv")) %>%
  left_join(sovidf, by=c("FIPS","year"))



#### CLEANING UP FOR ANALYSIS ####
final_ut_df_analysis <- filter(final_ut_df, year>=2012)

final_ut_df_analysis$enso <- ifelse(final_ut_df_analysis$year==2006 | final_ut_df_analysis$year==2007 | final_ut_df_analysis$year==2009 | final_ut_df_analysis$year==2010 | final_ut_df_analysis$year==2014 | final_ut_df_analysis$year==2015 | final_ut_df_analysis$year==2016 | final_ut_df_analysis$year==2018 | final_ut_df_analysis$year==2019,1,0)


#### EXPORTING DATA FRAME ####
write.csv(final_ut_df_analysis, "data\\final_ut_df_analysis.csv", row.names = T)
