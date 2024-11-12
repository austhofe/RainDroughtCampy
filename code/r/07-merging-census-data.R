#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(data.table)
library(stringr)
library(tidyr)

##Functions

##Data Cleaning
census_2009 <- read_csv("data/co-est2000-2009-alldata.csv")
census_2009 <- census_2009 %>%
  select(starts_with(c("SUMLEV",	"REGION",	"DIVISION",	"STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE")))


census_2019 <- read_csv("data/co-est2010-2019-alldata.csv")
census_2019 <- census_2019 %>%
  select(starts_with(c("SUMLEV",	"REGION",	"DIVISION",	"STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE")))

census_2022 <- read_csv("data/co-est2020-2022-alldata.csv")
census_2022 <- census_2022 %>%
  select(starts_with(c("SUMLEV",	"REGION",	"DIVISION",	"STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE")))

census_all <- merge(census_2009, census_2019, by=c("SUMLEV",	"REGION",	"DIVISION",	"STATE", "COUNTY", "STNAME", "CTYNAME"))
census_all <- merge(census_all, census_2022, by=c("SUMLEV",	"REGION",	"DIVISION",	"STATE", "COUNTY", "STNAME", "CTYNAME"))

census_clean <- filter(census_all, STNAME=="Arizona" | STNAME=="New Mexico" | STNAME=="Utah"  | STNAME=="Colorado")
census_clean <- filter(census_clean, COUNTY!=0)
census_clean <- census_clean %>%
  select(starts_with(c("STNAME", "CTYNAME", "POPESTIMATE")))

popestimates <- census_clean %>%
  pivot_longer(
    cols = starts_with("POPESTIMATE"),
    names_to = "year",
    names_prefix = "POPESTIMATE",
    values_to = "population",
    values_drop_na = FALSE
  )

popestimates <- popestimates %>%
  mutate(county = tolower(CTYNAME)) %>%
  mutate(county = sub("\\s+[^ ]+$", "", county)) %>%
  mutate(state_abbr =  recode(STNAME, "Arizona"= "AZ", "New Mexico"="NM", "Utah"= "UT", "Colorado"="CO"))

popestimates <- popestimates %>% select(year, population, county, state_abbr)
write.csv(popestimates, "data\\popestimates.csv", row.names = T)
