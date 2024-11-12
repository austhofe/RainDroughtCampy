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
usdavars <- c("Program", "Year", "State", "County", "Commodity", "Data Item", "Domain", "Value")
finalusdavars <- c("Year", "State", "County", "Commodity", "Data Item", "Value")

## Load data
cattle_inv <- read_csv("data/USDAAnimalData/cattle.csv")
cattle_inv <- cattle_inv[usdavars]
cattle_inv <- filter(cattle_inv, Domain == "TOTAL" & Value != "(D)" & County != "OTHER COUNTIES" & (`Data Item` == "CATTLE, COWS, MILK - INVENTORY" | `Data Item` == "CATTLE, COWS, BEEF - INVENTORY" | `Data Item` == "CATTLE, INCL CALVES - INVENTORY"))
cattle_inv <- cattle_inv[finalusdavars]

chickens_inv <- read_csv("data/USDAAnimalData/chickens.csv")
chickens_inv <- chickens_inv[usdavars]
chickens_inv <- filter(chickens_inv, Domain == "TOTAL" &  Value != "(D)" & County != "OTHER COUNTIES" & (`Data Item` == "CHICKENS, BROILERS - INVENTORY" | `Data Item` == "CHICKENS, LAYERS - INVENTORY"))
chickens_inv <- chickens_inv[finalusdavars]

goats_inv <- read_csv("data/USDAAnimalData/goats.csv")
goats_inv <- goats_inv[usdavars]
goats_inv <- filter(goats_inv, Value != "(D)" & County != "OTHER COUNTIES" & (`Data Item` == "GOATS, MEAT & OTHER - INVENTORY" | `Data Item` == "GOATS, MILK - INVENTORY"))
goats_inv <- goats_inv[finalusdavars]

sheep_inv <- read_csv("data/USDAAnimalData/sheep.csv")
sheep_inv <- sheep_inv[usdavars]
sheep_inv <- filter(sheep_inv, Domain == "TOTAL" & Value != "(D)" & County != "OTHER COUNTIES" & `Data Item` == "SHEEP, INCL LAMBS - INVENTORY")
sheep_inv <- sheep_inv[finalusdavars]


##Merge dataframes
usdadfs_inv <- list(cattle_inv, chickens_inv, goats_inv, sheep_inv)
usdadf_inv <- rbindlist(usdadfs_inv, use.names = TRUE, fill =TRUE)

usdadf_inv = usdadf_inv %>%
  mutate(County = tolower(County))

usdadf_inv_clean <- usdadf_inv %>% 
  full_join(county_info, by=c('County' = 'county'), keep = TRUE, all.x = TRUE)

popestimates_fips <- popestimates %>%
  full_join(county_info, by=c('county' = 'county'), keep = TRUE, all.x = TRUE)
write.csv(popestimates_fips, "data\\popestimates_fips.csv", row.names = T)


## remove duplicates from merging incorrectly on county/state
usdadf_inv_clean$FIPS <- as.numeric(usdadf_inv_clean$FIPS)
usdadf_inv_clean_AZ <- subset(usdadf_inv_clean, State=="ARIZONA" & (FIPS>=4000 & FIPS<=4028))
usdadf_inv_clean_CO <- subset(usdadf_inv_clean, State=="COLORADO" & (FIPS>=8000 & FIPS<=8126))
usdadf_inv_clean_NM <- subset(usdadf_inv_clean, State=="NEW MEXICO" & (FIPS>=35000 & FIPS<=35062))
usdadf_inv_clean_UT <- subset(usdadf_inv_clean, State=="UTAH" & FIPS>=49000)

##Merge dataframes
state_data <- list(usdadf_inv_clean_AZ, usdadf_inv_clean_CO, usdadf_inv_clean_NM, usdadf_inv_clean_UT)
usdadf_inv_clean_new <- rbindlist(state_data, use.names = TRUE, fill =TRUE)

## clean up NA values in the data converting to new columns for each commodity
usdadf_inv_clean_new$cattle_milk <- ifelse(usdadf_inv_clean_new$`Data Item` == "CATTLE, COWS, MILK - INVENTORY", usdadf_inv_clean_new$Value, NA)
usdadf_inv_clean_new$cattle_beef <- ifelse(usdadf_inv_clean_new$`Data Item` == "CATTLE, COWS, BEEF - INVENTORY", usdadf_inv_clean_new$Value, NA) 
usdadf_inv_clean_new$cattle_total <- ifelse(usdadf_inv_clean_new$`Data Item` == "CATTLE, INCL CALVES - INVENTORY", usdadf_inv_clean_new$Value, NA) 
usdadf_inv_clean_new$ck_broilers <- ifelse(usdadf_inv_clean_new$`Data Item` == "CHICKENS, BROILERS - INVENTORY", usdadf_inv_clean_new$Value, NA) 
usdadf_inv_clean_new$ck_layers <- ifelse(usdadf_inv_clean_new$`Data Item` == "CHICKENS, LAYERS - INVENTORY", usdadf_inv_clean_new$Value, NA) 
usdadf_inv_clean_new$goat_meat <- ifelse(usdadf_inv_clean_new$`Data Item` == "GOATS, MEAT & OTHER - INVENTORY", usdadf_inv_clean_new$Value, NA) 
usdadf_inv_clean_new$goat_milk <- ifelse(usdadf_inv_clean_new$`Data Item` == "GOATS, MILK - INVENTORY", usdadf_inv_clean_new$Value, NA) 
usdadf_inv_clean_new$sheep_total <- ifelse(usdadf_inv_clean_new$`Data Item` == "SHEEP, INCL LAMBS - INVENTORY", usdadf_inv_clean_new$Value, NA) 

usda_inv_bygroup <-usdadf_inv_clean_new %>%
  select(State, county, FIPS, Year, cattle_milk, cattle_beef, cattle_total, ck_broilers, ck_layers, goat_meat, goat_milk, sheep_total) %>%
  group_by(FIPS, Year, State, county) %>%
  arrange(FIPS, Year) %>%
  summarise_each(funs(max=max(., na.rm = TRUE)))
  ## summarize(total_animals = sum(cattle_milk, cattle_beef, cattle_total, ck_broilers, ck_layers, goat_meat, goat_milk, sheep_total))
  ##summarise(across(max=max(., na.rm = TRUE)))
  ##summarise(across(everything()))
  ##summarise_each(funs(max=max(., na.rm = TRUE)))
  ##summarise(total = sum(cattle_milk, cattle_beef, cattle_total, ck_broilers, ck_layers, goat_meat, goat_milk, sheep_total))

usda_inv_bygroup_premean <-usdadf_inv_clean_new %>%
  select(State, county, FIPS, Year, cattle_milk, cattle_beef, cattle_total, ck_broilers, ck_layers, goat_meat, goat_milk, sheep_total) %>%
  group_by(FIPS, Year, State, county) %>%
  arrange(FIPS, Year) %>%
  summarise_each(funs(max=max(., na.rm = TRUE)))
write.csv(usda_inv_bygroup_premean, "data\\animal_density_nomean_4-9-2024.csv", row.names = T)


## apply 2012 and 2017 data retro and prospectively
usda_inv_bygroup$cattle_milk_max <- as.numeric(gsub(",","",usda_inv_bygroup$cattle_milk_max))
usda_inv_bygroup$cattle_beef_max <- as.numeric(gsub(",","",usda_inv_bygroup$cattle_beef_max))
usda_inv_bygroup$cattle_total_max <- as.numeric(gsub(",","",usda_inv_bygroup$cattle_total_max))
usda_inv_bygroup$ck_broilers_max <- as.numeric(gsub(",","",usda_inv_bygroup$ck_broilers_max))
usda_inv_bygroup$ck_layers_max <- as.numeric(gsub(",","",usda_inv_bygroup$ck_layers_max))
usda_inv_bygroup$goat_meat_max <- as.numeric(gsub(",","",usda_inv_bygroup$goat_meat_max))
usda_inv_bygroup$goat_milk_max <- as.numeric(gsub(",","",usda_inv_bygroup$goat_milk_max))
usda_inv_bygroup$sheep_total_max <- as.numeric(gsub(",","",usda_inv_bygroup$sheep_total_max))

usda_inv2009_2012 <- usda_inv_bygroup %>%
  filter((Year>=2009 & Year<=2012)) %>%
  group_by(FIPS) %>%
  mutate(cattle_milk_fill = replace_na(cattle_milk_max, mean(cattle_milk_max, na.rm= TRUE))) %>%
  mutate(cattle_beef_fill = replace_na(cattle_beef_max, mean(cattle_beef_max, na.rm= TRUE))) %>%
  mutate(cattle_total_fill = replace_na(cattle_total_max, mean(cattle_total_max, na.rm= TRUE))) %>%
  mutate(ck_broilers_fill = replace_na(ck_broilers_max, mean(ck_broilers_max, na.rm= TRUE))) %>%
  mutate(ck_layers_fill = replace_na(ck_layers_max, mean(ck_layers_max, na.rm= TRUE))) %>%
  mutate(goat_meat_fill = replace_na(goat_meat_max, mean(goat_meat_max, na.rm= TRUE))) %>%
  mutate(goat_milk_fill = replace_na(goat_milk_max, mean(goat_milk_max, na.rm= TRUE))) %>%
  mutate(sheep_total_fill = replace_na(sheep_total_max, mean(sheep_total_max, na.rm= TRUE))) 

usda_inv2013_2021 <- usda_inv_bygroup %>%
  filter(Year>=2013) %>%
  group_by(FIPS) %>%
  mutate(cattle_milk_fill = replace_na(cattle_milk_max, mean(cattle_milk_max, na.rm= TRUE))) %>%
  mutate(cattle_beef_fill = replace_na(cattle_beef_max, mean(cattle_beef_max, na.rm= TRUE))) %>%
  mutate(cattle_total_fill = replace_na(cattle_total_max, mean(cattle_total_max, na.rm= TRUE))) %>%
  mutate(ck_broilers_fill = replace_na(ck_broilers_max, mean(ck_broilers_max, na.rm= TRUE))) %>%
  mutate(ck_layers_fill = replace_na(ck_layers_max, mean(ck_layers_max, na.rm= TRUE))) %>%
  mutate(goat_meat_fill = replace_na(goat_meat_max, mean(goat_meat_max, na.rm= TRUE))) %>%
  mutate(goat_milk_fill = replace_na(goat_milk_max, mean(goat_milk_max, na.rm= TRUE))) %>%
  mutate(sheep_total_fill = replace_na(sheep_total_max, mean(sheep_total_max, na.rm= TRUE))) 

##Merge dataframes
usda_inv_data <- list(usda_inv2013_2021, usda_inv2009_2012)
usda_inv_bygroup <- rbindlist(usda_inv_data, use.names = TRUE, fill =TRUE)
usda_inv_bygroup <- usda_inv_bygroup[c("FIPS", "Year", "State", "county", "cattle_milk_fill", "cattle_beef_fill", "cattle_total_fill", "ck_broilers_fill", "ck_layers_fill", "goat_meat_fill", "goat_milk_fill", "sheep_total_fill")]


write.csv(usda_inv_bygroup, "data\\animal_density_11-3-2023.csv", row.names = T)

## Determine rate per square mile
geo_county <- read.delim("data/2022_Gaz_counties_national.txt")
geo_county <- filter(geo_county, USPS =="AZ" | USPS =="CO" | USPS =="NM" | USPS =="UT")
geo_county <- geo_county[c("USPS","NAME","ALAND_SQMI","AWATER_SQMI")]
geo_county$state <- ifelse(geo_county$USPS == "AZ", "ARIZONA", NA)
geo_county$state <- ifelse(geo_county$USPS == "CO", "COLORADO", geo_county$state)
geo_county$state <- ifelse(geo_county$USPS == "UT", "UTAH", geo_county$state)
geo_county$state <- ifelse(geo_county$USPS == "NM", "NEW MEXICO", geo_county$state)

geo_county <- geo_county %>% mutate(county = sub("\\s+[^ ]+$", "", NAME))
geo_county <- geo_county %>%  mutate(county = tolower(county))

geo_county$landarea <- geo_county$ALAND_SQMI

geo_county <- geo_county %>% select(state, county, landarea)
write.csv(geo_county, "data\\geo_county.csv", row.names = T)