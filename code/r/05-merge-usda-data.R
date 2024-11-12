#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(data.table)
library(stringr)

##Functions
usdavars <- c("State", "County", "Commodity", "Value")

## Load data
cattle <- read_csv("data/USDAAnimalData/Cattle_OperationsWithSales.csv")
cattle <- cattle[usdavars]
names(cattle)[names(cattle) == 'Value'] <- 'cattle'

chicken <- read_csv("data/USDAAnimalData/BroilerCK_OperationsWithSales.csv")
chicken <- chicken[usdavars]
names(chicken)[names(chicken) == 'Value'] <- 'chicken'

goatsheepprod <- read_csv("data/USDAAnimalData/GoatSheepProducts_OperationsWithSales.csv")
goatsheepprod <- goatsheepprod[usdavars]
names(goatsheepprod)[names(goatsheepprod) == 'Value'] <- 'goatsheepprod'

sheep <- read_csv("data/USDAAnimalData/Sheep_OperationsWithSales.csv")
sheep <- sheep[usdavars]
names(sheep)[names(sheep) == 'Value'] <- 'sheep'

greens <- read_csv("data/USDAAnimalData/Greens_OperationsWithAreaHarvested.csv")
greens_group <- greens %>%
  group_by(State, County) %>%
  summarise(total=sum(Value))
names(greens_group)[names(greens_group) == 'total'] <- 'greens'
greens_group$Commodity <- 'GREENS'

az_county <- read_xlsx("data/01_az_county-info.xlsx")
nm_county <- read_xlsx("data/01_nm_county-info.xlsx")
co_county <- read_xlsx("data/01_co_county-info.xlsx")
ut_county <- read_xlsx("data/01_ut_county-info.xlsx")

counties <- list(az_county, nm_county, co_county, ut_county)
county_info  <- rbindlist(counties, use.names = TRUE, fill=TRUE)
county_info$id <- 1:nrow(county_info)
county_info = subset(county_info, select = c("county", "FIPS", "id"))


##Merge dataframes
usdadfs <- list(cattle, chicken, goatsheepprod, sheep, greens_group)
usdadf <- rbindlist(usdadfs, use.names = TRUE, fill =TRUE)

usdadf = usdadf %>%
  mutate(County = tolower(County))
county_info = county_info %>%
  mutate(county = tolower(county))

usdadf_clean <- usdadf %>% 
  full_join(county_info, by=c('County' = 'county'), keep = TRUE, all.x = TRUE)

## Clean data in Excel as needed
write.csv(usdadf_clean, "data\\usdadf_clean.csv", row.names = T)

## Load cleaned dataset
usdadf_clean_new <- read_csv("data/usdadf_clean.csv")

usdadf_cleanbygroup <-usdadf_clean_new %>%
  select(State, FIPS, cattle, chicken, goatsheepprod, sheep, greens) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  group_by(FIPS) %>%
  summarize(sum_cattle=sum(cattle),
            sum_ck = sum(chicken),
            sum_goatshp = sum(goatsheepprod),
            sum_shp = sum(sheep),
            sum_grns = sum(greens))

## Export
write.csv(usdadf_cleanbygroup, "data\\usdadf_cleanbygroup.csv", row.names = T)
