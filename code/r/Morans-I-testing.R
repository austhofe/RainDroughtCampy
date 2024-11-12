install.packages("ape", dependencies = T)
library(ape)

prism.stations <- raw_prism_file_paths
prism.stations$lat <- substr(prism.stations$filepath,87,93)
prism.stations$long <- substr(prism.stations$filepath,95,103)
prism.stations$lat <-as.numeric(prism.stations$lat)
prism.stations$long <-as.numeric(prism.stations$long)
latlongvars <- c("lat", "long")
prism.stations <- prism.stations[latlongvars]


az.moran.test <- filter(final_az_df_analysis_week, year==2009)
az.moran.test <- replace(az.moran.test, is.na(az.moran.test), 0)

prism.dists <- as.matrix(dist(cbind(az.moran.test$long, az.moran.test$lat)))
prism.dists.inv <-1/prism.dists
diag(prism.dists.inv) <- 0

prism.dists.inv[1:5, 1:5]
prism.dists.inv[is.infinite(prism.dists.inv)] <-0
Moran.I(az.moran.test$total__campy_cases, prism.dists.inv, na.rm=T)




az.moran.test <- filter(final_az_df_analysis_week, year==2019)
az.moran.test <- replace(az.moran.test, is.na(az.moran.test), 0)

prism.dists <- as.matrix(dist(cbind(az.moran.test$long, az.moran.test$lat)))
prism.dists.inv <-1/prism.dists
diag(prism.dists.inv) <- 0

prism.dists.inv[1:5, 1:5]
prism.dists.inv[is.infinite(prism.dists.inv)] <-0
Moran.I(az.moran.test$total__sal_cases, prism.dists.inv, na.rm=T)




co.moran.test <- filter(final_co_df_analysis_week, year==2019)
co.moran.test <- replace(co.moran.test, is.na(co.moran.test), 0)

prism.dists <- as.matrix(dist(cbind(co.moran.test$long, co.moran.test$lat)))
prism.dists.inv <-1/prism.dists
diag(prism.dists.inv) <- 0

prism.dists.inv[1:5, 1:5]
prism.dists.inv[is.infinite(prism.dists.inv)] <-0
Moran.I(co.moran.test$total__campy_cases, prism.dists.inv, na.rm=T)


nm.moran.test <- filter(final_nm_df_analysis_week, year==2019)
nm.moran.test <- replace(nm.moran.test, is.na(nm.moran.test), 0)

prism.dists <- as.matrix(dist(cbind(nm.moran.test$long, nm.moran.test$lat)))
prism.dists.inv <-1/prism.dists
diag(prism.dists.inv) <- 0

prism.dists.inv[1:5, 1:5]
prism.dists.inv[is.infinite(prism.dists.inv)] <-0
Moran.I(nm.moran.test$total__campy_cases, prism.dists.inv, na.rm=T)


ut.moran.test <- filter(final_ut_df_analysis_week, year==2019)
ut.moran.test <- replace(ut.moran.test, is.na(ut.moran.test), 0)

prism.dists <- as.matrix(dist(cbind(ut.moran.test$long, ut.moran.test$lat)))
prism.dists.inv <-1/prism.dists
diag(prism.dists.inv) <- 0

prism.dists.inv[1:5, 1:5]
prism.dists.inv[is.infinite(prism.dists.inv)] <-0
Moran.I(ut.moran.test$total__campy_cases, prism.dists.inv, na.rm=T)
