##################################################################
# Impact of weather on marathon performance across age and gender
# 1-Air pollutant concentration extraction
##################################################################

# Define data path
data_path = "/Users/yanweitong/Documents/PHP2550-Data/Project1"

# Load packages
library(RAQSAPI)
library(tidyverse)

# set credentials
# TO RUN YOURSELF YOU WILL NEED TO SET YOUR OWN CREDENTIALS 
# RAQSAPI::aqs_sign_up("yanwei_tong@brown.edu")
RAQSAPI::aqs_credentials("yanwei_tong@brown.edu", "copperfrog48")

# cbsa is core based statistical areas
cbsa_df <- data.frame(
  marathon = c("Boston", "NYC", "Chicago", "Twin Cities", "Grandmas"),
  cbsa = c("14460", "35620", "16980", "33460", "20260")
)


# read in marathon dates and merge
marathon_dates <- read.csv(paste0(data_path, "/marathon_dates.csv"))
marathon_dates$date <- as.Date(marathon_dates$date)
full_df <- left_join(marathon_dates, cbsa_df, by = "marathon")

# parameters for collection
# 88101: PM2.5 - Local Conditions; 88502: Acceptable PM2.5 AQI & Speciation Mass;
# 44201: Ozone (O3); 42602: Nitrogen dioxide (NO2); 42401: Sulfar Dioxide (SO2)
aqi_parameters <- c(88101, 88502, 44201, 42602, 42401)

# empty results data frame
results_df <- data.frame(
  cbsa_code = character(0), 
  state_code = character(0), 
  county_code = character(0), 
  site_number = character(0), 
  date_local = character(0), 
  parameter_code = character(0),
  parameter = character(0),
  units_of_measure = character(0), 
  sample_duration = character(0), 
  aqi = integer(0),
  arithmetic_mean = numeric(0))

# loop through dates/marathons
for(i in 1:nrow(full_df)){
  next_df <- aqs_dailysummary_by_cbsa(parameter = aqi_parameters,
                           bdate = full_df$date[i],
                           edate = full_df$date[i],
                           cbsa_code = full_df$cbsa[i]) %>%
    dplyr::select(c("cbsa_code", "state_code", "county_code", 
                    "site_number", "date_local", "parameter_code", 
                    "parameter", "units_of_measure", "sample_duration", 
                    "aqi", "arithmetic_mean"))
  results_df <- rbind(results_df, next_df)
}

results_df <- left_join(results_df, cbsa_df, by = c("cbsa_code" = "cbsa"))
write.csv(results_df, paste0(data_path, "/aqi_values.csv"), row.names = FALSE)



# Extract API data by bounding box
bbox_df <- data.frame(
  marathon = c("Boston", "NYC", "Chicago", "Twin Cities", "Grandmas"),
  minlat = c(42.2391, 40.6028, 41.8530, 44.9072, 46.7896),
  maxlat = c(42.3626, 40.8076, 41.9513, 44.9761, 47.0148),
  minlon = c(-71.5202, -74.0639, -87.6739, -93.3072, -92.0940),
  maxlon = c(-71.0793, -73.9609, -87.6208, -93.0906, -91.7156),
  cbsa = c("14460", "35620", "16980", "33460", "20260")
)

bbox_full_df <- left_join(marathon_dates, bbox_df, by = "marathon")

# Initiate an empty dataset
bbox_results_df <- NA

# loop through dates/marathons
for(i in 1:nrow(full_df)){
  next_df <- aqs_dailysummary_by_box(parameter = aqi_parameters,
                                      bdate = bbox_full_df$date[i],
                                      edate = bbox_full_df$date[i],
                                      minlat = bbox_full_df$minlat[i],
                                      maxlat = bbox_full_df$maxlat[i],
                                      minlon = bbox_full_df$minlon[i],
                                      maxlon = bbox_full_df$maxlon[i])  
  View(next_df)
  bbox_results_df <- rbind(bbox_results_df, next_df)
}

bbox_results_df <- left_join(bbox_results_df, bbox_df, by = c("cbsa_code" = "cbsa"))[-1,]
bbox_results_df_save <-  bbox_results_df %>% dplyr::select(marathon, date_local, state_code, state, cbsa_code, cbsa, 
                                                           county_code, county, parameter_code, parameter, 
                                                           sample_duration, arithmetic_mean, aqi, 
                                                           minlat, maxlat, minlon, maxlon)
write.csv(bbox_results_df_save, paste0(data_path, "/aqi_values_latlon_box.csv"), row.names = FALSE)

# Note: data by box has a high missingness rate for lack of monitor
