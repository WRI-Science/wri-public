#### Goal ####
# script to clean and calculate the daily BC and YT AQI from the AirNow individual pollutant 2024 .csv files
# first part of the script converts the hourly AQI values for SO2 and NO2 to daily max values
# second part of the script cleans each csv file and combines all the pollutant df's together 
# this is in order to identify which pollutant is the maximum at each site across dates per EPA's AQI calculation 
# third code identifies the maximum AQI at each individual site for each date
# fourth code counts the days above 100 at each site 

# In total this script takes about 2 minutes to run

library(sf) 
library(tidyverse) 
library(here)
library(ggplot2) 
library(dplyr)
library(readr)
library(purrr)
library(terra)
library(here) 
library(tidyr)

#### Base directories ####
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/air_quality"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/air_quality"

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) %>% 
  st_transform(5070)
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Data Layers ####
# Define the folder containing the CSV files with the most recent 2024 data
canada_air_pollutants_20_24 <- file.path(raw_data_file_path, "aqi/2024/AQIforallparameters_AlbertaBritishColumbiaYukon_2020_2024")

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

read_and_filter_pollutants_2024 <- function(folder_path, pollutants = c("SO2", "O3", "PM2.5", "PM10", "CO", "NO2")) {
  data_2024 <- list()
  for (pollutant in pollutants) {
    # Construct file path (assuming filenames are pollutant.csv, e.g., "SO2.csv")
    file_path <- file.path(folder_path, paste0(pollutant, ".csv"))
    if (!file.exists(file_path)) {
      warning(paste("File not found:", file_path))
      next
    }
    df <- read.csv(file_path)
    # Ensure Date column exists and is in POSIXct format
    if (!"Date" %in% colnames(df)) {
      warning(paste("No 'Date' column in:", file_path))
      next
    }
    df$Date <- as.POSIXct(df$Date, tz = "UTC")
    df_2024 <- subset(df, format(Date, "%Y") == "2024")
    # Assign to environment and list
    assign(paste0(pollutant, "_2024"), df_2024, envir = .GlobalEnv)
    data_2024[[paste0(pollutant, "_2024")]] <- df_2024
  }
  invisible(data_2024)
}

#### Canada: Read in all the csv files in the folder for each pollutant and filter for only year 2024 #### 
read_and_filter_pollutants_2024(canada_air_pollutants_20_24)

# Rename FullAQSCode to SiteID in the CO and O3 data frames
CO_2024 <- CO_2024 %>%
  rename(SiteID = FullAQSCode)
O3_2024 <- O3_2024 %>%
  rename(SiteID = FullAQSCode)

#### Calculate daily AQI from hourly AQI for NO2 and SO2 pollutants #### 

# Calculate daily AQI from hourly AQI for NO2
# other air pollutant date columns only have date while this includes date and hour
NO2_2024 <- NO2_2024 %>%
  # rename date column to Date_Hour
  rename(Date_Hour = Date) %>%
  # Make a date column that just has the calendar date
  mutate(Date = as.Date(Date_Hour)) %>%
  group_by(SiteID, Date) %>%
  mutate(
    Daily.AQI = max(Hourly.AQI, na.rm = TRUE),
    Daily.Concentration.ppb = max(Hourly.Concentration.ppb, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(Hourly.AQI == Daily.AQI & Hourly.Concentration.ppb == Daily.Concentration.ppb) %>%
  dplyr::select(-Hourly.AQI, -Hourly.Concentration.ppb, -Date_Hour) %>% 
  # use distinct to make sure there is only one row per site and date (in case multiple hours have the same max AQI)
  distinct(SiteID, Date, .keep_all = TRUE)

# Calculate daily AQI from hourly AQI for SO2
# other air pollutant date columns only have date while this includes date and hour
SO2_2024 <- SO2_2024 %>%
  # rename date column to Date_Hour
  rename(Date_Hour = Date) %>%
  # Make a date column that just has the calendar date
  mutate(Date = as.Date(Date_Hour)) %>%
  group_by(SiteID, Date) %>%
  mutate(
    Daily.AQI = max(Hourly.AQI, na.rm = TRUE),
    Daily.Concentration.ppb = max(Hourly.Concentration.ppb, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(Hourly.AQI == Daily.AQI & Hourly.Concentration.ppb == Daily.Concentration.ppb) %>%
  dplyr::select(-Hourly.AQI, -Hourly.Concentration.ppb, -Date_Hour) %>% 
  # use distinct to make sure there is only one row per site and date (in case multiple hours have the same max AQI)
  distinct(SiteID, Date, .keep_all = TRUE)

#### Identify the Max AQI at each site per day ####

all_keys <- c("SiteName", "Latitude", "Longitude", "Date", "AgencyName", "SiteID")

O3_2024_wide <- O3_2024 %>%
  dplyr::rename(AQI_O3 = Daily.AQI) %>%
  dplyr::mutate(SiteID = as.character(SiteID)) %>%
  dplyr::select(dplyr::all_of(all_keys), AQI_O3)

CO_2024_wide <- CO_2024 %>%
  dplyr::rename(AQI_CO = Daily.AQI) %>%
  dplyr::mutate(SiteID = as.character(SiteID)) %>%
  dplyr::select(dplyr::all_of(all_keys), AQI_CO)

PM10_2024_wide <- PM10_2024 %>%
  dplyr::rename(AQI_PM10 = Daily.AQI) %>%
  dplyr::mutate(SiteID = as.character(SiteID)) %>%
  dplyr::select(dplyr::all_of(all_keys), AQI_PM10)

PM2.5_2024_wide <- PM2.5_2024 %>%
  dplyr::rename(AQI_PM25 = Daily.AQI) %>%
  dplyr::mutate(SiteID = as.character(SiteID)) %>%
  dplyr::select(dplyr::all_of(all_keys), AQI_PM25)

NO2_2024_wide <- NO2_2024 %>%
  dplyr::rename(AQI_NO2 = Daily.AQI) %>%
  dplyr::mutate(SiteID = as.character(SiteID)) %>%
  dplyr::select(dplyr::all_of(all_keys), AQI_NO2)

SO2_2024_wide <- SO2_2024 %>%
  dplyr::rename(AQI_SO2 = Daily.AQI) %>%
  dplyr::mutate(SiteID = as.character(SiteID)) %>%
  dplyr::select(dplyr::all_of(all_keys), AQI_SO2)

# Now, full join using the shared keys
shared_keys <- all_keys

joined_can_pollutants_2024 <- PM10_2024_wide %>%
  dplyr::full_join(PM2.5_2024_wide, by = shared_keys) %>%
  dplyr::full_join(NO2_2024_wide,   by = shared_keys) %>%
  dplyr::full_join(SO2_2024_wide,   by = shared_keys) %>%
  dplyr::full_join(O3_2024_wide,    by = shared_keys) %>%
  dplyr::full_join(CO_2024_wide,    by = shared_keys)

# Clean up -999s (turn -999 to NA)
joined_can_pollutants_2024 <- joined_can_pollutants_2024 %>%
  mutate(across(starts_with("AQI_"), ~ ifelse(. == -999, NA, .)))

# Calculate Max AQI for each day/site combo, but skip if all AQIs are NA
max_aqi_by_site_per_day <- joined_can_pollutants_2024 %>%
  rowwise() %>%
  mutate(
    Max_AQI = if (all(is.na(c_across(starts_with("AQI_"))))) NA_real_
    else max(c_across(starts_with("AQI_")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(Max_AQI)) %>%
  dplyr::select(SiteName, SiteID, AgencyName, Latitude, Longitude, Date, Max_AQI)

#### Filter out AQI days over 100 and over 300 to get the days above at each site ####
# puts a 0 at sites where there are no days above 100
canada_days_above_100_count <- max_aqi_by_site_per_day %>%
  dplyr::group_by(SiteID, SiteName, AgencyName, Latitude, Longitude) %>%
  dplyr::summarise(days_above_100 = sum(Max_AQI > 100, na.rm = TRUE), .groups = "drop")

canada_days_above_300_count <- max_aqi_by_site_per_day %>%
  dplyr::group_by(SiteID, SiteName, AgencyName, Latitude, Longitude) %>%
  dplyr::summarise(days_above_300 = sum(Max_AQI > 300, na.rm = TRUE), .groups = "drop")

write.csv(
  canada_days_above_100_count,
  file = file.path(intermediate_data_file_path, "aqi/canada_days_above_100_2024.csv"),
  row.names = FALSE)

write.csv(
  canada_days_above_300_count,
  file = file.path(intermediate_data_file_path, "aqi/canada_days_above_300_2024.csv"),
  row.names = FALSE)



