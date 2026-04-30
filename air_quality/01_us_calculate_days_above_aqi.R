wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# This script reads spatial boundary layers and raw daily AQI data for 2024,
# filters to western U.S. monitoring sites, joins each AQI observation to its
# site metadata (including coordinates), and then, for each site, counts the
# number of days where the Air Quality Index exceeds specified thresholds
# (100 and 300). The results are written out as CSV summary tables with a
# row per site and columns for the count of exceedance days.

# Load required libraries
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
library(rlang)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "air_quality")
raw_data_file_path <- file.path(wri_project_root, "data", "air_quality", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "air_quality", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "air_quality")

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) %>% 
  st_transform(5070)
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

process_aqi_data_left <- function(aqi_data, aqi_threshold) {
  # Create the new column name based on the threshold
  days_col <- paste0("days_above_", aqi_threshold)
  
  aqi_days_count <- aqi_data %>%
    dplyr::filter(aqi > aqi_threshold) %>%
    dplyr::group_by(defining_site) %>%
    dplyr::summarise(days = n(), .groups = "drop") # temp name 'days'
  
  site_info <- aqi_data %>%
    dplyr::group_by(defining_site) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  out <- left_join(site_info, aqi_days_count, by = "defining_site") %>%
    dplyr::mutate(days = tidyr::replace_na(days, 0)) %>%
    dplyr::select(
      -date,
      -aqi,
      -category,
      -number_of_sites_reporting,
      -defining_parameter
    )
  
  # Rename the 'days' column to your dynamic column name
  out <- out %>% dplyr::rename(!!days_col := days)
  
  return(out)
} # adds 0 to sites with 0 days above AQI x

west_states <- c("Alaska", "California", "Nevada", "Wyoming", "Oregon", "Washington", "Idaho", "Utah", "New Mexico", "Arizona", "Colorado", "Montana",
                 "North Dakota", "South Dakota", "Nebraska", "Kansas", "Oklahoma", "Texas")

#### Data Layers ####
us_daily_aqi_by_county_2024 <- read_csv(file.path(raw_data_file_path, "daily_aqi_by_county_2024.csv")) %>%
  dplyr::rename_with(~gsub(" ", "_", tolower(.))) %>%
  dplyr::filter(state_name %in% west_states)

aqs_sites <- read_csv(file.path(raw_data_file_path, "aqs_sites.csv")) %>%  
  dplyr::rename_with(~gsub(" ", "_", tolower(.))) %>%
  dplyr::filter(state_name %in% west_states)

#### Attach daily aqi with aqs sites ####

# Create an aqi defining_site column in the aqs_site df in order to attach to the us_daily_aqi_by_county_2024 df to get lat/long coords of sensors
aqs_sites$defining_site <- paste(aqs_sites$state_code, aqs_sites$county_code, aqs_sites$site_number, sep = "-")

# Join the daily_aqi_by_county_2024 data with the aqs_sites data to get the lat/long coordinates
us_daily_aqi_by_county_2024_coords <- left_join(us_daily_aqi_by_county_2024, aqs_sites) %>%
  dplyr::select(-met_site_state_code, -met_site_county_code, -met_site_site_number, -met_site_type, -met_site_distance, -met_site_direction)

#### Use function to calculate days_above_100 and days_above_300 at each defining site and 0 at sites with no days above 100 ####
us_days_above_100_aqi_2024 <- process_aqi_data_left(us_daily_aqi_by_county_2024_coords, 100)
us_days_above_300_aqi_2024 <- process_aqi_data_left(us_daily_aqi_by_county_2024_coords, 300)

write.csv(
  us_days_above_100_aqi_2024,
  file = file.path(intermediate_data_file_path, "us_days_above_100_2024.csv"),
  row.names = FALSE)

write.csv(
  us_days_above_300_aqi_2024,
  file = file.path(intermediate_data_file_path, "us_days_above_300_2024.csv"),
  row.names = FALSE)


