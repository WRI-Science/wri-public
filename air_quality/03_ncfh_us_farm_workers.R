# script for cleaning and calculating NCFH total farm workers and H-2A Temp farm workers
# To fill in for US NAICS codes 111 and 112 lack of data on agricultural workers 

# Data obtained from the National Center for Farmworker Health shiny dashboard 
# https://ncfh.shinyapps.io/farmlabor_dashboard/?code=A5jnDDH07xC7u50CB3PQ7qAMSHMbbAI5r54lyjL0uf9bI&state=TBojGXtvf9
# download date - June 3, 2024
# Data on Total Farm Workers (Crop and Animal and Aquaculture) is from 2017 
# Data on H-2A Temporary Workers is from 2023

library(stringr)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(ggplot2)
library(viridis)
library(scales)
library(raster)
library(tidyr)
library(tibble)

#### Base directories ####
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/air_quality"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/air_quality"

#### Boundary layers ####
study_area_admin2_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_2.shp")) %>% 
  st_transform(5070)

#### Functions ####
# Function to read each NCFH state csv 
read_farm_worker_data <- function(file_path) {
  read.csv(file_path)
}

#### Data Layers ####
total_farm_workers_path <- file.path(raw_data_file_path, "vulnerable_occupations/ncfh_total_farm_workers_2017")
total_h2a_farm_workers_path <- file.path(raw_data_file_path, "vulnerable_occupations/ncfh_total_h2a_workers_2023")

# Get the list of CSV files in the total farm workers directory 
total_farm_workers_files <- list.files(total_farm_workers_path, pattern = "\\.csv$", full.names = TRUE)
# Get the list of CSV files in the total farm workers directory
h2a_farm_workers_files <- list.files(total_h2a_farm_workers_path, pattern = "\\.csv$", full.names = TRUE)

#### Bind all individual total farm worker state files together ####

# Read and bind all the total farm worker CSV files together
total_farm_workers_data <- lapply(total_farm_workers_files, read_farm_worker_data) %>%
  bind_rows() %>% 
  rename(state = State, county = County)

# Read and bind all the CSV files together
h2a_farm_workers_data <- lapply(h2a_farm_workers_files, read_farm_worker_data) %>%
  bind_rows() 

# Merge the datasets on common columns (state_name and county)
# Rename 'workers in county' and 'workers in state' to include h2a
# Combine merging and renaming in one tidyverse chain
merged_farm_workers_data <- 
  merge(total_farm_workers_data, h2a_farm_workers_data, all = TRUE) %>%
  rename(
    h2a_workers_in_county = workers_in_county,
    h2a_workers_in_state = workers_in_state
  ) %>%
  dplyr::select(-c(X))

# rename some Alaskan counties with data to merge them with the alaskan wwri study area county names 
# Create a named vector to map old county names to new county names
# Define the county mapping

alaska_county_mapping <- c(
  "Aleutians West Census Area" = "Aleutians West",
  "Juneau City and Borough" = "Juneau",
  "Kenai Peninsula Borough" = "Kenai Peninsula",
  "Southeast Fairbanks Census Area" = "Fairbanks North Star")

# Update county names for the specified Alaska counties
merged_farm_workers_data <- merged_farm_workers_data %>%
  mutate(county = if_else(county %in% names(alaska_county_mapping), 
                          alaska_county_mapping[match(county, names(alaska_county_mapping))], 
                          county))

# NCFH did not have Alaska H-2A worker data 
# Infill the value 57 for H-2A workers count in the specified Alaska counties pulled from state data 
merged_farm_workers_data <- merged_farm_workers_data %>%
  mutate(h2a_workers_in_county = if_else(state == "Alaska" & county %in% alaska_county_mapping, 57, h2a_workers_in_county))

# Add Total Farmworkers and H-2A workers together 
merged_farm_workers_data <- merged_farm_workers_data %>% 
  mutate(sum_total_workers_and_h2a = Total.Workers + h2a_workers_in_county)


#### Attach geometries to study area ####

# Read in the wwri_study_area_admin2.shp and filter for only US
us_counties <- study_area_admin2_shape_5070 %>%
  filter(country %in% "United States") 

# Join total farm worker data to study area shapefile
us_total_farm_workers <- us_counties %>%
  left_join(merged_farm_workers_data, by = c("state_name" = "state", "county" = "county"))

#### Write out us_total_farm_workers shapefile  ####

# Write the merged data to the specified file path as a GeoJSON
st_write(
  us_total_farm_workers,
  file.path(intermediate_data_file_path, "ncfh/us_total_farm_h2a_workers.geojson"),
  driver = "GeoJSON", 
  append = FALSE,
  delete_dsn = TRUE
)

#### Visualize ####

# Plot with grey color for zero and NA values
ggplot(data = us_total_farm_workers) +
  geom_sf(aes(fill = sum_total_workers_and_h2a), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Total Farm + H-2A Workers",
       fill = "Count") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

