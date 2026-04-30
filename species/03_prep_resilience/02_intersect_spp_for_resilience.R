wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# library(terra) # For spatial data manipulation
library(sf) # For working with spatial (vector/shapefile) data
library(tidyverse) # For data manipulation
library(here) # For sourcing the functions script cleanly later
sf::sf_use_s2(FALSE) # Turn it off entirely because many shapes are invalid with it on. Only BirdLife needs this off not IUCN but I've turned it off for both for consistency.


#### Script Overview ####
# This script intersects the filtered species range data with North America and saves those intersected ranges for further processing.


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "biodiversity")
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path <- file.path(int_data_file_path, "species_dfs_resilience") # output_path for resilience files


#### Data Layers ####
# Read in the data from the previous step
bird_spp_filtered_rr <- readRDS(file.path(output_path, "bird_spp_filtered_rr.rds"))
iucn_shps_filtered_rr <- readRDS(file.path(output_path, "iucn_shps_filtered_rr.rds"))


#### Functions ####
source(here("biodiversity", "00_biodiversity_custom_functions.R")) # For the process_species_for_status_and_resilience function to intersect species ranges


#### Data Processing ####
# Add birds df to the IUCN df to create the full list to iterate over
shps_filtered_rr_full <- append(list(iucn_shps_filtered_rr), list(bird_spp_filtered_rr))

# Use the custom function from 00_biodiversity_custom_functions.R to process the species in parallel for resilience
process_species_for_status_and_resilience(iucn_shps_filtered = shps_filtered_rr_full,
                                          version = "resilience")