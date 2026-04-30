wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra) # For rasterization
library(sf) # For working with spatial (vector/shapefile) data
library(tidyverse) # For data manipulation
library(here) # For sourcing the functions script cleanly later
sf::sf_use_s2(FALSE) # Turn it off entirely because many shapes are invalid with it on. Only BirdLife needs this off not IUCN but I've turned it off for both for consistency.


#### Script Overview ####
# This script takes the filtered species range data for status and rasterizes it to a 1 km resolution. It writes out the species ranges in a csv format (cell_id with presence type), which is used in later steps.


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "biodiversity")
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path <- file.path(int_data_file_path, "species_dfs_status")


#### Data Layers ####
# Read in the filtered status data from the previous step
bird_spp_filtered_status <- readRDS(file.path(output_path, "bird_spp_filtered_status.rds"))
iucn_shps_filtered_status <- readRDS(file.path(output_path, "iucn_shps_filtered_status.rds"))


#### Functions ####
source(here("biodiversity", "00_biodiversity_custom_functions.R")) # For the process_species_for_status_and_resilience function to rasterize species ranges


#### Data Processing ####
# Add the birds df to the IUCN dfs list to create the full list to iterate over
shps_filtered_status_full <- append(iucn_shps_filtered_status, list(bird_spp_filtered_status))

# Set up the template study area raster to rasterize the species ranges onto
study_area_1km <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_with_na_moll.tif")) # We use the 1 km resolution one because if we rasterize directly to 90 m, the data becomes too long for our current processing method. We believe this is okay for our purposes.

# Prepare the raster for effective conversion of species range rasters to csv form
study_area_1km <- study_area_1km %>%
  setNames("cell_id") %>% # Make the name of the raster layer "cell_id"
  #project(., "epsg:5070") %>% # choose a crs: equal area needed for species status. not relevant here since we replace the values but keep in mind this is reading the 0 and 1 as continuous/numeric and not categories.
  setValues(1:ncell(.)) # get numbered cells

# Write this raster out so that it can be read into the parallelization
writeRaster(study_area_1km, file.path(output_path, "study_area_1km_biodiversity_status_mask.tif"), overwrite = TRUE)

# Use the custom function from 00_biodiversity_custom_functions.R to process the species in parallel for status
process_species_for_status_and_resilience(iucn_shps_filtered = shps_filtered_status_full,
  version = "status")