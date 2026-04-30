wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyverse) # For data manipulation
library(sf) # Needed for the RDS files since they are spatial


#### Script Overview ####
# This script prepares the species data for status calculation purposes. It filters the BirdLife and IUCN data to keep only relevant species and their ranges. The filtered data is then saved for further processing in subsequent scripts.
# Note: We -have- to go through the status rasterization process first because resilience is reliant on what species are in the study area for status.


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "biodiversity")
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path_prev <- file.path(int_data_file_path, "species_data_with_initial_processing") # output_path from previous step  of initial processing
output_path <- file.path(int_data_file_path, "species_dfs_status") # output_path for status files (which begin to be made in this script)


#### Data Layers ####
# Read in the data from the previous step
bird_spp_prepped <- readRDS(file.path(output_path_prev, "bird_spp_prepped.rds"))
iucn_shps_prepped <- readRDS(file.path(output_path_prev, "iucn_shps_prepped.rds"))

# If going to geopackage route:
# bird_spp_prepped <- st_read(file.path(output_path_prev, "bird_spp_prepped.gpkg"))
# iucn_gpkg_files <- list.files(output_path_prev, pattern = "^iucn_shps_prepped_\\d+\\.gpkg$", full.names = TRUE)
# 
# # Read them in as a list
# iucn_shps_prepped <- lapply(iucn_gpkg_files, st_read)

#### Data Processing ####
# Filter IUCN and BirdLife data for status calculation purposes
# Filter BirdLife data
bird_spp_filtered_status <- bird_spp_prepped %>%
  filter((is.na(habitat) | habitat != "Marine") & # If any kept species are still NA by the end of checking if it's in our study area or not (the next script), look into it (ie. check for AVONET synonyms)
           (origin %in% c(1, 2)) & # We want to keep only Native and Reintroduced species
           (category != "DD") & # We want only non- Data Deficient species
           (presence %in% c(1, 4, 5))) # Keep Extant, Possibly Extinct, and Extinct species ranges

# Filter IUCN data
# We want to keep these combinations of habitat types:
# marine	terrestrial	freshwater
# F	        T	        T
# F	        F	        T
# T	        F	        T
# F	        T	        F

# Check for NAs in columns of interest
na_counts_mar_terr_fresh <- lapply(iucn_shps_prepped, function(shp) {
  mar_terr_fresh_cols <- shp[, c("marine", "terrestria", "freshwater")]
  na_counts <- sapply(mar_terr_fresh_cols, function(x) sum(is.na(x)))
  return(na_counts)
}) # If not all 0, consider how to handle. It'll keep geometry too since it's spatial but just ignore that count.

# Proceed with IUCN filtering if good to go
iucn_shps_filtered_status <- iucn_shps_prepped %>%
  map(~ filter(.x, 
               ( (marine == "false" & terrestria == "true" & freshwater == "true") |
                   (marine == "false" & terrestria == "false" & freshwater == "true") | 
                   (marine == "true" & terrestria == "false" & freshwater == "true") |
                   (marine == "false" & terrestria == "true" & freshwater == "false") ) & # Keep combos of interest
                 (origin %in% c(1, 2)) & # We want to keep only Native and Reintroduced species 
                 (category != "DD") & # We want only non- Data Deficient species
                 (presence %in% c(1, 4, 5)) # Keep Extant, Possibly Extinct, and Extinct species ranges
  )
  )

# Save these as R data and read them back in so you don't need to repeat these steps as much 
saveRDS(bird_spp_filtered_status, file = file.path(output_path, "bird_spp_filtered_status.rds"))
saveRDS(iucn_shps_filtered_status, file = file.path(output_path, "iucn_shps_filtered_status.rds"))

# Could also save again as geopackages if desired