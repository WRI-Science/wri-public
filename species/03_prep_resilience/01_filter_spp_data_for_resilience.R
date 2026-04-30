library(tidyverse) # For data manipulation
library(sf) # Need for the RDS files since they are spatial


#### Script Overview ####
# This script prepares the species data for resilience calculation purposes. It filters the BirdLife and IUCN data to keep only relevant species and their ranges. The filtered data is then saved for further processing in subsequent scripts.


#### Base Directories ####
data_file_path <- "/home/shares/wwri-wildfire/data/biodiversity"
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path_prev <- file.path(int_data_file_path, "species_data_with_initial_processing") # output_path from previous step of initial processing
output_path_status <- file.path(int_data_file_path, "species_dfs_status") # output_path for status files
output_path <- file.path(int_data_file_path, "species_dfs_resilience") # output_path for resilience files (which begin to be made in this script)


#### Data Layers ####
# Read in the data from the previous step
bird_spp_prepped <- readRDS(file.path(output_path_prev, "bird_spp_prepped.rds"))
iucn_shps_prepped <- readRDS(file.path(output_path_prev, "iucn_shps_prepped.rds"))
species_list_basic_info <- read_csv(file.path(output_path_status, "all_species_basic_info_study_area_cat_column.csv")) %>% 
  filter(is.na(study_area_category)) # Remove any extinct in study area species for resilience; they will get added back in at the end and get 0s for resilience. If you leave them in, then, if they aren't fully extinct in North America, they will get captured since we are now working with the North America study area for resilience. We do not want that.


#### Data Processing ####
# Filter IUCN and BirdLife data for resilience calculation purposes
# We want to same species IDs as used in status, but:
# - global ranges for all of NORTH AMERICA
# - ALL origins
# - presence = 1, 4 (5 is extinct so remove unlike in status)
# - filter out DD, -EX-, and -EW-
bird_spp_filtered_rr <- bird_spp_prepped %>%
  filter((is.na(habitat) | habitat != "Marine") &
           # (origin %in% c(1, 2)) & # We want all origins
           (!(category %in% c("DD", "EW", "EX"))) & # Add in EW and EX vs. status
           (presence %in% c(1, 4)) & # Don't want 5 anymore -- extinct
           (id_no %in% c(species_list_basic_info$iucn_sid)) # Keep to status IDs
  )

# Get filtered ranges for non-birds
iucn_shps_filtered_rr <- iucn_shps_prepped %>%
  map(~ filter(.x,
               ( (marine == "false" & terrestria == "true" & freshwater == "true") |
                   (marine == "false" & terrestria == "false" & freshwater == "true") |
                   (marine == "true" & terrestria == "false" & freshwater == "true") |
                   (marine == "false" & terrestria == "true" & freshwater == "false") ) & # Keep combos of interest
                 # (origin %in% c(1, 2) ) & # We want all origins
                 (!(category %in% c("DD", "EW", "EX"))) & # Add in EW and EX vs. status
                 (presence %in% c(1, 4)) & # Don't want 5 anymore -- extinct
                 (id_no %in% c(species_list_basic_info$iucn_sid)) # Keep to status IDs
  )
  ) %>%
  bind_rows() # Make one df since we have way less rows than what we worked through in status

# Save these as R data and read them back in in next script
saveRDS(bird_spp_filtered_rr, file = file.path(output_path, "bird_spp_filtered_rr.rds"))
saveRDS(iucn_shps_filtered_rr, file = file.path(output_path, "iucn_shps_filtered_rr.rds"))

# Can also save as geopackages if desired