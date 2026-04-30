wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra) # For rasterization
library(sf) # For working with spatial (vector/shapefile) data
library(tidyverse) # For data manipulation
library(rredlist) # For accessing the IUCN Red List API
api_key <- Sys.getenv('IUCN_KEY') # Get your IUCN API key from your .renv file


#### Script Overview ####
# This script takes the species range data that was rasterized in the previous step and retrieves and adds the IUCN Red List assessment information for each species. It assigns weights to species based on their IUCN Red List category and writes out the cleaned data with the IUCN Red List information included.


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "biodiversity")
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path <- file.path(int_data_file_path, "species_dfs_status")


#### Data Layers ####
# Read in the species range rasterization that got written to csvs with cell ids and presence for each species
all_spp_csv_filepaths <- list.files(path = output_path, pattern = "_raster.csv$", recursive = TRUE, full.names = TRUE)
all_spp_csvs <- lapply(all_spp_csv_filepaths, read.csv)


#### Functions ####
source(here("biodiversity", "00_biodiversity_custom_functions.R")) # For the get_spp_info function to get IUCN Red List info


#### Data Processing ####
# Get a list of all of the ids rasterized
all_spp_csv_ids <- as.integer(gsub("\\_raster.csv$", "", basename(all_spp_csv_filepaths)))

# Get the species info from the IUCN Red List API for each species id
iucn_red_list_from_api <- map_dfr(all_spp_csv_ids, get_spp_info, key = api_key)

# Write out IUCN Red List assessment info for species in our study area
write_csv(iucn_red_list_from_api, file.path(output_path, "iucn_red_list_from_api.csv"))

# Clean cell id column and add species ID to the cell presences 
# This step will also show if a species did not get rasterized due to too small of a range; will reveal the indices to remove/fix. Could also manually check if any csvs are empty too just in case.
clean_all_spp_csvs <- all_spp_csvs # Copy the list of dataframes to clean since we are editing in place

# Iterate over each species dataframe in the list
for (i in seq_along(clean_all_spp_csvs)) {
  # Rename cell column to cell_id to match the template raster
  clean_all_spp_csvs[[i]] <- rename(clean_all_spp_csvs[[i]], cell_id = cell)
  
  # Take corresponding species ID
  # Since we gathered the IDs in the same order as the csvs, we can use the index to get the species ID
  species_id <- all_spp_csv_ids[[i]]
  
  # Apply it to a new column in the df
  clean_all_spp_csvs[[i]]$iucn_sid <- species_id
  
  # Print message indicating what ID we are at
  message("Done with species ", i, " of ", length(clean_all_spp_csvs), ", ID ", all_spp_csv_ids[[i]])
}

rm(all_spp_csvs) # Free up memory

# Select relevant columns from the IUCN Red List data
red_list_categories <- iucn_red_list_from_api %>%
  select(iucn_sid, sciname, category = red_list_category_code)

# Add in threat statuses from API red list & convert them to numerical values
all_spp_with_categories <- clean_all_spp_csvs # Copy the cleaned list of dataframes to add categories to

# Iterate over each species dataframe in the list and join with the IUCN Red List categories
for (i in seq_along(all_spp_with_categories)) {
  # Join the IUCN Red List category info to the species dataframe
  all_spp_with_categories[[i]] <- left_join(all_spp_with_categories[[i]], red_list_categories, by = "iucn_sid")
  
  # Set weights based on the IUCN Red List category
  all_spp_with_categories[[i]] <- all_spp_with_categories[[i]] %>%
    mutate(weight = case_when(
      category == "EX" ~ 0.0,
      category == "EW" ~ 0.0, # For our purposes, this is in essence the same as extinct
      category == "CR" ~ 0.2,
      category == "EN" ~ 0.4,
      category == "VU" ~ 0.6,
      category == "NT" ~ 0.8,
      category == "LR/nt" ~ 0.8, # Equivalent to NT but outdated so may not be present
      category == "LR/cd" ~ 0.8, # Equivalent to NT but outdated so may not be present
      category == "LC" ~ 1.0,
      category == "LR/lc" ~ 1.0, # Equivalent to LC but outdated so may not be present
      TRUE ~ NA # Should not have any cases of this
    )) %>%
    mutate(weight = ifelse(presence == 5, 0.0, weight)) # If it's extinct in the range (presence = 5), then weigh it like it's extinct overall
}

# Save the cleaned species data with categories and weights for use in later script
saveRDS(all_spp_with_categories, file.path(output_path, "all_spp_with_categories.rds"))

rm(clean_all_spp_csvs) # Free up memory

# Create a dataframe with species scientific names and IUCN Red List categories for later use
species_list_basic_info <- iucn_red_list_from_api %>%
  select(iucn_sid, sciname, class = class_name, category = red_list_category_code, kingdom = kingdom_name)

# # Write out the basic species info to a CSV for later use
# write_csv(species_list_basic_info, file.path(output_path, "all_species_basic_info.csv"))

# Get the species that are extinct overall and species extinct in the study area
# Initialize an empty list
extinct_species <- list()

# Iterate over each species dataframe in the list to find extinct species (based on being EX/EW or having all presence = 5 -- weight = 0)
for (df in all_spp_with_categories) {
  if (all(df$weight == 0)) { # Check if all weights in the df are 0 (all extinct)
    id <- unique(df$iucn_sid) # If so, get the species id
    extinct_species <- c(extinct_species, id) # Append to list
  }
}

# Filter the basic information df to only extinct species and add a study area category column
extinct_species_info <- species_list_basic_info %>%
  filter(iucn_sid %in% extinct_species) %>% # Filter to only extinct species
  mutate(study_area_category = "EX (study area)") # Add a column indicating they are extinct in the study area

# Rejoin to rest of basic information df
species_list_basic_info_ex <- species_list_basic_info %>%
  left_join(extinct_species_info, by = c("iucn_sid", "sciname", "class", "category", "kingdom"))

# Write out the species list with basic info and study area category column for use later
write_csv(species_list_basic_info_ex, file.path(output_path, "all_species_basic_info_study_area_cat_column.csv"))