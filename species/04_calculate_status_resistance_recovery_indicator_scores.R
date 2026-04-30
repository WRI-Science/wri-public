wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyverse) # For data manipulation
library(here) # To assemble file paths within project
library(terra) # For raster operations


#### Script Overview ####
# This script takes the species level status data with presence cell information and joins it with the resistance and recovery scores by species. Then, raster files for status, resistance, and recovery are created. The status raster is rescaled to a 0-1 scale based on the 75th extinction risk in each cell. Resistance and recovery had rescaling at the species level and no further rescaling is done for those rasters.
# This script also writes out unaligned indicator layers. (TBD)


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "biodiversity")
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path_prev <- file.path(int_data_file_path, "species_dfs_status") # output_path from previous steps of status processing
output_path <- file.path(int_data_file_path, "species_dfs_resilience") # output_path for resilience files


#### Boundary Layers ####
# Read in the template study area raster to rasterize the species ranges onto (read in what was saved earlier)
study_area_1km <- rast(file.path(output_path_prev, "study_area_1km_biodiversity_status_mask.tif"))


#### Data Layers ####
# Read in the species level resistance and recovery scores
spp_resistance_recovery_indicator_scores <- read_csv(file.path(output_path, "spp_resistance_recovery_indicator_scores.csv"))

# Read in the species level status data with presence cell information
all_spp_with_categories <- readRDS(file.path(output_path_prev, "all_spp_with_categories.rds"))

# Read in the species basic information with study area extinct or not column
all_species_basic_info_study_area_cat_column <- read_csv(file.path(output_path_prev, "all_species_basic_info_study_area_cat_column.csv"))


#### Data Processing ####
# Prepare species that have 0 for resilience
# Note: Currently, these species are not included in the rescaling process
spp_to_add <- all_species_basic_info_study_area_cat_column %>%
  filter(!is.na(study_area_category)) %>% # Filter to species extinct in study area
  filter(!(iucn_sid %in% spp_resistance_recovery_indicator_scores$id_no)) %>% # Ensure only working with IDs not already in the scores (this should not reduce the number of species)
  select(iucn_sid) %>% # Select only needed column; joining is done by ID
  mutate(traits_recovery = 0, # Set traits recovery to 0 (for calculating indicator layer)
         traits_resistance = 0, # Set traits resistance to 0 (for calculating indicator layer)
         geom_area_rescaled = 0) # Set geom area rescaled to 0 (for calculating indicator layer)

# Combine all species data, including the species with 0s
spp_resistance_recovery_indicator_scores <- spp_resistance_recovery_indicator_scores %>%
  select(iucn_sid = id_no, traits_recovery, traits_resistance, geom_area_rescaled) %>%
  rbind(spp_to_add)

# Write out the full file for resistance and recovery
write_csv(spp_resistance_recovery_indicator_scores, file.path(output_path, "spp_resistance_recovery_indicator_scores_full.csv"))

# Assign these values to the cells each species is in (attach it to the stuff from status)
all_spp_with_status_resistance_recovery_indicator_scores <- all_spp_with_categories # Make a copy that we will edit

# Iterate over the copied df
for (i in seq_along(all_spp_with_status_resistance_recovery_indicator_scores)) {
  all_spp_with_status_resistance_recovery_indicator_scores[[i]] <- left_join(all_spp_with_status_resistance_recovery_indicator_scores[[i]], spp_resistance_recovery_indicator_scores, by = "iucn_sid") # Join the resistance and recovery scores to the species data
}

# Save the full data with status, resistance, and recovery scores in case of crash or needing to terminate
saveRDS(all_spp_with_status_resistance_recovery_indicator_scores, file.path(output_path, "cells_with_resistance_recovery_and_status_indicators.csv"))

rm(all_spp_with_categories) # Free up memory

# Split the dfs into two lists
total_spp_dfs <- length(all_spp_with_status_resistance_recovery_indicator_scores)
half_point <- ceiling(total_spp_dfs / 2)

# Bind each list individually
# We cannot do this all in one df or it is too long
first_half_spp <- bind_rows(all_spp_with_status_resistance_recovery_indicator_scores[1:half_point])
second_half_spp <- bind_rows(all_spp_with_status_resistance_recovery_indicator_scores[(half_point + 1):total_spp_dfs])

rm(all_spp_with_status_resistance_recovery_indicator_scores) # Free up memory

# Sum the status, resistance, and recovery scores by cell_id and count the number of species in each cell (first half)
first_half_status_resistance_recovery_indicator_scores_sum_spp_counts <- first_half_spp %>%
  group_by(cell_id) %>% # All calculations are done by cell ID
  summarize(
    total_status_score = sum(weight), # Sum the status scores
    total_resistance_traits_score = sum(traits_resistance), # Sum the resistance traits scores
    total_recovery_traits_score = sum(traits_recovery), # Sum the recovery traits scores
    total_range_area_score = sum(geom_area_rescaled), # Sum the rescaled range area scores
    species_count = n_distinct(iucn_sid) # Count the number of distinct species in each cell
  )

# Sum the status, resistance, and recovery scores by cell_id and count the number of species in each cell (second half) 
second_half_status_resistance_recovery_indicator_scores_sum_spp_counts <- second_half_spp %>%
  group_by(cell_id) %>% # All calculations are done by cell ID
  summarize(
    total_status_score = sum(weight), # Sum the status scores
    total_resistance_traits_score = sum(traits_resistance), # Sum the resistance traits scores
    total_recovery_traits_score = sum(traits_recovery), # Sum the recovery traits scores
    total_range_area_score = sum(geom_area_rescaled), # Sum the rescaled range area scores
    species_count = n_distinct(iucn_sid) # Count the number of distinct species in each cell
  )

# Now that we've summarized things and reduced rows, bind the two dfs together for further processing
status_resistance_recovery_indicator_scores_sum_spp_counts_full <- bind_rows(first_half_status_resistance_recovery_indicator_scores_sum_spp_counts, second_half_status_resistance_recovery_indicator_scores_sum_spp_counts)

# Write out the full data with summed scores and species counts by cell
write_csv(status_resistance_recovery_indicator_scores_sum_spp_counts_full, file.path(output_path, "resistance_recovery_indicator_scores_sum_spp_counts_full.csv"))


# Take the average by cell for status, resistance, and recovery by dividing the sums by number of species, and rescale status with a 75% extinction risk
average_status_resistance_recovery_indicator_scores_per_cell <- status_resistance_recovery_indicator_scores_sum_spp_counts_full %>%
  group_by(cell_id) %>% # All calculations are done by cell ID
  summarize(
    total_status_score_summed = sum(total_status_score),
    total_resistance_traits_score_summed = sum(total_resistance_traits_score),
    total_recovery_traits_score_summed = sum(total_recovery_traits_score),
    total_range_area_score_summed = sum(total_range_area_score),
    species_count_summed = sum(species_count)
  ) %>% # Ensure everything is summarized by cell (this is likely already true, but just in case somehow a species was spread between both halves of the combined df). Can probably delete this because I don't think this can happen?
  mutate(
    average_status_score = total_status_score_summed / species_count_summed, # Calculate average status weight by cell
         average_resistance_traits_score = total_resistance_traits_score_summed / species_count_summed, # Calculate average resistance traits score by cell
         average_recovery_traits_score = total_recovery_traits_score_summed / species_count_summed, # Calculate average recovery traits score by cell
         average_range_area_score = total_range_area_score_summed / species_count_summed, # Calculate average rescaled range area score by cell
    rescaled_weight_75_extinction_risk = ifelse(average_status_score < 0.25, 0, (average_status_score - 0.25) / (1 - 0.25)) # Rescale the average status weight to a 0-1 scale based on the 75% extinction risk; resistance and recovery do not get rescaling at this step as it already has 0-1 rescaling
    )


# Prepare the status, resistance traits, recovery traits, and range area (recovery) indicator raster .tif files, unaligned

# Status
# Use a named vector for look-up of the replacement values into the raster
weight_lookup_status <- setNames(average_status_resistance_recovery_indicator_scores_per_cell$rescaled_weight_75_extinction_risk,
                                 average_status_resistance_recovery_indicator_scores_per_cell$cell_id)

# Set up template to fill in
updated_raster_status <- study_area_1km

# Replace the values in the template with the actual values
updated_raster_status[] <- weight_lookup_status[as.character(values(study_area_1km))]

# Write out the status scores raster (unaligned)
writeRaster(updated_raster_status, 
            file.path(int_data_file_path, 
                      "biodiversity_status_scores_unaligned.tif"), 
            overwrite = TRUE)

rm(updated_raster_status) # Free up memory

# Resistance
# Use a named vector for look-up of the replacement values into the raster
weight_lookup_resistance_traits <- setNames(average_status_resistance_recovery_indicator_scores_per_cell$average_resistance_traits_score,
                                     average_status_resistance_recovery_indicator_scores_per_cell$cell_id)

# Set up template to fill in
updated_raster_resistance_traits <- study_area_1km

# Replace the values in the template with the actual values
updated_raster_resistance_traits[] <- weight_lookup_resistance_traits[as.character(values(study_area_1km))]

# Write out the resistance scores raster (unaligned)
writeRaster(updated_raster_resistance_traits, 
            file.path(int_data_file_path, 
                      "biodiversity_resistance_traits_scores_unaligned.tif"), 
            overwrite = TRUE)

rm(updated_raster_resistance_traits) # Free up memory

# Recovery
# Use a named vector for look-up of the replacement values into the raster
weight_lookup_recovery_traits <- setNames(average_status_resistance_recovery_indicator_scores_per_cell$average_recovery_traits_score,
                                   average_status_resistance_recovery_indicator_scores_per_cell$cell_id) 

# Set up template to fill in
updated_raster_recovery_traits <- study_area_1km

# Replace the values in the template with the actual values
updated_raster_recovery_traits[] <- weight_lookup_recovery_traits[as.character(values(study_area_1km))]

# Write out the resilience scores raster
writeRaster(updated_raster_recovery_traits, 
            file.path(int_data_file_path, 
                      "biodiversity_recovery_traits_scores_unaligned.tif"), 
            overwrite = TRUE)

rm(updated_raster_recovery_traits) # Free up memory

# Range area (recovery)
# Use a named vector for look-up of the replacement values into the raster
weight_lookup_range_area <- setNames(average_status_resistance_recovery_indicator_scores_per_cell$average_range_area_score,
                                     average_status_resistance_recovery_indicator_scores_per_cell$cell_id)

# Set up template to fill in
updated_raster_range_area <- study_area_1km

# Replace the values in the template with the actual values
updated_raster_range_area[] <- weight_lookup_range_area[as.character(values(study_area_1km))]

# Write out the range area scores raster (unaligned)
writeRaster(updated_raster_range_area, 
            file.path(int_data_file_path, 
                      "biodiversity_range_area_scores_unaligned.tif"), 
            overwrite = TRUE)

rm(updated_raster_range_area) # Free up memory