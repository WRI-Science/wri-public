library(terra) # For raster operations
library(here) # For contructing file path

#### Script Overview ####
# This script aligns the unaligned status, resistance, and recovery rasters to our 90 m study area template and calculates the resilience and domain score rasters.


#### Base Directories ####
data_file_path <- "/home/shares/wwri-wildfire/data/biodiversity"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers"
multi_domain_data_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path_prev <- file.path(int_data_file_path, "species_dfs_status") # output_path from previous steps of status processing
output_path <- file.path(int_data_file_path, "species_dfs_resilience") # output_path for resilience files
final_layers_output_path <- file.path(final_layers_file_path, path_year, "biodiversity") # output path for final layers


#### Boundary Layers ####
# Read in study area raster template
study_area_template <- rast(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))


#### Data Layers ####
# Read in the unaligned status, resistance, and recovery rasters
status_scores <- rast(file.path(int_data_file_path, "biodiversity_status_scores_unaligned.tif"))
resistance_traits_scores <- rast(file.path(int_data_file_path, "biodiversity_resistance_traits_scores_unaligned.tif"))
recovery_traits_scores <- rast(file.path(int_data_file_path, "biodiversity_recovery_traits_scores_unaligned.tif"))
range_area_scores <- rast(file.path(int_data_file_path, "biodiversity_range_area_scores_unaligned.tif"))


#### Functions ####
# Function to align rasters to a template raster
source(here("templates_and_functions", "align_raster_to_template.R")) 

# Function to calculate resilience
calc_resilience <- function(resistance, recovery) {
  message("-- Computing resilience composite via vector math + cover()")
  
  # 1) pure composite where both exist
  composite <- 1 - (1 - resistance) * (1 - recovery)
  names(composite) <- "resilience_temp"
  
  # 2) if composite is NA but resistance exists, fall back to resistance
  step2 <- cover(composite, resistance)
  
  # 3) if still NA but recovery exists, fall back to recovery
  resilience <- cover(step2, recovery)
  names(resilience) <- "resilience"
  
  return(resilience)
}


#### Data Processing ####
# Align final rasters
status_scores_aligned <- align_raster_to_template(study_area_template, status_scores) # Align status scores to the template
rm(status_scores) # Free up memory

resistance_traits_scores_aligned <- align_raster_to_template(study_area_template, resistance_traits_scores) # Align resistance scores to the template
rm(resistance_traits_scores) # Free up memory

recovery_traits_scores_aligned <- align_raster_to_template(study_area_template, recovery_traits_scores) # Align recovery scores to the template
rm(recovery_traits_scores) # Free up memory

range_area_scores_aligned <- align_raster_to_template(study_area_template, range_area_scores) # Align range area scores to the template
rm(range_area_scores) # Free up memory

# Calculate resistance and recovery
resistance_scores_aligned <- resistance_traits_scores_aligned # Resistance scores are the same as resistance traits scores
recovery_stack <- c(recovery_traits_scores_aligned, range_area_scores_aligned)
recovery_scores_aligned <- terra::mean(recovery_stack, na.rm = TRUE) # Recovery scores are the mean of recovery traits scores and range area scores

# Calculate resilience scores with resistance and recovery
resilience_scores_aligned <- calc_resilience(resistance_scores_aligned, recovery_scores_aligned)

# Calculate domain score as the average of status and resilience
domain_score_stack <- c(status_scores_aligned, resilience_scores_aligned)
domain_scores_aligned <- terra::mean(domain_score_stack, na.rm = TRUE) 

# Set domain score as 0-100 for viz/further analysis
domain_scores_aligned <- domain_scores_aligned * 100

# Rename raster variables for clarity
names(resistance_traits_scores_aligned) <- "biodiversity_resistance_traits"
names(recovery_traits_scores_aligned) <- "biodiversity_recovery_traits"
names(range_area_scores_aligned) <- "biodiversity_recovery_range_area"
names(resistance_scores_aligned) <- "biodiversity_resistance"
names(recovery_scores_aligned) <- "biodiversity_recovery"
names(status_scores_aligned) <- "biodiversity_status"
names(resilience_scores_aligned) <- "biodiversity_resilience"
names(domain_scores_aligned) <- "biodiversity_domain_score"

# Write out indicators
writeRaster(resistance_traits_scores_aligned, file.path(final_layers_output_path, "indicators", "biodiversity_resistance_traits.tif"), overwrite = TRUE) # Write resistance traits scores raster
rm(resistance_traits_scores_aligned) # Free up memory

writeRaster(recovery_traits_scores_aligned, file.path(final_layers_output_path, "indicators", "biodiversity_recovery_traits.tif"), overwrite = TRUE) # Write recovery traits scores raster
rm(recovery_traits_scores_aligned) # Free up memory

writeRaster(range_area_scores_aligned, file.path(final_layers_output_path, "indicators", "biodiversity_recovery_range_area.tif"), overwrite = TRUE) # Write range area scores raster
rm(range_area_scores_aligned) # Free up memory

writeRaster(status_scores_aligned, file.path(final_layers_output_path, "indicators", "biodiversity_status.tif"), overwrite = TRUE) # Write status scores raster to indicators
rm(status_scores_aligned) # Free up memory

# Write out the other final layers
writeRaster(resistance_scores_aligned, file.path(final_layers_output_path, "biodiversity_resistance.tif"), overwrite = TRUE) # Write resistance scores raster
rm(resistance_scores_aligned) # Free up memory

writeRaster(recovery_scores_aligned, file.path(final_layers_output_path, "biodiversity_recovery.tif"), overwrite = TRUE) # Write recovery scores raster
rm(recovery_scores_aligned) # Free up memory

writeRaster(status_scores_aligned, file.path(final_layers_output_path, "biodiversity_status.tif"), overwrite = TRUE) # Write status scores raster to final layers
rm(status_scores_aligned) # Free up memory

writeRaster(resilience_scores_aligned, file.path(final_layers_output_path, "biodiversity_resilience.tif"), overwrite = TRUE) # Write resilience scores raster
rm(resilience_scores_aligned) # Free up memory

writeRaster(domain_scores_aligned, file.path(final_layers_output_path, "biodiversity_domain_score.tif"), overwrite = TRUE) # Write domain scores raster
rm(domain_scores_aligned) # Free up memory