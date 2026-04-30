# Goal of this script
# The goal of this script is to calculate the resistance, recovery, resilience, status, and domain scores for the ____ domain.
# This will use the processed 90 m indicator rasters created throughout the other scripts in this domain.

#### Set Up ####
# Read in necessary packages
library(terra) 
library(here)
library(dplyr)

# Set base directories
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

#### Boundary layers ####
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
# Status
status_presence_absence <- rast(file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_status_presence.tif"))

# Resistance
egress <- rast(file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_egress.tif"))
fire_resource_density <- rast(file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_fire_resource_density.tif"))
wui <- rast(file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_wui.tif"))
resistance_national_parks <- rast(file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_national_parks.tif"))

# Recovery
degree_of_protection <- rast(file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_recovery_degree_of_protection.tif"))
recovery_national_parks <- rast(file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_recovery_national_parks.tif"))

#### Calculate Status ####
status <- status_presence_absence

# Align indicator with study_area_90m_template raster
status <- align_raster_to_template(study_area_90m_5070, status, input_type = "categorical")

plot(status, main = "Iconic Places: Status")

# Rename raster variable to match what is being represented
names(status) <- "status"

# Write out the status score raster
writeRaster(status,
            file.path(final_layers_file_path, "sense_of_place_iconic_places_status.tif"),
            overwrite = TRUE)

#### Calculate Resistance ####

# Take the mean across all resistance indicators
resistance <- terra::mean(c(wui, 
                     egress,
                     fire_resource_density,
                     resistance_national_parks), 
                   na.rm = TRUE)

# Rename raster variable to match what is being represented
names(resistance) <- "resistance"

# Align indicator with study_area_90m_template raster
resistance <- align_raster_to_template(study_area_90m_5070, resistance, input_type = "continuous")

# Write out the resistance score raster
writeRaster(resistance, 
            file.path(final_layers_file_path, "sense_of_place_iconic_places_resistance.tif"), 
            overwrite = TRUE)

#### Calculate Recovery ####

# Take the mean across all recovery indicators
recovery <- terra::mean(c(degree_of_protection, 
                   recovery_national_parks),
                 na.rm = TRUE)

# Align indicator with study_area_90m_template raster
recovery <- align_raster_to_template(study_area_90m_5070, recovery, input_type = "continuous")

# Rename raster variable to match what is being represented
names(recovery) <- "recovery"

# Write out the recovery score raster
writeRaster(recovery, 
            file.path(final_layers_file_path, "sense_of_place_iconic_places_recovery.tif"), 
            overwrite = TRUE)

#### Calculate Resilience ####

# resistance <- rast(file.path(final_layers_file_path, "sense_of_place_iconic_places_resistance.tif"))
# recovery <- rast(file.path(final_layers_file_path, "sense_of_place_iconic_places_recovery.tif"))

# Resilience is calculated as 1 - (1 - Resistance) * (1 - Recovery)
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

resilience <- calc_resilience(resistance, recovery)

plot(resilience, main = "Iconic Places: Resilience")
plot(is.na(resilience), main = "Iconic Places: Resilience (NA if else)")  

# Align indicator with study_area_90m_template raster
resilience <- align_raster_to_template(study_area_90m_5070, resilience, input_type = "continuous")

# Rename raster variable to match what is being represented
names(resilience) <- "resilience"

# Write out the resilience score raster
writeRaster(resilience, 
            file.path(final_layers_file_path, "sense_of_place_iconic_places_resilience.tif"), 
            overwrite = TRUE)

#### Calculate Domain Score ####

# Domain Score is calculated as the mean of status and resilience
# For iconic places status is 1 or NA (presence/absence)
domain_score <- resilience

# Align indicator with study_area_90m_template raster
domain_score <- align_raster_to_template(study_area_90m_5070, domain_score, input_type = "continuous")

# multiply by 100 to get scores at 0-100
domain_score <- domain_score*100

# Rename raster variable to match what is being represented
names(domain_score) <- "domain_score"

# Write out the domain score raster
writeRaster(domain_score, 
            file.path(final_layers_file_path, "sense_of_place_iconic_places_domain_score.tif"), 
            overwrite = TRUE)

# plot domain scores
plot(rast("/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places/sense_of_place_iconic_places_domain_score.tif"), 
     main = "Sense of Place: Iconic Places Mean Domain Score")

