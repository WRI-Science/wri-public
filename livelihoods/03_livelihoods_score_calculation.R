wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf) 
library(tidyverse) 
library(here) 
library(ggplot2) 
library(dplyr)
library(terra)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "livelihoods")
raw_data_file_path <- file.path(wri_project_root, "data", "livelihoods", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "livelihoods", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "livelihoods")

#### Boundary layers ####
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))
human_settlement_layer <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "global_human_settlement_layer", "human_sett_aligned.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

# Read in all necessary data for this script
# Status
median_income <- rast(file.path(final_layers_file_path, "indicators/livelihoods_status_median_income.tif"))
unemployment <- rast(file.path(final_layers_file_path, "indicators/livelihoods_status_unemployment.tif"))
housing_burden <- rast(file.path(final_layers_file_path, "indicators/livelihoods_status_housing_burden.tif"))

# Resistance
job_vulnerability <- rast(file.path(final_layers_file_path, "indicators/livelihoods_resistance_job_vulnerability.tif"))

# Recovery
diversity_of_jobs <- rast(file.path(final_layers_file_path, "indicators/livelihoods_recovery_diversity_of_jobs.tif"))

#### Calculate Status ####
# Take the mean across all status indicators
status <- terra::mean(c(median_income,
                        unemployment,
                        housing_burden),
                      na.rm = TRUE)

# Mask to human settlement layer 
status <- mask(status, human_settlement_layer)

# Rename raster variable to match what is being represented
names(status) <- "status"

# Align indicator with study_area_90m_template raster
status <- align_raster_to_template(study_area_90m_5070, status, input_type = "continuous")

# Write out the status score raster
writeRaster(status,
            file.path(final_layers_file_path, "livelihoods_status.tif"),
            overwrite = TRUE)

#### Calculate Resistance ####

# Take the mean across all resistance indicators
resistance <- terra::mean(c(job_vulnerability), 
                   na.rm = TRUE)

# Mask to human settlement layer 
resistance <- mask(resistance, human_settlement_layer)

# Rename raster variable to match what is being represented
names(resistance) <- "resistance"

# Align indicator with study_area_90m_template raster
resistance <- align_raster_to_template(study_area_90m_5070, resistance, input_type = "continuous")

# Write out the resistance score raster
writeRaster(resistance, 
            file.path(final_layers_file_path, "livelihoods_resistance.tif"), 
            overwrite = TRUE)

#### Calculate Recovery ####

# Take the mean across all recovery indicators
recovery <- terra::mean(c(diversity_of_jobs),
                 na.rm = TRUE)

# Mask to human settlement layer 
recovery <- mask(recovery, human_settlement_layer)

# Align indicator with study_area_90m_template raster
recovery <- align_raster_to_template(study_area_90m_5070, recovery, input_type = "continuous")

# Rename raster variable to match what is being represented
names(recovery) <- "recovery"

# Write out the recovery score raster
writeRaster(recovery, 
            file.path(final_layers_file_path, "livelihoods_recovery.tif"), 
            overwrite = TRUE)

#### Calculate Resilience ####

# status <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_status.tif"))
resistance <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_resistance.tif"))
recovery <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_recovery.tif"))

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

# Mask on status human settlement layer 
resilience <- mask(resilience, human_settlement_layer)

plot(resilience, main = "Livelihoods: Resilience")
plot(is.na(resilience), main = "Livelihoods: Resilience (NA if else)")  

# Align indicator with study_area_90m_template raster
resilience <- align_raster_to_template(study_area_90m_5070, resilience, input_type = "continuous")

# Rename raster variable to match what is being represented
names(resilience) <- "resilience"

# Write out the resilience score raster
writeRaster(resilience, 
            file.path(final_layers_file_path, "livelihoods_resilience.tif"), 
            overwrite = TRUE)

#### Calculate Domain Score ####
resilience <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_resilience.tif"))
  
# Domain Score is calculated as the mean of status and resilience
domain_score <- terra::mean(c(status, 
                              resilience), 
                            na.rm = FALSE)

# Mask on status human settlement layer 
domain_score <- mask(domain_score, human_settlement_layer)

# Align indicator with study_area_90m_template raster
domain_score <- align_raster_to_template(study_area_90m_5070, domain_score, input_type = "continuous")

# multiply by 100 to get scores at 0-100
domain_score <- domain_score*100

# Rename raster variable to match what is being represented
names(domain_score) <- "domain_score"

# Write out the domain score raster
writeRaster(domain_score, 
            file.path(final_layers_file_path, "livelihoods_domain_score.tif"), 
            overwrite = TRUE)

# plot domain scores
plot(domain_score, main = "Livelihoods: Domain Score")

