#### Set Up ####
# Read in necessary packages
library(terra) 
library(here)
library(dplyr)

# Set base directories
data_file_path <- "/home/shares/wwri-wildfire/data/air_quality"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/air_quality"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

#### Boundary layers ####
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Indicators ####
# Status
days_above_aqi_100 <- rast(file.path(final_layers_file_path, "indicators/air_quality_status_aqi_100.tif"))
days_above_aqi_300 <- rast(file.path(final_layers_file_path, "indicators/air_quality_status_aqi_300.tif"))

# Resistance
asthma_prevalence <- rast(file.path(final_layers_file_path, "indicators/air_quality_resistance_asthma.tif"))
copd_prevalence <- rast(file.path(final_layers_file_path, "indicators/air_quality_resistance_copd.tif"))
hospital_density <- rast(file.path(final_layers_file_path, "indicators/air_quality_resistance_hospital_density.tif"))
vulnerable_populations <- rast(file.path(final_layers_file_path, "indicators/air_quality_resistance_vulnerable_populations.tif"))
vulnerable_workers <- rast(file.path(final_layers_file_path, "indicators/air_quality_resistance_vulnerable_workers.tif"))

# Recovery
# No recovery in air_quality 

#### Calculate Status ####
# Take the mean across all status indicators
status <- terra::mean(c(days_above_aqi_100,
                        days_above_aqi_300),
                      na.rm = TRUE)

# Rename raster variable to match what is being represented
names(status) <- "status"

# Align indicator with study_area_90m_template raster
status <- align_raster_to_template(study_area_90m_5070, status, input_type = "continuous")

# Write out the status score raster
writeRaster(status, 
            file.path(final_layers_file_path, "air_quality_status.tif"), 
            overwrite = TRUE)

#### Calculate Resistance ####
# Take the mean across all resistance indicators
resistance <- terra::mean(c(asthma_prevalence,
                            copd_prevalence, 
                            hospital_density,
                            vulnerable_populations,
                            vulnerable_workers),
                          na.rm = TRUE)

# Rename raster variable to match what is being represented
names(resistance) <- "resistance"

# Align indicator with study_area_90m_template raster
resistance <- align_raster_to_template(study_area_90m_5070, resistance, input_type = "continuous")

# Write out the resistance score raster
writeRaster(resistance, 
            file.path(final_layers_file_path, "air_quality_resistance.tif"), 
            overwrite = TRUE)

#### Calculate Resilience ####
# Resilience is resistance here in air quality
resilience <- resistance

# Rename raster variable to match what is being represented
names(resilience) <- "resilience"

# Align indicator with study_area_90m_template raster
resilience <- align_raster_to_template(study_area_90m_5070, resilience, input_type = "continuous")

# Write out the resistance score raster
writeRaster(resilience, 
            file.path(final_layers_file_path, "air_quality_resilience.tif"), 
            overwrite = TRUE)

#### Calculate Domain Score ####
status <- rast("/home/shares/wwri-wildfire/final_layers/2024/air_quality/air_quality_status.tif")
# Domain Score is calculated as the mean of status and resilience
domain_score <- terra::mean(c(status, 
                              resilience), 
                            na.rm = TRUE)

# Align indicator with study_area_90m_template raster
domain_score <- align_raster_to_template(study_area_90m_5070, domain_score, input_type = "continuous")

# multiply by 100 to get scores at 0-100
domain_score <- domain_score*100

# Rename raster variable to match what is being represented
names(domain_score) <- "domain_score"

# Write out the domain score raster
writeRaster(domain_score, 
            file.path(final_layers_file_path, "air_quality_domain_score.tif"), 
            overwrite = TRUE)

plot(domain_score, main = "Air Quality: Domain Score")
plot(resilience, main = "Air Quality: resilience")
plot(resistance, main = "Air Quality: resistance")
plot(status, main = "Air Quality: status")

