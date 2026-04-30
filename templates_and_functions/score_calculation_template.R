# Goal of this script
# The goal of this script is to calculate the resistance, recovery, resilience, status, and domain scores for the ____ domain.
# This will use the processed 90 m indicator rasters created throughout the other scripts in this domain.

#### Set Up ####
# Read in necessary packages
library(terra) # For raster manipulation
library(here)
library(dplyr)

# Set base directories
data_file_path <- "/home/shares/wwri-wildfire/data/domain_name"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2023/domain_name"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "boundary_layers/processed/admin-boundary-layers/wwri_study_area_admin_1.shp")) %>% 
  st_transform(5070)
study_area_admin2_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "boundary_layers/processed/admin-boundary-layers/wwri_study_area_admin_2.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "boundary_layers/processed/admin-boundary-layers/wwri_study_area_raster-mask-lvl-0-90m-with-na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Indicators ####
# Status
indicator_1 <- rast(file.path(data_file_path, "indicators/indicator_1.tif"))
indicator_2 <- rast(file.path(data_file_path, "indicators/indicator_2.tif"))
# Resistance
indicator_3 <- rast(file.path(data_file_path, "indicators/indicator_3.tif"))
indicator_4 <- rast(file.path(data_file_path, "indicators/indicator_4.tif"))
indicator_5 <- rast(file.path(data_file_path, "indicators/indicator_5.tif"))
# Recovery
indicator_6 <- rast(file.path(data_file_path, "indicators/indicator_6.tif"))
indicator_7 <- rast(file.path(data_file_path, "indicators/indicator_7.tif"))
# Study area raster (for masking/cropping to consistently)
study_area_rast <- rast(file.path(multi_domain_data_file_path, "study_area/study_area.shp"))

#### Calculate Status ####
# Take the mean across all status indicators
status <- terra::mean(c(indicator_1,
                        indicator_2),
                      na.rm = TRUE)

# Rename raster variable to match what is being represented
names(status) <- "status"

# Align indicator with study_area_90m_template raster
indicator_or_scores <- align_raster_to_template(template_raster, domain_name_indicator, input_type = c("categorical", "continuous"))

# Write out the status score raster
writeRaster(status, 
            file.path(final_layers_file_path, "domain_name_resistance.tif"), 
            overwrite = TRUE)

#### Calculate Resistance ####
# Take the mean across all resistance indicators
resistance <- terra::mean(c(indicator_3,
                            indicator_4, 
                            indicator_5),
                          na.rm = TRUE)

# Rename raster variable to match what is being represented
names(resistance) <- "resistance"

# Align indicator with study_area_90m_template raster
indicator_or_scores <- align_raster_to_template(template_raster, domain_name_indicator, input_type = c("categorical", "continuous"))

# Write out the resistance score raster
writeRaster(resistance, 
            file.path(final_layers_file_path, "domain_name_resilience.tif"), 
            overwrite = TRUE)

#### Calculate Recovery ####
# Take the mean across all recovery indicators
recovery <- terra::mean(c(indicator_6,
                          indicator_7),
                        na.rm = TRUE)

# Rename raster variable to match what is being represented
names(recovery) <- "recovery"

# Align indicator with study_area_90m_template raster
indicator_or_scores <- align_raster_to_template(template_raster, domain_name_indicator, input_type = c("categorical", "continuous"))

# Write out the recovery score raster
writeRaster(recovery, 
            file.path(final_layers_file_path, "domain_name_recovery.tif"), 
            overwrite = TRUE)

#### Calculate Resilience ####
# Resilience is calculated as 1 - (1 - Resistance) * (1 - Recovery)
resilience <- 1 - ((1 - resistance) * (1 - recovery))

# Rename raster variable to match what is being represented
names(resilience) <- "resilience"

# Align indicator with study_area_90m_template raster
indicator_or_scores <- align_raster_to_template(template_raster, domain_name_indicator, input_type = c("categorical", "continuous"))

# Write out the resilience score raster
writeRaster(resilience, 
            file.path(final_layers_file_path, "domain_name_resilience.tif"), 
            overwrite = TRUE)

#### Calculate Domain Score ####

# Domain Score is calculated as the mean of status and resilience
domain_score <- terra::mean(c(status, 
                              resilience), 
                            na.rm = TRUE)

# multiply by 100 to get scores at 0-100
domain_score <- domain_score*100

# Rename raster variable to match what is being represented
names(domain_score) <- "domain_score"

# Align indicator with study_area_90m_template raster
indicator_or_scores <- align_raster_to_template(template_raster, domain_name_indicator, input_type = c("categorical", "continuous"))

# Write out the domain score raster
writeRaster(domain_score, 
            file.path(final_layers_file_path, "domain_name_domain_score.tif"), 
            overwrite = TRUE)

#### Plots to check everything out ####
# plot(status)
# plot(resistance)
# plot(recovery)
# plot(resilience)