wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# Goal of this script
# The goal of this script is to calculate the resistance, recovery, resilience, status, and domain scores for the infrastructure domain.
# This will use the processed 90 m indicator rasters created throughout the other scripts in this domain.

#### Set Up ####
# Read in necessary packages
library(terra) # For raster manipulation
library(here)
library(dplyr)

# Set base directories
data_file_path <- file.path(wri_project_root, "data", "infrastructure")
final_layers_file_path <- file.path(wri_project_root, "final_layers")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

#### Boundary layers ####
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Indicators ####

# Status
human_settlement_layer <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "human_settlement", "human_sett_aligned.tif"))

# Resistance
building_codes <- rast(file.path(final_layers_file_path, "2024/infrastructure/indicators/infrastructure_resistance_building_codes.tif"))
egress_masked <- rast(file.path(final_layers_file_path, "2023/infrastructure/indicators/infrastructure_resistance_egress_masked.tif"))
fire_resource_proximity <- rast(file.path(final_layers_file_path, "2024/infrastructure/indicators/infrastructure_resistance_fire_resource_density.tif"))
wildland_urban_interface <- rast(file.path(final_layers_file_path, "2024/infrastructure/indicators/infrastructure_resistance_wildland_urban_interface.tif"))
defensible_space <- rast(file.path(final_layers_file_path, "2024/infrastructure/indicators/infrastructure_resistance_d_space.tif"))

# Recovery
homeowners <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_owners.tif"))
median_income <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "indicators", "livelihoods_status_median_income.tif"))
incorporation <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_incorporation.tif"))

#### Calculate Status ####

# Take the mean across all status indicators
status <- human_settlement_layer

# Rename raster variable to match what is being represented
names(status) <- "status"

# Write out the status score raster
writeRaster(status, 
            file.path(final_layers_file_path, "2024/infrastructure/infrastructure_status.tif"), 
            overwrite = TRUE)

#### Calculate Resistance ####

# Take the mean across all resistance indicators
resistance <- mean(c(building_codes, # aligned
                     egress_masked, # aligned
                     fire_resource_proximity, # aligned
                     wildland_urban_interface, # aligned
                     defensible_space), #aligned
                   na.rm = TRUE)

# Rename raster variable to match what is being represented
names(resistance) <- "resistance"

# Align indicator with study_area_90m_template raster
resistance <- align_raster_to_template(study_area_90m_5070, resistance, input_type = "continuous")

resistance <- mask(resistance, human_settlement_layer)

# testing
plot(resistance) # Plot the actual resistance values
plot(is.na(resistance), add=TRUE, col="red")  # Overlay NA pixels in red

plot(resistance == 1)

# Write out the resistance score raster
writeRaster(resistance, 
            file.path(final_layers_file_path, "2024/infrastructure/infrastructure_resistance.tif"), 
            overwrite = TRUE)

#### Calculate Recovery ####

# Take the mean across all recovery indicators
recovery <- mean(c(homeowners, 
                     median_income, 
                     incorporation), 
                   na.rm = TRUE)

plot(homeowners == 1, main = "home")
plot(median_income == 1, main = "income")
plot(incorporation == 1, main = "incorp")

# Rename raster variable to match what is being represented
names(recovery) <- "recovery"

# mask on status human settlement layer 
recovery <- mask(recovery, human_settlement_layer)

# Align indicator with study_area_90m_template raster
recovery <- align_raster_to_template(study_area_90m_5070, recovery, input_type = "continuous")

plot(recovery == 1, main = "Recovery == 1")

# Write out the recovery score raster
writeRaster(recovery, 
            file.path(final_layers_file_path, "2024/infrastructure/infrastructure_recovery.tif"), 
            overwrite = TRUE)

#### Calculate Resilience ####
resistance <- rast(file.path(wri_project_root, "final_layers", "2024", "infrastructure", "infrastructure_resistance.tif"))
#recovery <- rast(file.path(wri_project_root, "final_layers", "2024", "infrastructure", "infrastructure_recovery.tif"))

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

# Rename raster variable to match what is being represented
names(resilience) <- "resilience"

# Align indicator with study_area_90m_template raster
resilience <- align_raster_to_template(study_area_90m_5070, resilience, input_type = "continuous")

# mask on status human settlement layer 
resilience <- mask(resilience, human_settlement_layer)

# Write out the resilience score raster
writeRaster(resilience, 
            file.path(final_layers_file_path, "2024/infrastructure/infrastructure_resilience.tif"), 
            overwrite = TRUE)

#### Calculate Domain Score ####

# Domain Score is calculated as a mean of status and resilience but Status is just the human settlement layer so only a mask is applied to the resilience raster
domain_score <- resilience

# Align indicator with study_area_90m_template raster
domain_score <- align_raster_to_template(study_area_90m_5070, domain_score, input_type = "continuous")

# multiply by 100 to get scores at 0-100
domain_score <- domain_score*100

# Rename raster variable to match what is being represented
names(domain_score) <- "domain_score"

# Write out the domain score raster
writeRaster(domain_score, 
            file.path(final_layers_file_path, "2024/infrastructure/infrastructure_domain_score.tif"), 
            overwrite = TRUE)

#### Plots to check everything out ####
plot(domain_score, main = "Infrastructure Domain Score")
plot(resilience, main = "Infrastructure Resilience")

domain_score <- rast(file.path(wri_project_root, "final_layers", "2024", "infrastructure", "infrastructure_domain_score.tif"))

plot(domain_score == 100)

