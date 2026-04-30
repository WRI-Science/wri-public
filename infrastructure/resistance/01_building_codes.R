wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# Load required library
library(sf)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)
library(purrr)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "infrastructure")
raw_data_file_path <- file.path(wri_project_root, "data", "infrastructure", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "infrastructure", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers")

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "boundary_layers/processed/admin-boundary-layers/wwri_study_area_admin_1.shp")) %>% 
  st_transform(5070)
study_area_admin2_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "boundary_layers/processed/admin-boundary-layers/wwri_study_area_admin_2.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "boundary_layers/processed/admin-boundary-layers/wwri_study_area_raster-mask-lvl-0-90m-with-na.tif"))

human_settlement_layer <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "human_settlement", "human_sett_aligned.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
building_codes <- read.csv(file.path(intermediate_data_file_path, "building_codes.csv")) 

# average the score for each state/province
building_codes$building_codes_avg_score <- rowMeans(building_codes[, 2:5], na.rm = TRUE)

# Apply the building code score to the correct state
state_province_building_codes_score <- left_join(study_area_admin1_shape_5070, building_codes, by = c("name" = "State_province"))

# plot
ggplot() +
  geom_sf(data = state_province_building_codes_score, aes(fill = building_codes_avg_score)) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Infrastructure: Resistance",
       fill = "Building Codes Score") +
  theme(legend.position = "right")

#### Rasterize indicator or score #### 

infrastructure_building_codes <- terra::rasterize(
  state_province_building_codes_score,
  study_area_90m_5070,
  field = "building_codes_avg_score", 
  fun = "mean")

# Align indicator with study_area_90m_template raster
infrastructure_building_codes <- align_raster_to_template(study_area_90m_5070, infrastructure_building_codes, input_type = "categorical")

building_codes <- mask(infrastructure_building_codes, human_settlement_layer)

writeRaster(building_codes, 
            filename = file.path(final_layers_file_path, "2024/infrastructure/indicators/infrastructure_resistance_building_codes.tif"),
            overwrite = TRUE)

plot(building_codes, main = "Infrastructure: Resistance: Building Codes")



