library(sf) 
library(tidyverse) 
library(here)
library(ggplot2) 
library(dplyr)
library(readr)
library(purrr)
library(terra)
library(here) # To assemble file paths within project

#### Script Overview #### 
# This script calculates Livelihoods Resistance and Recovery Values.

# Resistance is calculated by analyzing the proportion of jobs in specific industries prone to wildfire disruptions, normalized between 0 and 1 for both countries.
# Recovery is determined by calculating the Shannon diversity index of job types within each region, which measures economic diversity and potential for recovery, normalized within each country.

# Resistance Calculation
# Identifies key industries likely impacted by wildfires using NAICS codes and computes the proportion of total employment these industries represent within each census tract or subdivision.

# Recovery Calculation
# Uses job distribution across various NAICS categories to compute the Shannon diversity index for each area, providing a measure of economic diversity and resilience potential.

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/sense_of_place/iconic_places"

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) %>% 
  st_transform(5070)
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Data Layers ####

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Bulk of script goes here ####

#### Rasterize ####

# Rasterize indicator or score
domain_name_indicator <- terra::rasterize(
  object,
  study_area_90m_5070,
  field = "value", 
  fun = "mean")

# Align indicator with study_area_90m_template raster
indicator_or_scores <- align_raster_to_template(template_raster, domain_name_indicator, input_type = c("categorical", "continuous"))

# Save to aurora
writeRaster(indicator_or_scores, 
            filename = file.path(final_layers_file_path, "air_quality_status_presence_90m.tif"),
            overwrite = TRUE)

