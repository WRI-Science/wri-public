library(sf)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)
library(viridis)

# Set base directories
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places"

# Boundary layers
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
national_parks <- st_read(file.path(intermediate_data_file_path, "wwri_national_parks_polygons.shp")) 
natural_habitats_resistance <- rast("/home/shares/wwri-wildfire/final_layers/2024/natural_habitats/natural_habitats_resistance.tif")
natural_habitats_recovery <- rast("/home/shares/wwri-wildfire/final_layers/2024/natural_habitats/natural_habitats_recovery.tif")

# Convert to SpatVector
parks_vect <- vect(national_parks)

#### Resistance Calculation ####

# Extract mean resistance value per polygon
parks_resistance_summary <- terra::extract(natural_habitats_resistance, 
                                           parks_vect, 
                                           fun = mean, 
                                           touches = TRUE,
                                           na.rm = TRUE)

# Join extracted values back to sf object
parks_with_resistance <- national_parks %>%
  mutate(ID = row_number()) %>%
  left_join(parks_resistance_summary, by = "ID")

# Plot parks colored by resilience value
ggplot(parks_with_resistance) +
  geom_sf(aes(fill = resistance), color = "black", size = 0.1) +
  scale_fill_viridis(name = "resistance", na.value = "lightgrey") +
  theme_minimal() +
  labs(
    title = "Parks with resistance Scores",
    subtitle = "Mean raster value extracted from Natural Habitats resistance layer"
  )

# Convert sf to SpatVector
parks_vect_resistance <- vect(parks_with_resistance)

# Rasterize indicator or score
sense_of_place_iconic_places_parks_resistance <- terra::rasterize(
  parks_vect_resistance,
  study_area_90m_5070,
  field = "resistance", 
  fun = "mean")

# Align indicator with study_area_90m_template raster
sense_of_place_iconic_places_parks_resistance <- align_raster_to_template(study_area_90m_5070, sense_of_place_iconic_places_parks_resistance, input_type = "continuous")

# Write out to aurora
writeRaster(sense_of_place_iconic_places_parks_resistance, 
            filename = file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_national_parks.tif"),
            overwrite = TRUE)

plot(sense_of_place_iconic_places_parks_resistance, main = "Iconic Places: Resistance: National Parks")

#### Recovery Calculation ####

# Extract mean recovery value per polygon
parks_recovery_summary <- terra::extract(natural_habitats_recovery, 
                                         parks_vect, 
                                         fun = mean, 
                                         touches = TRUE,
                                         na.rm = TRUE)

# Join extracted values back to sf object
parks_with_recovery <- national_parks %>%
  mutate(ID = row_number()) %>%
  left_join(parks_recovery_summary, by = "ID")

# Plot parks colored by recovery value
ggplot(parks_with_recovery) +
  geom_sf(aes(fill = recovery), color = "black", size = 0.1) +
  scale_fill_viridis(name = "Recovery", na.value = "lightgrey") +
  theme_minimal() +
  labs(
    title = "Parks with Recovery Scores",
    subtitle = "Mean raster value extracted from Natural Habitats recovery layer"
  )

# Convert sf to SpatVector
parks_vect_recovery <- vect(parks_with_recovery)

# Rasterize indicator or score
sense_of_place_iconic_places_parks_recovery <- terra::rasterize(
  parks_vect_recovery,
  study_area_90m_5070,
  field = "recovery", 
  fun = "mean")

# Align indicator with study_area_90m_template raster
sense_of_place_iconic_places_parks_recovery <- align_raster_to_template(study_area_90m_5070, sense_of_place_iconic_places_parks_recovery, input_type = "continuous")

# Write out to aurora
writeRaster(sense_of_place_iconic_places_parks_recovery, 
            filename = file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_recovery_national_parks.tif"),
            overwrite = TRUE)

plot(sense_of_place_iconic_places_parks_recovery, main = "Iconic Places: Recovery: National Parks")

