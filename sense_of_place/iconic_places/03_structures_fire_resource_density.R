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
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_places")

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) 
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
distance_to_nearest_fire_station <- rast(file.path(wri_project_root, "final_layers", "2024", "infrastructure", "indicators", "infrastructure_resistance_fire_resource_density.tif"))
historic_structures <- st_read(file.path(intermediate_data_file_path, "wwri_historic_structures_buffered.gpkg")) %>% 
  rename(geometry = geom)

# Check CRS
crs(distance_to_nearest_fire_station)
crs(historic_structures)

# Check resolution
res(distance_to_nearest_fire_station) # 90 m

# Check extent
ext(distance_to_nearest_fire_station)

# Plot the rasters
plot(distance_to_nearest_fire_station, main = "Distance to Nearest Fire Station")

# Check geometry types and counts
geometry_types <- st_geometry_type(historic_structures) %>% 
  table()
print(geometry_types)

# clean some geometries
wwri_historic_places_clean <- historic_structures %>%
  filter(st_is_valid(.) & !st_is_empty(.))

# Convert to terra vector
wwri_historic_structures_combined_vect <- vect(wwri_historic_places_clean)

# Extract mean distance values from the raster at each buffered point and polygon
# 20406
fire_resource_density <- terra::extract(distance_to_nearest_fire_station, 
                                        wwri_historic_structures_combined_vect, 
                                        fun = mean, 
                                        touches = TRUE,
                                        na.rm = T)

# Join extracted values back to sf object
historic_structures_fire_resource_density <- wwri_historic_places_clean %>%
  mutate(ID = row_number()) %>%
  left_join(fire_resource_density, by = "ID")

# Rename the distance column
historic_structures_fire_resource_density <- historic_structures_fire_resource_density %>%
  rename(distance_to_nearest_fire_station = lyr.1)

# Plot 
ggplot() +
  geom_sf(data = study_area_admin1_shape_5070) +
  geom_sf(
    data = historic_structures_fire_resource_density,
    aes(
      colour = distance_to_nearest_fire_station,
      fill   = distance_to_nearest_fire_station
    ),
    size = 0.1
  ) +
  scale_colour_viridis_c(option = "plasma", guide = "none") +
  scale_fill_viridis_c(option = "plasma", name = "Fire Station\nProximity") +
  theme_minimal() +
  labs(title = "Historic Structures by Proximity to Fire Station")


# write out geopackage of points and polygons
st_write(historic_structures_fire_resource_density, 
         file.path(intermediate_data_file_path, "historic_structures_proximity_to_fire_resources_scores.gpkg"), 
         append = FALSE)

#### Rasterize indicator or score #### 
sense_of_place_iconic_places_fire_resources_density <- terra::rasterize(
  historic_structures_fire_resource_density,
  study_area_90m_5070,
  field = "distance_to_nearest_fire_station", 
  fun = "mean")

# Align indicator with study_area_90m_template raster
sense_of_place_iconic_places_fire_resources_density <- align_raster_to_template(study_area_90m_5070, sense_of_place_iconic_places_fire_resources_density, input_type = "continuous")

# Save raster to aurora
writeRaster(sense_of_place_iconic_places_fire_resources_density, 
            filename = file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_fire_resource_density.tif"),
            overwrite = TRUE)

plot(sense_of_place_iconic_places_fire_resources_density, main = "Iconic Places: Resistance: Fire Resource Density")

