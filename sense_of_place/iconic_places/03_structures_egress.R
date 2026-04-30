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
library(here)

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
# use the unmasked road raster
egress_layer <- rast(file.path(wri_project_root, "final_layers", "2023", "infrastructure", "indicators", "infrastructure_resistance_egress.tif"))
historic_structures <- st_read(file.path(intermediate_data_file_path, "wwri_historic_structures_buffered.gpkg")) %>% 
  rename(geometry = geom)

# Plot the rasters
plot(egress_layer, main = "Egress")

# Check geometry types and counts
geometry_types <- st_geometry_type(historic_structures) %>% 
  table()
print(geometry_types)

# clean some geometries
wwri_historic_places_clean <- historic_structures %>%
  filter(st_is_valid(.) & !st_is_empty(.))

# Convert to terra vector
wwri_historic_structures_combined_vect <- vect(wwri_historic_places_clean)

ext(wwri_historic_structures_combined_vect)

# Extract mean distance values from the raster at each buffered point and polygon
# 20096
egress_scores <- terra::extract(
  egress_layer,
  wwri_historic_structures_combined_vect,
  fun     = mean,
  na.rm   = TRUE,
  touches = TRUE
)

hist(egress_scores$exits_divided_by_2_rescaled)

# Proportion of “mean” column that is NA
table(is.na(egress_scores[,2]))
# FALSE  TRUE 
# 20091     5 

# Join extracted values back to sf object
historic_structures_egress <- wwri_historic_places_clean %>%
  mutate(ID = row_number()) %>%
  left_join(egress_scores, by = "ID")

# Plot
ggplot() +
  geom_sf(data = study_area_admin1_shape_5070) +
  geom_sf(
    data = historic_structures_egress,
    aes(
      colour = exits_divided_by_2_rescaled,
      fill   = exits_divided_by_2_rescaled
    ),
    size = 0.1
  ) +
  scale_colour_viridis_c(option = "plasma", guide = "none") +
  scale_fill_viridis_c(option = "plasma", name = "Egress rescaled") +
  theme_minimal() +
  labs(title = "Historic Places by Egress")

# write out geopackage of points and polygons
st_write(historic_structures_egress, 
         file.path(intermediate_data_file_path, "historic_structures_egress_scores.gpkg"), 
         append = FALSE)

#### Rasterize indicator or score #### 
sense_of_place_iconic_places_egress <- terra::rasterize(
  historic_structures_egress,
  study_area_90m_5070,
  field = "exits_divided_by_2_rescaled", 
  fun = "mean")

plot(sense_of_place_iconic_places_egress, main = "Iconic Places: Resistance: Egress")

# Align indicator with study_area_90m_template raster
sense_of_place_iconic_places_egress <- align_raster_to_template(study_area_90m_5070, sense_of_place_iconic_places_egress, input_type = "continuous")

# Save raster to aurora
writeRaster(sense_of_place_iconic_places_egress, 
            filename = file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_egress.tif"),
            overwrite = TRUE)


