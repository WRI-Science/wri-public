library(sf)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)

# Set base directories
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places"

# Boundary layers
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp"))
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

# Data Layers
wwri_historic_structures <- st_read(file.path(intermediate_data_file_path, "wwri_historic_structures_buffered.gpkg")) %>% 
  rename(geometry = geom)
wwri_national_parks <- st_read(file.path(intermediate_data_file_path, "wwri_national_parks_polygons.shp")) 

# check crs
crs(wwri_historic_structures)
crs(wwri_national_parks)

# Bind historic structures and national parks into one data layer 
wwri_iconic_places <- bind_rows(wwri_historic_structures, wwri_national_parks)

ggplot() +
  # Study area boundary
  geom_sf(
    data = study_area_admin1_shape_5070,
    fill = NA, color = "gray30"
  ) +
  # map fill to a label so it shows up in the legend
  geom_sf(
    data = wwri_national_parks,
    aes(fill = "National Parks"),
    color = "darkgreen"
  ) +
  geom_sf(
    data = wwri_historic_structures,
    aes(fill = "Historic Structures"),
    color = "purple"
  ) +
  # now give those labels their colours
  scale_fill_manual(
    name   = "Iconic Places",
    values = c(
      "National Parks"      = "darkgreen",
      "Historic Structures" = "purple"
    )
  )

# Check geometry types and counts
geometry_types <- st_geometry_type(wwri_iconic_places) %>% 
  table()
# All polygons - 20180
print(geometry_types)

# Convert to terra vector
wwri_iconic_places_vect <- vect(wwri_iconic_places)

# Rasterize onto study area
wwri_iconic_places_status_raster <- terra::rasterize(
  wwri_iconic_places_vect,
  study_area_90m_5070,
  field = 1,
  fun = "max",
  touches = T)

plot(wwri_iconic_places_status_raster, main = "Iconic Place: Status")

# Align indicator with study_area_90m_template raster
wwri_iconic_places_status_raster <- align_raster_to_template(study_area_90m_5070, wwri_iconic_places_status_raster, input_type = "categorical")

# Save to file
writeRaster(wwri_iconic_places_status_raster, 
            filename = file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_status_presence.tif"),
            overwrite = TRUE)

