# script to visualize National Parks

library(sf)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)

# Set base directories
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_places/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/sense_of_place/iconic_places"

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp"))
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp"))
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_with_na.tif"))

#### Data Layers ####
us_national_parks <- st_read(file.path(raw_data_file_path, "Administrative_Boundaries of_National Park_System_Units/Administrative Boundaries of National Park System Units.shp")) %>% 
  st_transform(5070) %>% 
  st_make_valid()
can_national_parks <- st_read(file.path(raw_data_file_path, "vw_Places_Public_lieux_public_APCA/vw_Places_Public_lieux_public_APCA.shp")) %>% 
  st_transform(5070) %>% 
  st_make_valid()

#### US National Parks ####

# Define the states of interest
states_of_interest <- c("WA", "CA", "OR", "ID", "AZ", "NV", "WY", "CO", "UT", "MT", "NM", "AK")

# 146 polygons 
us_national_parks_filtered <- us_national_parks %>% 
  filter(STATE %in% states_of_interest)

# List of UNIT_TYPEs to remove because they already exist in NPS historic structures database 
types_to_exclude <- c(
  "National Battlefield", 
  "National Historic Site", 
  "National Historical Park", 
  "National Memorial", 
  "National Monument")

# Filter them out
# 68
us_national_parks_filtered_cleaned <- us_national_parks_filtered %>%
  filter(!UNIT_TYPE %in% types_to_exclude)

#### Canadian National Parks ####

# get count of park types
table(can_national_parks$PLACE_TYPE)

#### Combine CAN + US national park layers ####
wwri_national_parks <- bind_rows(us_national_parks_filtered_cleaned, can_national_parks) %>% 
  # remove shape area column bc it throws errors
  dplyr::select(-c(Shape_Area, Shape__Are)) 

# Intersect with wwri boundary  
wwri_national_parks <- st_intersection(wwri_national_parks, study_area_admin0_shape_5070)

ggplot() +
  geom_sf(data = study_area_admin1_shape_5070, fill = NA, color = "grey30", linewidth = 0.5) +
  geom_sf(data = wwri_national_parks, fill = "red", color = NA, alpha = 0.6) +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "National Parks")

# write out national park layer to server 
st_write(wwri_national_parks, file.path(intermediate_data_file_path, "wwri_national_parks_polygons.shp"), append = F)
