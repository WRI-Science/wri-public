wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# The data are organized in tiles of 100 km x 100 km and follow the EQUI7 tiling grid and projection system. 
# The images are compressed GeoTiff files (*.tif). There is a mosaic in GDAL Virtual format (*.vrt), which can readily be opened in 
# most Geographic Information Systems. Please consider the generation of image pyramids before using *.vrt files.
# The raster dataset contains Wildland-urban interface (WUI) data (one layer), 10 m spatial resolution, 8 discrete classes:
# intermix WUI (where buildings and wildland vegetation intermingle)
# interface WUI (where buildings are close to large wildland vegetation patches)
# 0 - Non-Vegetated / Non-WUI
# 1 - Forest/Shrubland/Wetland-dominated Intermix WUI
# 2 - Forest/Shrubland/Wetland-dominated Interface WUI
# 3 - Grassland-dominated Intermix WUI
# 4 - Grassland -dominated Interface WUI
# 5 - Non-WUI: Forest/Shrub/Wetland-dominated
# 6 - Non-WUI: Grassland-dominated
# 7 - Non-WUI: Urban
# 8 - Non-WUI: Other
# In addition, the data contain tabular data on WUI area, population and biomass in
# the WUI, as well as wildfire area and people affected by wildfire in the WUI per world region, country, subnational administrative unit and biome.
# The data also contain the key algorithm for WUI mapping (also accessible here: https://github.com/franzschug/global_wildland_urban_interface).

# Load required libraries
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
library(pbapply)
library(XML)
library(future.apply)
library(future)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_places")

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp"))
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
wilderness_urban_interface_5070_rescaled <- rast(file.path(wri_project_root, "final_layers", "2024", "infrastructure", "indicators", "infrastructure_resistance_wildland_urban_interface_unmasked.tif"))
# 20096 entries
historic_structures <- st_read(file.path(intermediate_data_file_path, "wwri_historic_structures_buffered.gpkg")) %>% 
  rename(geometry = geom)

# Check CRS
crs(wilderness_urban_interface_5070_rescaled)

# Check geometry types and counts
geometry_types <- st_geometry_type(historic_structures) %>% 
  table()
print(geometry_types)

# Convert to terra vector
wwri_historic_structures_combined_vect <- vect(historic_structures)

# Extract cell values from the WUI raster at each buffered point and polygon
# THIS TOOK 1.5 HOURS TO RUN !! - read in gpkg from intermediate folder
#historic_structures_wui <- st_read(file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "intermediate", "historic_structures_wui_scores.gpkg"))

wui_cell_values <- terra::extract(wilderness_urban_interface_5070_rescaled, 
                                  wwri_historic_structures_combined_vect, 
                                  fun = mean, 
                                  touches = TRUE,
                                  na.rm = T)

# Join extracted values back to sf object
historic_structures_wui <- historic_structures %>%
  mutate(ID = row_number()) %>%
  left_join(wui_cell_values, by = "ID") %>% 
  rename(wui_score = infrastructure_resistance_wildland_urban_interface_unmasked)

#### rescale WUI values ####

# # Round values first because the mean of the area was taken so some values are off
# wwri_historic_structures_rescaled <- historic_structures_wui %>%
#   mutate(wui_score = case_when(
#     round(study_area_wui_map_5070_fixed) == 1 ~ 0,
#     round(study_area_wui_map_5070_fixed) == 2 ~ 0.5,
#     round(study_area_wui_map_5070_fixed) == 3 ~ 0,
#     round(study_area_wui_map_5070_fixed) == 4 ~ 0.5,
#     round(study_area_wui_map_5070_fixed) %in% c(0, 5, 6, 7, 8) ~ 1,
#     TRUE ~ NA_real_
#   ))

hist(historic_structures_wui$wui_score)

# Proportion of “mean” column that is NA
table(is.na(historic_structures_wui$wui_score))
# FALSE  TRUE 
# 20095     1 

# Plot
ggplot() +
  geom_sf(data = study_area_admin1_shape_5070) +
  geom_sf(data = historic_structures_wui,
    aes(colour = wui_score, fill   = wui_score), size = 0.1) +
  scale_colour_viridis_c(option = "plasma", guide = "none") +
  scale_fill_viridis_c(option = "plasma", name = "WUI") +
  theme_minimal() +
  labs(title = "Historic Places by WUI")

# write out geopackage of points and polygons
st_write(historic_structures_wui, 
                  file.path(intermediate_data_file_path, "historic_structures_wui_scores.gpkg"), 
                                    append = FALSE)

#### Rasterize ####

# Rasterize indicator or score
wwri_sense_of_place_iconic_places_wui <- terra::rasterize(
  historic_structures_wui,
  study_area_90m_5070,
  field = "wui_score", 
  fun = "mean")

# Align indicator with study_area_90m_template raster
wwri_sense_of_place_iconic_places_wui <- align_raster_to_template(study_area_90m_5070, wwri_sense_of_place_iconic_places_wui, input_type = "continuous")

# Save to aurora
writeRaster(wwri_sense_of_place_iconic_places_wui, 
            filename = file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_resistance_wui.tif"),
            overwrite = TRUE)

plot(wwri_sense_of_place_iconic_places_wui, main = "Iconic Places: WUI Resistance")

