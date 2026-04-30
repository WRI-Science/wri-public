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
library(tmap)
library(spatstat)
library(gridExtra)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/infrastructure"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/infrastructure/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/infrastructure/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/infrastructure"

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) 
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 
study_area_admin0_shape_moll <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0_moll.shp")) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))
study_area_1km_moll <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_moll.tif"))
study_area_1km_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0.tif"))

moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers Fire Stations ####
us_fire_stations_5070 <- st_read(file.path(raw_data_file_path, "fire_stations/2024/Fire_and_Emergency_Medical_Service_(EMS)_Stations/Fire_and_Emergency_Medical_Service_(EMS)_Stations.shp")) %>% 
  st_transform(5070)

us_fire_stations_5070 <- st_intersection(us_fire_stations_5070, study_area_admin0_shape_5070)

bc_fire_stations_5070 <- st_read(file.path(raw_data_file_path, "fire_stations/2024/BCGW_02001F02_1748559278610_16688/GSR_FIRST_RESPONDERS_SVW/FRST_RSPND_point.shp")) %>% 
  st_transform(5070) %>% 
  filter(RESP_GRP == "FIRE")

yt_fire_stations_5070 <- st_read("/home/shares/wwri-wildfire/data/multi_domain_data/yukon_fire_resources/yukon_fire_departments.shp") %>% 
  st_transform(5070)


#### Join BC, YT, US Hospital Shapefiles convert to moll ####
fire_stations_5070 <- bind_rows(us_fire_stations_5070, bc_fire_stations_5070, yt_fire_stations_5070)

fire_stations_moll <- st_transform(fire_stations_5070, moll_crs)

# Plot Hospital Points onto Study Area Polygons
study_area_admin1_shape_5070_bc <- filter(study_area_admin1_shape_5070, name == "British Columbia")
ggplot() +
  geom_sf(data = study_area_admin1_shape_5070_bc) +
  geom_sf(data = bc_fire_stations_5070, color = "red", size = 0.05)

#### Kernel Density Estimate on fire station locations w. 1km moll study area raster ####

# Extract raster info
r_ext <- ext(study_area_1km_moll)
xmin <- r_ext[1]; xmax <- r_ext[2]; ymin <- r_ext[3]; ymax <- r_ext[4]
ncol_r <- ncol(study_area_1km_moll)
nrow_r <- nrow(study_area_1km_moll)

# Window for spatstat
window <- owin(xrange = c(xmin, xmax), yrange = c(ymin, ymax))

# Unique points
coords_unique <- unique(st_coordinates(fire_stations_moll))
pp <- ppp(coords_unique[,1], coords_unique[,2], window = window)

# KDE
bw <- bw.ppl(pp)
dens <- density.ppp(pp, sigma = bw, dimyx = c(nrow_r, ncol_r), positive = TRUE)

# Convert to terra raster
dens_raster <- rast(dens)
crs(dens_raster) <- crs(study_area_1km_moll)
ext(dens_raster) <- ext(study_area_1km_moll)

plot(dens_raster)

# Mask to study area raster (if NA outside area)
density_rast_masked <- mask(dens_raster, study_area_1km_moll)

# Convert to fire stations per km² if needed
density_per_km2 <- density_rast_masked * 1e6

# Plot
tm_shape(density_per_km2) +
  tm_raster(style = "cont", palette = "-RdYlBu", title = "Fire Station Density (km²)", alpha = 0.7) +
  tm_shape(study_area_admin0_shape_moll) + tm_borders(lwd = 0.09, col = "grey") +
  tm_shape(fire_stations_moll) + tm_dots(size = 0.001, col = "black") +
  tm_layout(
    title = "Fire Station Density (km²)",
    title.position = c("left", "top"),
    legend.outside = FALSE,
    bg.color = "white"
  )

# Rescale between 0 and 1
min_val <- global(density_per_km2, "min", na.rm = TRUE)[[1]]
max_val <- global(density_per_km2, "max", na.rm = TRUE)[[1]]
density_0_1 <- (density_per_km2 - min_val) / (max_val - min_val)

# Reproject to EPSG:5070 
density_0_1_5070 <- project(
  density_0_1, 
  "EPSG:5070", 
  method = "bilinear"
)

# Mask with study area shapefile in 5070
density_0_1_5070_masked <- mask(density_0_1_5070, vect(study_area_admin0_shape_5070))

plot(density_0_1_5070_masked)

#### Rasterize indicator and resample the density raster to study area 90m raster ####

# Align indicator with study_area_90m_template raster
infrastructure_resistance_fire_resource_density <- align_raster_to_template(study_area_90m_5070, density_0_1_5070_masked, input_type = "continuous")

# Plot the final raster
plot(infrastructure_resistance_fire_resource_density, main = "Resistance: Fire Resource Density (EPSG:5070, 90m)")

# Write the updated raster 
writeRaster(infrastructure_resistance_fire_resource_density, 
            file.path(final_layers_file_path, "2024/infrastructure/indicators/infrastructure_resistance_fire_resource_density.tif"), 
            overwrite = TRUE)
