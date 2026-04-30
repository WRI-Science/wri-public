library(sf)
library(sp)
library(gstat)
library(raster)
library(terra)
library(tidyverse)
library(httr)
library(giscoR)
library(classInt)
library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)
library(progressr)
library(here)

#### Base directories ####
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/air_quality"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/air_quality"

#### Boundary layers ####
study_area_admin_0_expanded_eastward_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0_expanded_eastward.shp"))
study_area_admin_0_moll_expanded_eastward <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0_moll_expanded_eastward.shp"))
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

# Function to combine and process the US and CAN AQI data frames
combine_aqi_data <- function(us_df, canadian_df) {
  # Convert all column names to lowercase for both dataframes
  colnames(us_df) <- tolower(colnames(us_df))
  colnames(canadian_df) <- tolower(colnames(canadian_df))
  
  # Standardize Canadian column names to match US columns
  canadian_df <- canadian_df %>%
    dplyr::rename(
      defining_site = siteid,
      local_site_name = sitename,
      owning_agency = agencyname
    )
  
  # Only keep columns that are present in both dataframes, or those you want
  canadian_df <- canadian_df %>%
    dplyr::select(defining_site, local_site_name, owning_agency, latitude, longitude, days_above_300)
  
  us_df <- us_df %>%
    dplyr::select(defining_site, county_name, local_site_name, owning_agency, latitude, longitude, days_above_300)
  
  # Combine
  combined_df <- dplyr::bind_rows(us_df, canadian_df)
  return(combined_df)
}

#### Data Layers ####
us_days_above_300_2024 <- read_csv(file.path(intermediate_data_file_path, "aqi/us_days_above_300_2024.csv"))
canada_days_above_300_2024 <- read_csv(file.path(intermediate_data_file_path, "aqi/canada_days_above_300_2024.csv"))

#### Combine the US and Canada count of days > AQI 100 #### 

# Run the function
us_can_days_above_300_combined_2024 <- combine_aqi_data(us_days_above_300_2024, canada_days_above_300_2024)

# summary stats
us_can_days_above_300_combined_2024_stats <- us_can_days_above_300_combined_2024 %>%
  filter(days_above_300 != 0) %>%
  summarise(
    n = n(),
    min = min(days_above_300, na.rm = TRUE),
    q1 = quantile(days_above_300, 0.25, na.rm = TRUE),
    median = median(days_above_300, na.rm = TRUE),
    mean = mean(days_above_300, na.rm = TRUE),
    q3 = quantile(days_above_300, 0.75, na.rm = TRUE),
    max = max(days_above_300, na.rm = TRUE),
    sd = sd(days_above_300, na.rm = TRUE),
    sd2 = 2 * sd(days_above_300, na.rm = TRUE),
    sd3 = 3 * sd(days_above_300, na.rm = TRUE),
    p1 = quantile(days_above_300, 0.01, na.rm = TRUE),
    p5 = quantile(days_above_300, 0.05, na.rm = TRUE),
    p95 = quantile(days_above_300, 0.95, na.rm = TRUE),
    p99 = quantile(days_above_300, 0.99, na.rm = TRUE)
  )

print(us_can_days_above_300_combined_2024_stats)

# # Rescale values and move geometry column
us_can_days_above_300_combined_2024 <- us_can_days_above_300_combined_2024 %>%
  mutate(
    days_above_300_rescaled = pmax(0, pmin(1, scales::rescale(days_above_300, to = c(1, 0), from = c(0, 2.83))))
  )

# plot sensor coordinates in 4326
ggplot(us_can_days_above_300_combined_2024, aes(x = longitude, y = latitude)) +
  borders("world", colour = "gray85", fill = "gray80") +
  coord_quickmap(xlim = c(-179.1, -95), ylim = c(27, 71.4)) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = "2024 Latitude and Longitude Points for US + CAN monitoring sites",
       x = "Longitude",
       y = "Latitude")

# read in days above 300 aqi and transform to mollweide
us_can_days_above_300_2024_moll <- us_can_days_above_300_combined_2024 %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%  # Convert to sf with point geometry
  sf::st_set_crs(4326) %>%  # Set the CRS to WGS84 (native lat/lon)
  st_transform(moll_crs)        # Transform to Mollweide projection

# write out mollweide shapefile point data 
sf::st_write(
  us_can_days_above_300_2024_moll,
  dsn = file.path(intermediate_data_file_path, "us_can_days_above_300_combined_2024_moll.shp"),
  delete_dsn = TRUE,
  quiet = FALSE
)

#### Load WWRI Study Area county level shapefiles ####

# load in 5km grid so it doesn't have to be remade 
country_grid <- sf::st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/study_area_grid_expanded_east_moll_5km.gpkg"))

# # gstat::idw() does not take a raster so have to create an sf grid
# # need to use the expanded eastward molleweide shapefile for equal distance calculations and interpolation eastward
# country_grid <- st_make_grid(
#   study_area_admin_0_moll_expanded_eastward,
#   cellsize = units::as_units(5, "km"),
#   what = "polygons",
#   square = TRUE
# ) |>
#   st_intersection(st_buffer(study_area_admin_0_moll_expanded_eastward, 0)) |>
#   st_as_sf() |>
#   st_make_valid()
# 
# sf::st_geometry(country_grid) <- "geometry"
# 
# sf::st_write(country_grid, file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/study_area_grid_expanded_east_moll_5km.gpkg"))


#### parallelize idw ####

# Split the grid into chunks
n_chunks <- 80

grid_chunks <- split(country_grid, cut(seq(nrow(country_grid)), n_chunks, labels = FALSE))

# Set up parallel backend
cl <- makeCluster(n_chunks)
registerDoParallel(cl)

# Function to perform IDW on a chunk
idw_chunk <- function(chunk, data) {
  gstat::idw(
    days_above_300_rescaled ~ 1,
    locations = data,
    newdata = chunk,
    idp = 5,
    nmax = 10
  ) |> 
    dplyr::select(1:3) |>
    dplyr::rename(aqi = var1.pred)
}

# Perform IDW interpolation in parallel
aqi_interp_list <- foreach(chunk = grid_chunks, .packages = c("sf", "gstat", "dplyr")) %dopar% {
  idw_chunk(chunk, us_can_days_above_300_2024_moll)
}

# Combine results
aqi_interp <- do.call(rbind, aqi_interp_list)

# Stop parallel backend
stopCluster(cl)

# convert back to 5070 for visual purposes 
aqi_interp_5070 <- aqi_interp %>%
  sf::st_as_sf(coords = c("x", "y"), crs = moll_crs) %>%
  sf::st_transform(5070)  # Transform to EPSG:5070

#### Visualize idw ####

# crop the aqi_interp to the study area
aqi_interp_5070_cropped <- aqi_interp_5070 %>%
  sf::st_crop(study_area_admin0_shape_5070) %>%
  sf::st_intersection(study_area_admin0_shape_5070)

ggplot(data = aqi_interp_5070_cropped) +
  geom_sf(aes(fill = aqi), color = NA) +
  theme_void() +
  scale_fill_viridis_c(
    name = "Status",
    na.value = "grey80",
    option = "viridis", 
    direction = 1
  ) +
  labs(
    title = "AQI days > 300",
    subtitle = "2024; 5km",
    fill = "Status"
  ) +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))


#### Floors & Ceilings: Scale the values between 0-52 AQI days above 100 ####

# # Rescale values and move geometry column
# aqi_interp_5070_cropped_rescaled <- aqi_interp_5070_cropped %>%
#   mutate(
#     aqi_rescaled_0_52 = scales::rescale(aqi, to = c(0, 52)),
#     aqi_rescaled_0_1 = scales::rescale(aqi_rescaled_0_52, to = c(1, 0))
#   ) %>%
#   relocate(geometry, .after = last_col())
# 
# ggplot(data = aqi_interp_5070_cropped_rescaled) +
#   geom_sf(aes(fill = aqi_rescaled_0_1), color = NA) +
#   theme_void() +
#   scale_fill_viridis_c(
#     name = "Status",
#     na.value = "grey80",
#     option = "viridis", # other options: "magma", "inferno", "plasma", "cividis", etc.
#     direction = 1
#   ) +
#   labs(
#     title = "Days > AQI 300",
#     subtitle = "2024; 5km",
#     fill = "Status"
#   ) +
#   theme(legend.position = "right") +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### Create raster of 2024 idw aqi rescaled ####

# Rasterize the vector data onto the study area raster
air_quality_aqi_300_2024 <- terra::rasterize(aqi_interp_5070_cropped, 
                                             study_area_90m_5070, 
                                             field = "aqi", 
                                             fun = "mean")

# Plot the final raster
plot(air_quality_aqi_300_2024, main = "Status: Days above AQI 300")

# Align indicator with study_area_90m_template raster
air_quality_aqi_300_2024 <- align_raster_to_template(study_area_90m_5070, air_quality_aqi_300_2024, input_type = "continuous")

# Save to aurora
writeRaster(air_quality_aqi_300_2024, 
            filename = file.path(final_layers_file_path, "indicators/air_quality_status_aqi_300.tif"),
            overwrite = TRUE)
