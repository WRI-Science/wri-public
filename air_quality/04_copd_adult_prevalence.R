# script for mapping the prevalence (%) of chronic obstructive pulmonary disease (COPD) 
# US PLACES data from https://www.cdc.gov/places/index.html
# Canada 2019/2020 Two year Health Estimate Data from CanStats
library(stringr)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(ggplot2)
library(viridis)
library(scales)
library(raster)
library(tidyr)
library(tibble)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/air_quality"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/air_quality"

#### Boundary layers ####
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))
us_tract <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/us_census_tracts.shp")) %>% 
  st_transform(5070) 
bc_yt_health_service_delivery_area_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/bc_health_areas/wwri_yt_bc_health_service_delivery_area.shp")) %>% 
  st_transform(5070)

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
us_health_data_csv <- read_csv(file.path(raw_data_file_path,"PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2024_release_20250529.csv"))
bc_yt_hsda_copd <- read_csv(file.path(raw_data_file_path, "13100113-eng_chronic_conditions_canada_full_2015_19/1310011301_databaseLoadingData_Canada_COPD_2019_2020.csv"))

#### US CDC COPD ####

# Filter for COPD and remove point geometry column 
us_copd_csv <- us_health_data_csv %>%
  dplyr::select(-Geolocation) %>% 
  filter(MeasureId == "COPD") 

# Join asthma tract data to US tract shapefiles
us_copd_tract_shape <- left_join(
  us_tract,
  us_copd_csv,
  by = c("geoid" = "LocationName")
)

#### Canada COPD ####

# filter dataset for % COPD value for Total population
bc_yt_hsda_copd <- bc_yt_hsda_copd %>% 
  filter(Characteristics == "Percent") %>% 
  filter(`Age group` == "Total, 12 years and over") %>% 
  mutate(GEO = str_remove(GEO, " Health Service Delivery Area, British Columbia"))

# Rename hsda_names that do not match the hsda_names in the shapefile
bc_yt_hsda_copd_renamed <- bc_yt_hsda_copd %>%
  mutate(GEO = ifelse(GEO == "Kootenay-Boundary", "Kootenay Boundary",
                      ifelse(GEO == "Thompson/Cariboo", "Thompson Cariboo Shuswap",
                             GEO)))  # Keep the original value if no conditions are met

# Join astma tract data to US tract shapefiles
canada_copd_hsda_shape <- left_join(
  bc_yt_health_service_delivery_area_5070,
  bc_yt_hsda_copd_renamed,
  by = c("hsda_name" = "GEO"),
  relationship = "many-to-many"
)

#### Join US and Canada asthma data ####

us_can_copd_percent <- bind_rows(us_copd_tract_shape, canada_copd_hsda_shape) %>%
  mutate(
    perc_copd = coalesce(Data_Value, VALUE)
  ) 

# Plot with grey color for zero and NA values
ggplot(data = us_can_copd_percent) +
  geom_sf(aes(fill = perc_copd), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "COPD US > 18, CAN > 35",
       fill = "% Prevalence",
  ) +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) 


#### Rescale the values between 0-1 ####

us_can_copd_rescaled <- us_can_copd_percent %>%
  mutate(percent_copd_rescaled = scales::rescale(perc_copd, to = c(1, 0)))

# plot rescaled values 
ggplot(data = us_can_copd_rescaled) +
  geom_sf(aes(fill = percent_copd_rescaled)) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "COPD% US > 18, CAN > 35",
       fill = "Resistance") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### create raster ####

# Rasterize the vector data onto the study area raster
copd_prevalence_rescaled_90m_5070 <- terra::rasterize(
  us_can_copd_rescaled, 
  study_area_90m_5070, 
  field = "percent_copd_rescaled", 
  fun = "mean", 
  na.rm = T)

# Plot the final raster
plot(copd_prevalence_rescaled_90m_5070, main = "Air Quality: Resistance: COPD (EPSG:5070, 90m)")

# Align indicator with study_area_90m_template raster
air_quality_resistance_copd <- align_raster_to_template(study_area_90m_5070, copd_prevalence_rescaled_90m_5070)

# Save to aurora
writeRaster(air_quality_resistance_copd, 
            filename = file.path(final_layers_file_path, "indicators/air_quality_resistance_copd.tif"),
            overwrite = TRUE)




