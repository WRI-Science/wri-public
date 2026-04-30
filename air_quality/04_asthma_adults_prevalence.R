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
us_tract <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/us_census_tract/us_census_tracts.shp")) %>% 
  st_transform(5070) 
bc_yt_health_service_delivery_area_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/bc_health_areas/wwri_yt_bc_health_service_delivery_area.shp")) %>% 
  st_transform(5070)

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
us_health_data_csv <- read_csv(file.path(raw_data_file_path,"PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2024_release_20250529.csv"))
bc_yt_hsda_asthma <- read.csv(file.path(raw_data_file_path,"13100113-eng_chronic_conditions_canada_full_2015_19/1310011301_databaseLoadingData_bc_yt_asthma_adults_2016_2020.csv"))

#### US CDC Asthma ####

# Filter for Asthma and remove point geometry column 
us_asthma_csv <- us_health_data_csv %>%
  dplyr::select(-Geolocation) %>% 
  filter(MeasureId == "CASTHMA") 

# Join astma tract data to US tract shapefiles
us_asthma_tract_shape <- left_join(
  us_tract,
  us_asthma_csv,
  by = c("GEOID" = "LocationID")
)

#### Canada Asthma ####

# Filter for 2019/2020 year 
bc_yt_hsda_asthma <- bc_yt_hsda_asthma %>%
  filter(REF_DATE == "2019/2020") %>% 
  filter(Characteristics == "Percent") %>% 
  filter(Age.group == c("Total, 12 years and over")) %>% # %'s for 18 and over are population calculated. so you can't add them and many NA's
  mutate(GEO = str_remove(GEO, " Health Service Delivery Area, British Columbia"))

# Rename hsda_names that do not match the hsda_names in the shapefile
bc_yt_hsda_asthma_renamed <- bc_yt_hsda_asthma %>%
  mutate(GEO = ifelse(GEO == "Kootenay-Boundary", "Kootenay Boundary",
                            ifelse(GEO == "Thompson/Cariboo", "Thompson Cariboo Shuswap",
                                   GEO)))  # Keep the original value if no conditions are met

# Join astma tract data to US tract shapefiles
canada_asthma_hsda_shape <- left_join(
  bc_yt_health_service_delivery_area_5070,
  bc_yt_hsda_asthma_renamed,
  by = c("hsda_name" = "GEO"),
  relationship = "many-to-many"
)


#### Join US and Canada asthma data ####

us_can_asthma_percent <- bind_rows(us_asthma_tract_shape, canada_asthma_hsda_shape) %>%
  mutate(
    perc_asthma = coalesce(Data_Value, VALUE)
  ) %>%
  select(-Data_Value, -VALUE)


# Plot with grey color for zero and NA values
ggplot(data = us_can_asthma_percent) +
  geom_sf(aes(fill = perc_asthma), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Asthma US > 18, CAN > 12",
       fill = "% Prevalence",
  ) +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) 


#### Rescale the values between 0-1 ####

us_can_asthma_rescaled <- us_can_asthma_percent %>%
  mutate(percent_asthma_rescaled = scales::rescale(perc_asthma, to = c(1, 0)))

# plot rescaled values 
ggplot(data = us_can_asthma_rescaled) +
  geom_sf(aes(fill = percent_asthma_rescaled)) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Asthma% US > 18, CAN > 12",
       fill = "Resistance") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### create raster ####

# Rasterize the vector data onto the study area raster
asthma_prevalence_rescaled_90m_5070 <- terra::rasterize(
  us_can_asthma_rescaled, 
  study_area_90m_5070, 
  field = "percent_asthma_rescaled", 
  fun = "mean", 
  na.rm = T)

# Plot the final raster
plot(asthma_prevalence_rescaled_90m_5070, main = "Air Quality: Resistance: Asthma (EPSG:5070, 90m)")

# Align indicator with study_area_90m_template raster
air_quality_resistance_asthma <- align_raster_to_template(study_area_90m_5070, asthma_prevalence_rescaled_90m_5070)

# Save to aurora
writeRaster(air_quality_resistance_asthma, 
            filename = file.path(final_layers_file_path, "indicators/air_quality_resistance_asthma.tif"),
            overwrite = TRUE)



