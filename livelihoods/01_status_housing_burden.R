wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

## Housing Burden
# The US reports their housing statistics by rent burdened housing units burden for 
# each housing tenure (mortgage status) this is then combined and divided by total 
# number of housing to get percentage of housing units that are rent burdened.
# Canada reports their housing statistics by the percentage of households that spend
# 30% or more of their income on housing costs.

# Load the required libraries
library(sf) 
library(tidyverse) 
library(here) 
library(ggplot2) 
library(tidycensus) 
library(purrr) 
library(scales)
library(viridis)
library(terra)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
raw_data_file_path <- file.path(wri_project_root, "data", "livelihoods", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "livelihoods", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "livelihoods")

#### Boundary layers ####
canada_census_subdivisions <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/canada_census_subdivisions/canada_census_subdivisions.shp"))  %>% 
  st_transform(5070) 
us_tract <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/us_census_tract/us_census_tracts.shp")) %>% 
  st_transform(5070) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

human_settlement_layer <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "human_settlement", "human_sett_aligned.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
west_states <- c("NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK")
canadian_housing_path <- read_csv(file.path(multi_domain_data_file_path, "canada_housing_burden/98100243.csv"))

#### Canada Housing Burden #### 

# Read the Canadian housing data from a CSV file
canada_housing_raw <- canadian_housing_path %>%
  mutate(prov = substr(DGUID, 10, 11)) %>%
  filter(prov %in% c("59", "60"))

# Process the raw Canadian housing data
canada_housing <- canada_housing_raw %>%
  filter(`Age of primary household maintainer (9)` == "Total - Age of primary household maintainer",
         `Household type including census family structure (9)` == "Total - Household type including family structure", 
         `Statistics (3C)` == "Number of private households",
         `Housing indicators (6)` %in% c("Total - Housing indicators",
                                         "Affordability: 30% or more of household income is spent on shelter costs")) %>%
  dplyr::select(geo_id = DGUID, metric ="Housing indicators (6)", number = "Tenure including presence of mortgage payments and subsidized housing (8):Total - Tenure including presence of mortgage payments and subsidized housing[1]") %>%
  pivot_wider(names_from = metric, values_from = number) %>%
  group_by(geo_id) %>%
  summarize(total_units = sum(`Total - Housing indicators`, na.rm = TRUE),
            burdened =    sum(`Affordability: 30% or more of household income is spent on shelter costs`, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(pct_burdened = (burdened/total_units) * 100) %>%
  dplyr::select(geo_id, pct_burdened)

# Filter the Canadian housing data by census divisions
canada_housing_divisions <- canada_housing %>%
  filter(nchar(geo_id) == 13) %>%
  mutate(housing_burden_rescaled = scales::rescale(pct_burdened, to = c(1, 0))) %>%
  mutate(divisions_geo_id = substr(geo_id, 10, 13)) %>%
  dplyr::select(-geo_id)

# Join Canada subdivision housing burden data to Canadian subdivision shapefiles
canada_housing_burden_shape <- 
  canada_census_subdivisions %>% 
  # 1. Extract the 4-digit division code from the DGUID
  mutate(divisions_geo_id = substr(DGUID, 10, 13)) %>%  
  # 2. Join in only the burdened score
  left_join(
    canada_housing_divisions %>% select(divisions_geo_id, housing_burden_rescaled),
    by = "divisions_geo_id")

# Plotting canadian unemployment
ggplot(canada_housing_burden_shape) +
  geom_sf(aes(fill = housing_burden_rescaled), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: Canada housing burden", fill = "status") +
  theme_minimal()

#### US Housing #### 

us_housing_raw <- map_df(west_states, ~get_acs(geography = "tract",
                                               variables = c("B25140_003E", # Owner-occupied housing units with a mortgage where monthly housing costs are 30% or more of household income
                                                             "B25140_007E", # Owner-occupied housing units without a mortgage where monthly housing costs are 30% or more of household income
                                                             "B25140_011E", # Renter-occupied housing units where gross rent is 30% or more of household income
                                                             "B25140_001E"), # Total occupied housing units
                                               state = .x,
                                               geometry = TRUE,
                                               year = 2023,
                                               survey = "acs5"))

# Process the raw US housing data
us_housing_processed <- us_housing_raw %>%
  dplyr::select(-NAME, -moe) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  group_by(GEOID) %>%
  summarize(total_units =          sum(B25140_001, na.rm = TRUE),
            mortgage_burdened =    sum(B25140_003, na.rm = TRUE),
            no_mortgage_burdened = sum(B25140_007, na.rm = TRUE),
            renter_burdened =      sum(B25140_011, na.rm = TRUE),
            .groups = 'drop')

# Calculate the percentage of housing cost-burdened households and scale the values
us_housing <- us_housing_processed %>%
  mutate(pct_burdened = ((mortgage_burdened +
                            no_mortgage_burdened +
                            renter_burdened) / total_units) * 100,
         housing_burden_rescaled = scales::rescale(pct_burdened, to = c(1, 0))) %>%
  dplyr::select(GEOID, pct_burdened, housing_burden_rescaled)

# Join US tract median income data to US tract shapefiles
us_housing_burden_shape <- left_join(us_tract, us_housing, by = "GEOID")

# Plotting US unemployment
ggplot(us_housing_burden_shape) +
  geom_sf(aes(fill = housing_burden_rescaled), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: US median income", fill = "status") +
  theme_minimal()

#### Merge US and Canada shapefiles ####

us_can_housing_burden <- bind_rows(canada_housing_burden_shape, us_housing_burden_shape)

# Plot 
ggplot(data = us_can_housing_burden) +
  geom_sf(aes(fill = housing_burden_rescaled), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) + 
  labs(title = "Livelihoods: Status: Housing Burden",
       fill = "Status",
       caption = "Source: Cancensus 2021 County Subdivisions | US ACS 2019-2023 Tract Level") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### Rasterize Housing Burden ####

# Rasterize the vector data onto the study area raster
livelihoods_status_housing_burden <- terra::rasterize(us_can_housing_burden, 
                                                     study_area_90m_5070, 
                                                     field = "housing_burden_rescaled", 
                                                     fun = "mean")

# Align indicator with study_area_90m_template raster
livelihoods_status_housing_burden <- align_raster_to_template(study_area_90m_5070, livelihoods_status_housing_burden, input_type = "continuous")

# Mask to human settlement layer 
livelihoods_status_housing_burden <- mask(livelihoods_status_housing_burden, human_settlement_layer)

# Write the updated raster to a file
writeRaster(livelihoods_status_housing_burden, 
            file.path(final_layers_file_path, "indicators/livelihoods_status_housing_burden.tif"), 
            overwrite = TRUE)

plot(livelihoods_status_housing_burden, main = "Livelihoods: Status: Housing Burden")

