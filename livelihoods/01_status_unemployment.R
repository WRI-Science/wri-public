wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

## Unemployment
# The US reports unemployment as the number of unemployed persons and the labor force within a census tract. 
# The unemployed persons can be divided by the labor force number to get the unemployment rate at the census tract level. 
# US data is retrieved using the Census API. 
# Canada reports its unemployment as it's own statistic at the census subdivision level. Values are rescaled between 
# The indicator’s maximum value (1) at 4% unemployment and the minimum value (0) at 10%, rescaling values linearly between these two thresholds. 
# Rescale the US and Canada separately 

# Download Canadian Employment Data: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810048501

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
canadian_employment_path <- read_csv(file.path(raw_data_file_path, "canadian_labor_statistics/98100485.csv"))

#### Canada Employment #### 

# Load and process Canadian Employment Data
canada_unemployment_data <- canadian_employment_path %>%
  rename(unemployment_rate = `Gender (3):Total - Gender[1]`) %>%
  filter(`Labour force status (8)` == "Unemployment rate", `Age (15A)` == "Total - Age") %>%
  dplyr::select(DGUID, unemployment_rate)

# Calculate unemployment data for canadian divisions to infill into missing subdivisions
canada_unemployment_divisions <- canada_unemployment_data %>%
  filter(nchar(DGUID) == 13) %>%
  mutate(divisions_DGUID = substr(DGUID, nchar(DGUID)-3, nchar(DGUID))) %>%
  dplyr::select(divisions_DGUID, unemployment_rate_div = unemployment_rate)

# Calculate unemployment data for canadian subdivisions
canada_unemployment_subdivisions <- canada_unemployment_data %>%
  filter(nchar(DGUID) == 16) %>%
  mutate(divisions_DGUID = substr(DGUID, 10, 13)) %>%
  left_join(canada_unemployment_divisions, by = "divisions_DGUID") %>%
  mutate(unemployment_rate = ifelse(is.na(unemployment_rate), unemployment_rate_div, unemployment_rate)) %>%
  dplyr::select(DGUID, unemployment_rate) %>%
  mutate(
    unemployment_linear_rescaled = case_when( # linear rescale the unemployment rate between 4% and 10%
      unemployment_rate <= 4 ~ 1,
      unemployment_rate >= 10 ~ 0,
      TRUE ~ (10 - unemployment_rate) / (10 - 4)
    )
  )

# Join Canada subdivision unemployment data to Canadian subdivision shapefiles
canada_unemployment_shape <- left_join(canada_census_subdivisions, canada_unemployment_subdivisions, by = "DGUID")

# Plotting canadian unemployment
ggplot(canada_unemployment_shape) +
  geom_sf(aes(fill = unemployment_linear_rescaled), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: Canada unemployment", fill = "unemployment status") +
  theme_minimal()

#### US Employment #### 
variables <- c(
  unemployment_rate = "S2301_C04_001")

# Function to fetch data for a single state
fetch_data_for_tract <- function(state) {
  get_acs(
    geography = "tract",
    variables = variables,
    year = 2023,
    survey = "acs5",  # Use the 5-year ACS survey
    output = "wide",
    state = state
  )
}

# Fetch data for each state and combine results
census_data_list <- purrr::map(west_states, fetch_data_for_tract)
census_data_us <- bind_rows(census_data_list)

# Linear rescale the unemployment rate between 4% and 10%
census_unemployment_rescaled <- census_data_us %>% 
  rename(unemployment_rate = unemployment_rateE) %>% 
  mutate(
    unemployment_linear_rescaled = case_when(
      unemployment_rate <= 4   ~ 1,
      unemployment_rate >= 10  ~ 0,
      TRUE                     ~ (10 - unemployment_rate) / (10 - 4)
    )
  )

# Join US tract unemployment data to US tract shapefiles
us_unemployment_shape <- left_join(us_tract, census_unemployment_rescaled, by = "GEOID")

# Plotting US unemployment
ggplot(us_unemployment_shape) +
  geom_sf(aes(fill = unemployment_linear_rescaled), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: US unemployment", fill = "unemployment status") +
  theme_minimal()

#### Merge US and Canada shapefiles ####

us_can_unemployment <- bind_rows(canada_unemployment_shape, us_unemployment_shape)

# Plot 
ggplot(data = us_can_unemployment) +
  geom_sf(aes(fill = unemployment_linear_rescaled), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) + 
  labs(title = "Livelihoods: Status: Unemployment",
       fill = "Status",
       caption = "Source: Cancensus 2021 County Subdivisions | US ACS 2019-2023 Tract Level") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### Rasterize Unemployment ####

# Rasterize the vector data onto the study area raster
livelihoods_status_unemployment <- terra::rasterize(us_can_unemployment, 
                                                         study_area_90m_5070, 
                                                         field = "unemployment_linear_rescaled", 
                                                         fun = "mean")

# Align indicator with study_area_90m_template raster
livelihoods_status_unemployment <- align_raster_to_template(study_area_90m_5070, livelihoods_status_unemployment, input_type = "continuous")

# mask to human settlement layer 
livelihoods_status_unemployment <- mask(livelihoods_status_unemployment, human_settlement_layer)

# Write the updated raster to a file
writeRaster(livelihoods_status_unemployment, 
            file.path(final_layers_file_path, "indicators/livelihoods_status_unemployment.tif"), 
            overwrite = TRUE)

plot(livelihoods_status_unemployment, main = "Livelihoods: Status: Unemployment")


