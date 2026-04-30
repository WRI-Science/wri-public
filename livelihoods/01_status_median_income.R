## Median Income
# The US reports median income as USD at the household level for each census tract.
# Canada reports median income as CAD at the household level for each census subdivision.
# Download Canadian Income Data: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810007001

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
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/livelihoods/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/livelihoods/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/livelihoods"

#### Boundary layers ####
canada_census_subdivisions <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/canada_census_subdivisions/canada_census_subdivisions.shp"))  %>% 
  st_transform(5070) 
us_tract <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/us_census_tract/us_census_tracts.shp")) %>% 
  st_transform(5070) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

human_settlement_layer <- rast("/home/shares/wwri-wildfire/data/multi_domain_data/int/global_human_settlement_layer/human_sett_aligned.tif")

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
west_states <- c("NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK")
canadian_income_path <- read_csv(file.path(multi_domain_data_file_path, "canadian_income_statistics/98100070.csv"))

#### Canada Median Income #### 

# Read and preprocess Canadian Income Data
canada_income_data <- canadian_income_path %>%
  rename(income = `Income statistics (8):Median amount ($)[3]`) %>%
  filter(`Income sources and taxes (32)` == "Total income") %>%
  dplyr::select(DGUID, income)

# Calculate median income data for canadian divisions to infill into missing subdivisions
canada_income_divisions <- canada_income_data %>%
  filter(nchar(DGUID) == 13) %>%
  mutate(divisions_DGUID = substr(DGUID, nchar(DGUID)-3, nchar(DGUID))) %>%
  dplyr::select(divisions_DGUID, income_div = income)

# Calculate median income data for canadian subdivisions
canada_income_subdivisions <- canada_income_data %>%
  filter(nchar(DGUID) == 16) %>%
  mutate(divisions_DGUID = substr(DGUID, 10, 13)) %>%
  left_join(canada_income_divisions, by = "divisions_DGUID") %>%
  mutate(income = ifelse(is.na(income), income_div, income)) %>%
  dplyr::select(DGUID, income_raw = income) %>%
  mutate(
    income_rescaled = scales::rescale(income_raw, to = c(0, 1))
  )

# Join Canada subdivision median income data to Canadian subdivision shapefiles
canada_median_income_shape <- left_join(canada_census_subdivisions, canada_income_subdivisions, by = "DGUID")

# Plotting canadian unemployment
ggplot(canada_median_income_shape) +
  geom_sf(aes(fill = income_rescaled), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: Canada median income", fill = "median income status") +
  theme_minimal()

#### US Median Income #### 

variables <- c(
  median_income = "B19013_001E", # E = estimate, M = margin of error
  population = "S0101_C01_001E") # E = estimate, M = margin of error

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

# identify NA tracts - 267 NA tracts
na_summary <- census_data_us %>%
  filter(is.na(median_income)) 

#### US county gapfilling ####

# Function to fetch data for a single state
fetch_data_for_county <- function(state) {
  get_acs(
    geography = "county",
    variables = variables,
    year = 2023,
    survey = "acs5",  # Use the 5-year ACS survey
    output = "wide",
    state = state
  )
}

# Fetch data for each state and combine results
census_data_list_county <- purrr::map(west_states, fetch_data_for_county)
census_data_us_county <- bind_rows(census_data_list_county)

# set up which columns to gapfill
cols_to_fill <- c("median_income") # can add more if desired

# if tracts is NA, take value from county level (ONLY if pop > 0 --  cases where pop = 0)
# geoid length 5 = county
# geoid length 11 = tract
census_data_us_tract_gapfilled <- 
  census_data_us %>% 
  # 1) Save the full tract ID
  rename(tract_GEOID = GEOID) %>% 
  # 2) Derive a county ID just for the join
  mutate(county_GEOID = substr(tract_GEOID, 1, 5)) %>% 
  # 3) Bring in county medians (suffixing them _gf)
  left_join(
    census_data_us_county %>% rename(county_GEOID = GEOID),
    by     = "county_GEOID",
    suffix = c("", "_gf")
  ) %>% 
  # 4) Fill tract NAs from county
  mutate(across(
    all_of(cols_to_fill),
    ~ ifelse(is.na(.), get(paste0(cur_column(), "_gf")), .)
  )) %>% 
  # 5) Clean up helper columns, but keep tract_GEOID
  dplyr::select(-ends_with("_gf"), -county_GEOID) %>% 
  # 6) (Optionally) put your tract_GEOID back into the standard name
  rename(GEOID = tract_GEOID)

# check NA tracts again- 122 NA tracts
na_summary_check <- census_data_us_tract_gapfilled %>%
  filter(is.na(median_income)) 

# Linear rescale the median income between 0 and 1
us_median_income_rescaled <- census_data_us_tract_gapfilled %>%
  mutate(
    income_rescaled = rescale(median_income, to = c(0, 1))
  )

# Join US tract median income data to US tract shapefiles
us_median_income_shape <- left_join(us_tract, us_median_income_rescaled, by = "GEOID")

# Plotting US unemployment
ggplot(us_median_income_shape) +
  geom_sf(aes(fill = income_rescaled), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: US median income", fill = "status") +
  theme_minimal()

#### Merge US and Canada shapefiles ####

us_can_median_income <- bind_rows(canada_median_income_shape, us_median_income_shape)

# Plot 
ggplot(data = us_can_median_income) +
  geom_sf(aes(fill = income_rescaled), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) + 
  labs(title = "Livelihoods: Status: Median Income",
       fill = "Status",
       caption = "Source: Cancensus 2021 County Subdivisions | US ACS 2019-2023 Tract Level") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### Rasterize Median Income ####

# Rasterize the vector data onto the study area raster
livelihoods_status_median_income <- terra::rasterize(us_can_median_income, 
                                                    study_area_90m_5070, 
                                                    field = "income_rescaled", 
                                                    fun = "mean")

# Align indicator with study_area_90m_template raster
livelihoods_status_median_income <- align_raster_to_template(study_area_90m_5070, livelihoods_status_median_income, input_type = "continuous")

# Mask to human settlement layer 
livelihoods_status_median_income <- mask(livelihoods_status_median_income, human_settlement_layer)

# Write the updated raster to a file
writeRaster(livelihoods_status_median_income, 
            file.path(final_layers_file_path, "indicators/livelihoods_status_median_income.tif"), 
            overwrite = TRUE)

plot(livelihoods_status_median_income, main = "Livelihoods: Status: Median Income")


