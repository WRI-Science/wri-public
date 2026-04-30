# Load required packages
library(cancensus)
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(viridis)
library(purrr)
library(raster)
library(scales)  
library(terra)

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
can_subdivisions <- st_read(file.path(multi_domain_data_file_path,"int/boundary_layers/canada_census_subdivisions/canada_census_subdivisions.shp")) %>% 
  st_transform(5070) 

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

# Define the list of western states
west_states <- c("AK", "CA", "OR", "WA", "NV", "AZ", "NM", "CO", "UT", "ID", "MT", "WY")

#### Canadian Census 2021 Subdivision Data ####

# follow instructions here to get key: https://mountainmath.github.io/cancensus/
#set_cancensus_cache_path("/home/farnisa/cache_cancensus", install = TRUE)
# 
# View(list_census_datasets()) # view available datasets on cancensus
# View(list_census_vectors("CA21")) # To view available Census variables for the 2021 Census
# find_census_vectors('work', dataset = 'CA21', type = 'total', query_type = 'keyword', interactive = T)

# 59 and 60 are BC and YT regions
regions_of_interest <- c("59", "60")

# Total - 0-14, Total - 15-19, Total - 65+
variables_of_interest <- c("v_CA21_11", "v_CA21_71", "v_CA21_251")

# Return an sf-class data frame
census_data_canada <- get_census(dataset='CA21', regions=list(PR = regions_of_interest), 
                          vectors=variables_of_interest, level='CSD', geo_format = "sf")

# filter data for regions and variables of interest
census_data_filtered <- get_census(
  dataset = 'CA21', 
  regions = list(PR = regions_of_interest),
  vectors = variables_of_interest, 
  level = 'CSD', 
  geo_format = "sf"
) %>%
  dplyr::select(
    population_2021 = Population, 
    CSDUID = GeoUID, 
    population_0_14 = 'v_CA21_11: 0 to 14 years', 
    population_15_19 = 'v_CA21_71: 15 to 19 years', 
    population_65_above = 'v_CA21_251: 65 years and over'
  ) %>% 
  mutate(
    vulnerable_populations = rowSums(across(c(population_0_14, population_15_19, population_65_above))),
    total_population = population_2021,
    percent_vulnerable = (vulnerable_populations / total_population) * 100
  ) %>%
  st_drop_geometry()

# join Canada vulnerable populations df to Canadian study area shapefile
canada_subdivisions_census_shape <- left_join(can_subdivisions, census_data_filtered, by = c("CSDUID"))

#### US 5 year ACS Tract Census Data ####

variables <- c(
  elderly_population = "S0101_C02_030",
  children_population = "S0101_C02_022"
)

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

# write out this csv for storing due to govt 
#write.csv(census_data_us, file.path(intermediate_data_file_path, "vulnerable_populations/us_acs5_census_data_vulnerable_populations_2023.csv"), row.names = F)

# Create new columns for vulnerable populations and their percentage
census_data_us <- census_data_us %>%
  dplyr::mutate(
    percent_vulnerable = elderly_populationE + children_populationE)

# join study area shapefile to census data
us_tract_census_shape <- left_join(us_tract, census_data_us, by = c("GEOID"))

#### Merge US and Canada shapefiles ####

us_can_vulnerable_populations <- bind_rows(canada_subdivisions_census_shape, us_tract_census_shape)

# Plot 
ggplot(data = us_can_vulnerable_populations) +
  geom_sf(aes(fill = percent_vulnerable), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) + 
  labs(title = "Vulnerable Populations: >=65 & <=18",
       fill = "%",
       caption = "Source: Cancensus 2021 County Subdivisions | US ACS 2019-2023 Tract Level") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### Rescale the values between 0-1 ####

us_can_vulnerable_populations_rescaled <- us_can_vulnerable_populations %>%
  mutate(percent_vulnerable_rescaled = scales::rescale(percent_vulnerable, to = c(1, 0)))

# plot rescaled values 
ggplot(data = us_can_vulnerable_populations_rescaled) +
  geom_sf(aes(fill = percent_vulnerable_rescaled)) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Vulnerable Populations: >=65 & <=18",
       fill = "Resistance") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))


#### Create raster ####

# Rasterize the vector data onto the study area raster
vulnerable_populations_rescaled_90m_5070 <- terra::rasterize(
  us_can_vulnerable_populations_rescaled, 
  study_area_90m_5070, 
  field = "percent_vulnerable_rescaled", 
  fun = "mean", 
  na.rm = T)

# Plot the final raster
plot(vulnerable_populations_rescaled_90m_5070, main = "Air Quality: Resistance: Vulnerable Populations (EPSG:5070, 90m)")

# Align indicator with study_area_90m_template raster
air_quality_resistance_vulnerable_populations <- align_raster_to_template(study_area_90m_5070, vulnerable_populations_rescaled_90m_5070)

# Save to aurora
writeRaster(air_quality_resistance_vulnerable_populations, 
            filename = file.path(final_layers_file_path, "indicators/air_quality_resistance_vulnerable_populations.tif"),
            overwrite = TRUE)



