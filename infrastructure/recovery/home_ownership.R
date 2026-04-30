wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# Load required packages
library(tidycensus)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(purrr)
library(raster)
library(scales)  
library(terra)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "infrastructure")
raw_data_file_path <- file.path(wri_project_root, "data", "infrastructure", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "infrastructure", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "infrastructure")

#### Boundary layers ####
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

us_tract <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/us_census_tract/us_census_tracts.shp")) %>% 
  st_transform(5070) 
can_subdivisions <- st_read(file.path(multi_domain_data_file_path,"int/boundary_layers/canada_census_subdivisions/canada_census_subdivisions.shp")) %>% 
  st_transform(5070) 

#### Data Layers ####
canadian_housing_path <- read_csv(file.path(multi_domain_data_file_path, "canada_housing_burden/98100243.csv"))
                                  
#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))
west_states <- c("AK", "CA", "OR", "WA", "NV", "AZ", "NM", "CO", "UT", "ID", "MT", "WY")

#### Canadian Housing Data ####

# read in canada housing file and select only yukon and BC
canada_owners_raw <- canadian_housing_path %>%
  mutate(prov = substr(DGUID, 10, 11)) %>%
  filter(prov %in% c("59", "60"))

# these are what we are interested in:
# [11] "Tenure including presence of mortgage payments and subsidized housing (8):Owner[2]"
# [17] "Tenure including presence of mortgage payments and subsidized housing (8):Renter[5]"

# get the variables with are interested in and calculate relative proportions of renters vs. owners
# we grab census subdivisions here because that is the finest resolution available -- this does not perfectly match US tracts, but is the best equivalent in canada
canada_owners <- canada_owners_raw %>%
  filter(`Age of primary household maintainer (9)` == "Total - Age of primary household maintainer",
         `Household type including census family structure (9)` == "Total - Household type including family structure", 
         `Statistics (3C)` == "Number of private households",
         `Housing indicators (6)` == "Total - Housing indicators") %>%
  dplyr::select(prov, DGUID, name = GEO, metric = "Housing indicators (6)", owning_households = "Tenure including presence of mortgage payments and subsidized housing (8):Owner[2]", 
                renting_households = "Tenure including presence of mortgage payments and subsidized housing (8):Renter[5]") %>%
  group_by(DGUID, name, prov) %>%
  summarize(owners_count = sum(owning_households),  # na.rm = TRUE
            renters_count = sum(renting_households), # na.rm = TRUE
            total = owners_count + renters_count,
            owners = owners_count/total,
            .groups = 'drop') %>%
  dplyr::select(DGUID, owners) # name, prov, 

# join Canada vulnerable populations df to Canadian study area CSD shapefile
canada_owners_subdivisions_shape <- left_join(can_subdivisions, canada_owners, by = c("DGUID"))

# # get census division values to infill missing census sub-division values 
# canada_owners_census_divisions <- canada_owners %>%
#   filter(nchar(DGUID) == 13)
# # for these columns, if subdivision is NA, take value from division level
# canada_owners_subdivisions_shape_gapfilled <- canada_owners_subdivisions_shape %>%
#   left_join(
#     canada_owners_census_divisions %>%
#       dplyr::select(DGUID, owners_div = owners),
#     by = "DGUID"
#   ) %>%
#   mutate(
#     owners = coalesce(owners, owners_div)
#   ) %>%
#   dplyr::select(-owners_div)


#### US 5 year ACS Tract Census Homeowner Data ####

variables <- c(
  "S2501_C01_001", #estimate_total_occupied_housing_units
  "S2501_C03_001" #estimate_owner_occupied_housing_units
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
    owners = (S2501_C03_001E/S2501_C01_001E))

# join study area shapefile to census data
us_tract_census_shape <- left_join(us_tract, census_data_us, by = c("GEOID"))

#### Merge US and Canada shapefiles ####

us_can_homeowners <- bind_rows(canada_owners_subdivisions_shape, us_tract_census_shape)

#### Rescale the values between 0-1 ####

us_can_homeowners_rescaled <- us_can_homeowners %>%
  mutate(owners_rescaled = scales::rescale(owners, to = c(0, 1)))

# plot rescaled values 
ggplot(data = us_can_homeowners_rescaled) +
  geom_sf(aes(fill = owners_rescaled)) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Infrastructure: Homeownership",
       fill = "Recovery") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))


#### create raster ####

# Rasterize the vector data onto the study area raster
us_can_homeowners_rescaled_90m_5070 <- terra::rasterize(
  us_can_homeowners_rescaled, 
  study_area_90m_5070, 
  field = "owners_rescaled", 
  fun = "mean", 
  na.rm = T)

# Plot the final raster
plot(us_can_homeowners_rescaled_90m_5070, main = "Infrastructure: Recovery: Homeowners")

# Align indicator with study_area_90m_template raster
infrastructure_recovery_homeowners <- align_raster_to_template(study_area_90m_5070, us_can_homeowners_rescaled_90m_5070, input_type = "continuous")

# Save to aurora
writeRaster(infrastructure_recovery_homeowners, 
            filename = file.path(final_layers_file_path, "indicators/infrastructure_recovery_homeowners.tif"),
            overwrite = TRUE)




